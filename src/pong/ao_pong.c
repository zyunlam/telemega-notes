/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "ao.h"
#include <ao_exti.h>
#include <ao_event.h>
#include <ao_button.h>
#include <ao_boot.h>
#include <ao_vga.h>
#include "ao_pong_text.h"

// uint8_t ao_sensor_errors;

#define BALL_SPEED	3

#define WIN_SCORE	11

#define BALL_WIDTH	5
#define BALL_HEIGHT	5

#define PADDLE_WIDTH	5
#define PADDLE_HEIGHT	25
#define PADDLE_OFFSET	20

struct point {
	int	x, y;
};

struct rect {
	int	x, y, w, h;
};

static struct ao_data	ao_data;

static int		player_value[2];
static int		player_score[2];

#define player_filter(old, new)	(((old) * 7 + (new)) >> 3)

static struct rect	player[2];

static struct rect	ball;

static int	ball_e;
static int	ball_dx, ball_dy;
static int	ball_step_x, ball_step_y;

static int
intersect(struct rect *a, struct rect *b)
{
	return (a->x <= b->x + b->w && b->x <= a->x + a->w &&
		a->y <= b->y + b->h && b->y <= a->y + a->h);
}

static int
ao_ball_step(void)
{
	int	p;

	/* Move the ball */
	ball_e += ball_dy;
	ball.x += ball_step_x;
	while (ball_e >= ball_dx) {
		ball_e -= ball_dx;
		ball.y += ball_step_y;
	}

	/* player missed */
	if (ball.x <= 0)
		return 1;

	if (ball.x >= AO_VGA_WIDTH - BALL_WIDTH)
		return -1;

	/* bounce off walls */
	if (ball.y < 0 || ball.y + ball.h > AO_VGA_HEIGHT) {
		ball_step_y = -ball_step_y;
		ball.y += ball_step_y;
	}

	/* bounce off paddles */

	for (p = 0; p < 2; p++) {
		if (intersect(&ball, &player[p])) {
			int	dy = 2 * (ball.y - player[p].y) + ball.h - player[p].h;
			int	dx = 20;

			ball_step_x = -ball_step_x;
			ball.x += ball_step_x;
			ball_step_y = 1;
			if (dy < 0) {
				ball_step_y = -1;
				dy = -dy;
			}
			ball_e = 0;
			ball_dx = dx;
			ball_dy = dy;
		}
	}

	return 0;
}

#define AO_ADC_MAX	4095

static void
ao_paddle_set(int p, int value)
{
	int	pos = value * (AO_VGA_HEIGHT - PADDLE_HEIGHT) / AO_ADC_MAX;

	player[p].y = pos;
}

enum pong_state {
	pong_start,
	pong_serve,
	pong_volley,
	pong_endgame,
};

static enum pong_state pong_state;
static int pong_timer;
static int pong_server;

#define PONG_SERVE_WAIT	90

static enum pong_state
ao_pong_start(void)
{
	int p;

	pong_timer = 1;

	pong_server = ao_time() & 1;

	for (p = 0; p < 2; p++)
		player_score[p] = 0;

	return pong_serve;
}

static enum pong_state
ao_pong_serve(void)
{
	int seed;
	if (--pong_timer > 0)
		return pong_serve;

	seed = ao_time();

	ball.y = (AO_VGA_HEIGHT - BALL_HEIGHT) / 2;

	if (pong_server) {
		ball.x = player[1].x - BALL_WIDTH;
		ball_step_x = -BALL_SPEED;
	} else {
		ball.x = player[0].x + PADDLE_WIDTH;
		ball_step_x = BALL_SPEED;
	}

	ball.w = BALL_WIDTH;
	ball.h = BALL_HEIGHT;

	ball_dx = 100;
	ball_dy = (seed & 7) * 10;
	ball_e = 0;

	seed >>= 3;

	if (seed & 1) {
		ball_step_y = BALL_SPEED;
	} else {
		ball_step_y = -BALL_SPEED;
	}

	return pong_volley;
}

static enum pong_state
ao_pong_volley(void)
{
	int	miss = ao_ball_step();
	int	point;

	if (miss == 0)
		return pong_volley;

	point = (miss + 1) >> 1;
	player_score[point]++;
	if (player_score[point] == WIN_SCORE)
		return pong_endgame;

	pong_server = point;
	pong_timer = PONG_SERVE_WAIT;
	return pong_serve;
}

static void
ao_pong_step(void)
{
	int p;

	/* Paddles are always active */
	for (p = 0; p < 2; p++) {
		player_value[p] = player_filter(player_value[p], ao_data.adc.player[p]);
		ao_paddle_set(p, player_value[p]);
	}
	switch (pong_state) {
	case pong_start:
		pong_state = ao_pong_start();
		break;
	case pong_serve:
		pong_state = ao_pong_serve();
		break;
	case pong_volley:
		pong_state = ao_pong_volley();
		break;
	case pong_endgame:
		break;
	}
}

static void
ao_pong_rect(struct rect *r, uint32_t fill)
{
	ao_rect(&ao_vga_bitmap,
		r->x, r->y, r->w, r->h, fill, AO_COPY);
}

static void
ao_pong_score(int p, int value, uint32_t fill)
{
	int	x = AO_VGA_WIDTH / 2 + (p * 2 - 1) * 50 - 24;
	int	a, b;
	char	c[4];

	if (fill != 0) {
		a = value % 10;
		b = value / 10;
		if (b)
			c[0] = b + '0';
		else
			c[0] = ' ';
		c[1] = a + '0';
		c[2] = '\0';
		ao_pong_text(&ao_vga_bitmap, x, 30, c);
	}
}

#define NET_WIDTH	2
#define NET_HEIGHT	8

static void
ao_pong_net(uint32_t fill)
{
	int n;

	if (fill == 0)
		return;

	for (n = NET_HEIGHT/2; n < AO_VGA_HEIGHT - NET_HEIGHT; n += NET_HEIGHT * 2) {
		ao_rect(&ao_vga_bitmap,
			(AO_VGA_WIDTH - NET_WIDTH) >> 1,
			n,
			NET_WIDTH,
			NET_HEIGHT,
			1, AO_COPY);
	}
}

static void
ao_pong_draw(uint32_t fill)
{
	int p;

	for (p = 0; p < 2; p++) {
		ao_pong_rect(&player[p], fill);
		ao_pong_score(p, player_score[p], fill);
	}
	if (fill == 0 || pong_state == pong_volley)
		ao_pong_rect(&ball, fill);
	ao_pong_net(fill);
}

static void
ao_pong_redraw(void)
{
	ao_pong_draw(0);
	ao_pong_step();
	ao_pong_draw(1);
}

static void
ao_pong_setup(void)
{
	int	p;

	ao_rect(&ao_vga_bitmap,
		0, 0, AO_VGA_WIDTH, AO_VGA_HEIGHT,
		0, AO_COPY);

	ball.w = BALL_WIDTH;
	ball.h = BALL_HEIGHT;

	for (p = 0; p < 2; p++) {
		if (p)
			player[p].x = AO_VGA_WIDTH - PADDLE_OFFSET - PADDLE_WIDTH;
		else
			player[p].x = PADDLE_OFFSET;
		player[p].y = (AO_VGA_HEIGHT - PADDLE_HEIGHT) / 2;
		player[p].w = PADDLE_WIDTH;
		player[p].h = PADDLE_HEIGHT;
	}

	pong_state = pong_endgame;
}

static struct ao_task pong_task;

static void
ao_pong(void)
{
	ao_pong_setup();
	for (;;) {
		ao_vga_vblank = 0;
		while (ao_vga_vblank == 0)
			ao_sleep(&ao_vga_vblank);
		ao_data = ao_data_ring[ao_data_ring_prev(ao_data_head)];
		ao_pong_redraw();
	}
}

static struct ao_task event_task;

static void
ao_event(void) {
	struct ao_event event;

	for (;;) {
		ao_event_get(&event);
		if (event.value == 0)
			pong_state = pong_start;
	}
}

int
main(void)
{
	ao_clock_init();

	ao_task_init();

	ao_led_init(LEDS_AVAILABLE);
	ao_led_on(AO_LED_GREEN);
	ao_led_off(AO_LED_BLUE);
	ao_timer_init();
	ao_dma_init();
	ao_cmd_init();
	ao_spi_init();
	ao_exti_init();
	ao_button_init();
	ao_vga_init();

	ao_timer_set_adc_interval(1);

	ao_adc_init();
	ao_usb_init();

	ao_add_task(&pong_task, ao_pong, "pong");

	ao_add_task(&event_task, ao_event, "event");

	ao_vga_enable(1);

	ao_start_scheduler();
	return 0;
}
