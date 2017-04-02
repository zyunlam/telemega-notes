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

#include <ao.h>
#include <ao_exti.h>
#include <ao_profile.h>
#if HAS_STACK_GUARD
#include <ao_mpu.h>
#endif
#include <ao_ps2.h>
#include <ao_vga.h>
#include <ao_console.h>

struct ao_task ball_task;

#define BALL_WIDTH	5
#define BALL_HEIGHT	5

static int	ball_x;
static int	ball_y;
static int	ball_dx, ball_dy;

uint8_t		ball_enable;

void
ao_ball(void)
{
	ball_dx = 1;
	ball_dy = 1;
	ball_x = 0;
	ball_y = 0;
	for (;;) {
		while (!ball_enable)
			ao_sleep(&ball_enable);
		for (;;) {
			ao_line(&ao_vga_bitmap,
				-100, -100, ball_x*2, ball_y*2,
				1, AO_XOR);
			ao_text(&ao_vga_bitmap,
				ball_x, ball_y - 10,
				"Hello, Bdale!",
				1, AO_XOR);
			ao_rect(&ao_vga_bitmap,
				ball_x, ball_y,
				BALL_WIDTH,
				BALL_HEIGHT,
				1,
				AO_XOR);
			ao_delay(AO_MS_TO_TICKS(10));
			ao_rect(&ao_vga_bitmap,
				ball_x, ball_y,
				BALL_WIDTH,
				BALL_HEIGHT,
				1,
				AO_XOR);
			ao_text(&ao_vga_bitmap,
				ball_x, ball_y - 10,
				"Hello, Bdale!",
				1, AO_XOR);
			ao_line(&ao_vga_bitmap,
				-100, -100, ball_x*2, ball_y*2,
				1, AO_XOR);
			if (!ball_enable)
				break;
			ball_x += ball_dx;
			ball_y += ball_dy;
			if (ball_x + BALL_WIDTH > AO_VGA_WIDTH) {
				ball_x = AO_VGA_WIDTH - BALL_WIDTH;
				ball_dx = -ball_dx;
			}
			if (ball_x < 0) {
				ball_x = -ball_x;
				ball_dx = -ball_dx;
			}
			if (ball_y + BALL_HEIGHT > AO_VGA_HEIGHT) {
				ball_y = AO_VGA_HEIGHT - BALL_HEIGHT;
				ball_dy = -ball_dy;
			}
			if (ball_y < 0) {
				ball_y = -ball_y;
				ball_dy = -ball_dy;
			}
		}
	}
}

static void
ao_fb_init(void)
{
	ao_rect(&ao_vga_bitmap,
		0, 0, AO_VGA_WIDTH, AO_VGA_HEIGHT,
		1, AO_COPY);

	ao_rect(&ao_vga_bitmap,
		10, 10, 10, 10,
		0, AO_COPY);

	ao_rect(&ao_vga_bitmap,
		AO_VGA_WIDTH - 20, 10, 10, 10,
		0, AO_COPY);

	ao_rect(&ao_vga_bitmap,
		10, AO_VGA_HEIGHT - 20, 10, 10,
		0, AO_COPY);

	ao_rect(&ao_vga_bitmap,
		AO_VGA_WIDTH - 20, AO_VGA_HEIGHT - 20, 10, 10,
		0, AO_COPY);

	ao_text(&ao_vga_bitmap,
		20, 100,
		"Hello, Bdale!",
		0, AO_COPY);

	ao_text(&ao_vga_bitmap,
		1, ao_font.ascent,
		"UL",
		0, AO_COPY);

	ao_text(&ao_vga_bitmap,
		1, AO_VGA_HEIGHT - ao_font.descent,
		"BL",
		0, AO_COPY);
}

static void
ao_video_toggle(void)
{
	ao_cmd_decimal();
	if (ao_cmd_lex_i)
		ao_fb_init();
	ao_vga_enable(ao_cmd_lex_i);
}

static void
ao_ball_toggle(void)
{
	ao_cmd_decimal();
	ball_enable = ao_cmd_lex_i;
	ao_wakeup(&ball_enable);
}

static void
ao_ps2_read_keys(void)
{
	char	c;

	for (;;) {
		c = ao_ps2_getchar();
		printf("%02x %c\n", c, ' ' <= c && c < 0x7f ? c : '.');
		flush();
		if (c == ' ')
			break;
	}
}

static void
ao_console_send(void)
{
	char	c;

	while ((c = getchar()) != '~') {
		ao_console_putchar(c);
		flush();
	}
}

__code struct ao_cmds ao_demo_cmds[] = {
	{ ao_video_toggle, "V\0Toggle video" },
	{ ao_ball_toggle, "B\0Toggle ball" },
	{ ao_ps2_read_keys, "K\0Read keys from keyboard" },
	{ ao_console_send, "C\0Send data to console, end with ~" },
	{ 0, NULL }
};

int
main(void)
{
	ao_clock_init();

#if HAS_STACK_GUARD
	ao_mpu_init();
#endif

	ao_task_init();
	ao_serial_init();
	ao_timer_init();

	ao_spi_init();
	ao_dma_init();
	ao_exti_init();

	ao_ps2_init();
	ao_vga_init();
	ao_console_init();

	ao_cmd_init();

	ao_usb_init();

	ao_config_init();

	ao_add_task(&ball_task, ao_ball, "ball");
	ao_cmd_register(&ao_demo_cmds[0]);

	ao_start_scheduler();
	return 0;
}
