/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
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
#include <ao_st7565.h>

#define WIDTH	AO_ST7565_WIDTH
#define HEIGHT	AO_ST7565_HEIGHT
#define STRIDE	AO_BITMAP_STRIDE(WIDTH)

static uint32_t	image[STRIDE * HEIGHT];

static struct ao_bitmap fb = {
	.base = image,
	.stride = STRIDE,
	.width = WIDTH,
	.height = HEIGHT,
	.damage = AO_BOX_INIT,
};

static void
ao_st7565_test(void)
{
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
	ao_st7565_update(&fb);
	ao_text(&fb, &BitstreamVeraSans_Roman_24_font,
		0, 20, "hello world", AO_BLACK, AO_COPY);
	ao_st7565_update(&fb);
}

static int16_t	x1 = 32, _y1 = 10, x2 = 32, y2 = 40;
static int16_t	dx1 = 2, dy1 = 2, dx2 = -2, dy2 = -1;

#define bounds(v,m,M,d)	\
		if (v < m) {			\
			v = m + m - v;		\
			d = -d;			\
		} else if (v > M) {		\
			v = M - (v - M);	\
			d = -d;			\
		}

static void
ao_st7565_line(void)
{
	int	i;

	for (i = 0; i < 100; i++) {
		ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
		ao_line(&fb, x1, _y1, x2, y2, AO_BLACK, AO_COPY);
		ao_st7565_update(&fb);
		x1 += dx1;
		_y1 += dy1;
		x2 += dx2;
		y2 += dy2;
		printf("%d,%d - %d,%d\n", x1, _y1, x2, y2);
		fflush(stdout);
		bounds(x1, 0, WIDTH, dx1);
		bounds(x2, 0, WIDTH, dx2);
		bounds(_y1, 0, HEIGHT, dy1);
		bounds(y2, 0, HEIGHT, dy2);
		ao_delay(AO_MS_TO_TICKS(200));
	}
}

static const struct ao_transform logo_transform = {
	.x_scale = 48, .x_off = 0,
	.y_scale = 48, .y_off = 0,
};

#define LOGO_FONT BenguiatGothicStd_Bold_26_font

static void
ao_st7565_poly(void)
{
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
	ao_logo(&fb, &logo_transform, &LOGO_FONT, 0x00000000, AO_COPY);
	ao_st7565_update(&fb);
}

const struct ao_cmds ao_st7565_cmds[] = {
	{ ao_st7565_test, "g\0Test ST7565 display" },
	{ ao_st7565_line, "l\0Draw lines" },
	{ ao_st7565_poly, "p\0Draw polygon" },
	{ 0, NULL },
};

int main(void)
{
	ao_clock_init();
	ao_led_init();
	ao_timer_init();
	ao_task_init();
	ao_dma_init();
	ao_spi_init();
	ao_serial_init();
	ao_usb_init();
	ao_st7565_init();
	ao_cmd_init();
	ao_cmd_register(ao_st7565_cmds);
	ao_start_scheduler();
}
