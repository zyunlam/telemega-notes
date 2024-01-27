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
#include <ao_packet.h>
#include <ao_companion.h>
#include <ao_aes.h>
#include <ao_quadrature.h>
#include <ao_button.h>
#include <ao_lco.h>
#include <ao_lco_cmd.h>
#include <ao_radio_cmac_cmd.h>
#include <ao_eeprom.h>
#include <ao_adc_single.h>
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

static const float pad_volts = 12.3f;
static const float lco_volts = 4.1f;
static const int rssi = -30;

static int	boxes[] = { 1, 2, 3, 5, 8, 11, 13, 17, 19, 23, 29, 31, 37, 62, 97 };

//static int	max_box = 97;

#define ARRAYSIZE(a)	(sizeof(a) / sizeof((a)[0]))

static bool
valid_box(int box)
{
	size_t i;
	if (box == 0)
		return true;
	for (i = 0; i < ARRAYSIZE(boxes); i++)
		if (boxes[i] == box)
			return true;
	return false;
}

#if 0
static void
next_box(void)
{
	for (int n = box_number + 1; n <= max_box; n++)
		if (valid_box(n)) {
			box_number = n;
			return;
		}
	box_number = 0;
}

static void
prev_box(void)
{
	for (int n = box_number - 1; n >= 0; n--)
		if (valid_box(n)) {
			box_number = n;
			return;
		}
	box_number = max_box;
}
#endif

static const struct ao_transform logo_transform = {
	.x_scale = 48, .x_off = 2,
	.y_scale = 48, .y_off = 0,
};

#define BIG_FONT BitstreamVeraSans_Roman_58_font
#define VOLT_FONT BitstreamVeraSans_Roman_58_font
#define SMALL_FONT BitstreamVeraSans_Roman_12_font
#define TINY_FONT BitstreamVeraSans_Roman_10_font
#define LOGO_FONT BenguiatGothicStd_Bold_26_font

#define LABEL_Y		(int16_t) (SMALL_FONT.ascent)
#define VALUE_Y		(int16_t) (LABEL_Y + BIG_FONT.ascent + 5)
#define BOX_X		2
#define PAD_X		90
#define BOX_LABEL_X	30
#define VOLT_LABEL_X	25
#define RSSI_LABEL_X	15
#define PAD_LABEL_X	95
#define SEP_X		(PAD_X - 8)
#define SCAN_X		(WIDTH - 100) / 2
#define SCAN_Y		50
#define SCAN_HEIGHT	3
#define FOUND_Y		63
#define FOUND_X		6
#define FOUND_WIDTH	17
#define MAX_VALID	(WIDTH / FOUND_WIDTH)

static int16_t	box_number = 88;
static int16_t	pad_number = 8;

static void
ao_st7565_poly(void)
{
	int16_t scan_number;
	char	str[8];
	int 	i;
	int	v;
	int	last_box;
	int16_t	b;

	for (scan_number = 0; scan_number < 100; scan_number++) {
		ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
		ao_logo(&fb, &logo_transform, &LOGO_FONT, AO_BLACK, AO_COPY);
		if (scan_number) {
			ao_rect(&fb, SCAN_X, SCAN_Y, (int16_t) scan_number, SCAN_HEIGHT, AO_BLACK, AO_COPY);
			b = 0;
			v = 0;
			last_box = 0;
			for (i = scan_number; i > 1; i--) {
				if (valid_box(i)) {
					if (!last_box)
						last_box = i;
					v++;
					if (v == MAX_VALID)
						break;
				}
			}
			for (; i <= scan_number; i++) {
				if (valid_box(i)) {
					sprintf(str, "%02d%s", i, i == last_box ? "" : ",");
					ao_text(&fb, &TINY_FONT, (int16_t) (FOUND_X + FOUND_WIDTH * b),
						FOUND_Y, str, AO_BLACK, AO_COPY);
					b++;
				}
			}
		}
		ao_st7565_update(&fb);
		ao_delay(AO_MS_TO_TICKS(50));
	}
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
	switch (box_number) {
	case 0:
		sprintf(str, "%4.1f", lco_volts);
		ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, AO_BLACK, AO_COPY);
		ao_text(&fb, &SMALL_FONT, VOLT_LABEL_X, LABEL_Y, "LCO Battery", AO_BLACK, AO_COPY);
		break;
	default:
		switch (pad_number) {
		case -1:
			sprintf(str, "%4.1f", pad_volts);
			ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, AO_BLACK, AO_COPY);
			ao_text(&fb, &SMALL_FONT, VOLT_LABEL_X, LABEL_Y, "Pad Battery", AO_BLACK, AO_COPY);
			break;
		case 0:
			sprintf(str, "%4d", rssi);
			ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, AO_BLACK, AO_COPY);
			ao_text(&fb, &SMALL_FONT, RSSI_LABEL_X, LABEL_Y, "Signal Strength", AO_BLACK, AO_COPY);
			break;
		default:
			sprintf(str, "%02d", box_number);
			ao_text(&fb, &BIG_FONT, BOX_X, VALUE_Y, str, AO_BLACK, AO_COPY);
			ao_text(&fb, &SMALL_FONT, BOX_LABEL_X, LABEL_Y, "Box", AO_BLACK, AO_COPY);

			sprintf(str, "%d", pad_number);
			ao_text(&fb, &BIG_FONT, PAD_X, VALUE_Y, str, AO_BLACK, AO_COPY);
			ao_text(&fb, &SMALL_FONT, PAD_LABEL_X, LABEL_Y, "Pad", AO_BLACK, AO_COPY);

			ao_rect(&fb, SEP_X, 0, 2, HEIGHT, AO_BLACK, AO_COPY);
		}
		break;
	}
	ao_st7565_update(&fb);
}

const struct ao_cmds ao_st7565_cmds[] = {
	{ ao_st7565_test, "g\0Test ST7565 display" },
	{ ao_st7565_line, "l\0Draw lines" },
	{ ao_st7565_poly, "p\0Draw polygon" },
	{ 0, NULL },
};

int
main(void)
{
	ao_clock_init();

	ao_led_init();
	ao_led_on(LEDS_AVAILABLE);
	ao_task_init();

	ao_timer_init();

	ao_spi_init();
	ao_dma_init();
	ao_exti_init();
	ao_adc_single_init();

	ao_beep_init();
	ao_cmd_init();

	ao_quadrature_init();
	ao_button_init();

	ao_radio_init();

	ao_usb_init();

	ao_st7565_init();

	ao_config_init();

	ao_lco_init();
	ao_lco_cmd_init();

//	ao_cmd_register(ao_st7565_cmds);

	ao_led_off(LEDS_AVAILABLE);

	ao_start_scheduler();
	return 0;
}
