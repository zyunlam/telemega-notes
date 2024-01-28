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

static void
ao_st7565_reset(void)
{
	ao_gpio_set(AO_ST7565_RESET_PORT, AO_ST7565_RESET_PIN, 0);
	ao_delay(AO_MS_TO_TICKS(100));
	ao_gpio_set(AO_ST7565_RESET_PORT, AO_ST7565_RESET_PIN, 1);
	ao_delay(AO_MS_TO_TICKS(100));
}


static void
ao_st7565_start(uint8_t a0)
{
	ao_gpio_set(AO_ST7565_A0_PORT, AO_ST7565_A0_PIN, a0);
	ao_spi_get_bit(AO_ST7565_CS_PORT,
		       AO_ST7565_CS_PIN,
		       AO_ST7565_SPI_BUS,
		       AO_ST7565_SPI_SPEED);
}

static void
ao_st7565_stop(void)
{
	ao_spi_put_bit(AO_ST7565_CS_PORT,
		       AO_ST7565_CS_PIN,
		       AO_ST7565_SPI_BUS);
	ao_gpio_set(AO_ST7565_A0_PORT, AO_ST7565_A0_PIN, 1);
}


static void
ao_st7565_instruction(uint8_t cmd)
{
	ao_st7565_start(0);
	ao_spi_send(&cmd, 1, AO_ST7565_SPI_BUS);
	ao_st7565_stop();
}

static void
ao_st7565_instruction_param(uint8_t cmd, uint8_t param)
{
	uint8_t	b[2] = { cmd, param };
	ao_st7565_start(0);
	ao_spi_send(b, 2, AO_ST7565_SPI_BUS);
	ao_st7565_stop();
}

static void
ao_st7565_instructions(const uint8_t *cmd, uint16_t len)
{
	ao_st7565_start(0);
	ao_spi_send(cmd, len, AO_ST7565_SPI_BUS);
	ao_st7565_stop();
}

static void
ao_st7565_data(const void *base, uint16_t len)
{
	ao_st7565_start(1);
	ao_spi_send(base, len, AO_ST7565_SPI_BUS);
	ao_st7565_stop();
}

static uint8_t brightness;

void
ao_st7565_set_brightness(uint8_t val)
{
	if (val > 63)
		val = 63;
	brightness = val;
	ao_st7565_instruction_param(ST7565_ELECTRONIC_VOLUME_SET, val);
}

uint8_t
ao_st7565_get_brightness(void)
{
	return brightness;
}

static bool setup_done;

static void
ao_st7565_setup(void)
{
	static const uint8_t init[] = {
		/*
		 * Should be set to one of ST7565_LCD_BIAS_1_9 or
		 * ST7565_LCD_BIAS_1_7
		 */
		AO_ST7565_BIAS,
		ST7565_ADC_SELECT_NORMAL,
		ST7565_COMMON_MODE_NORMAL,
		ST7565_DISPLAY_START_LINE_SET(0),
		ST7565_POWER_CONTROL_SET(0x4),
	};

	if (setup_done)
		return;
	setup_done = true;
	ao_st7565_reset();
	ao_st7565_instructions(init, sizeof(init));
	ao_delay(AO_MS_TO_TICKS(50));
	ao_st7565_instruction(ST7565_POWER_CONTROL_SET(0x6));
	ao_delay(AO_MS_TO_TICKS(50));
	ao_st7565_instruction(ST7565_POWER_CONTROL_SET(0x7));
	ao_delay(AO_MS_TO_TICKS(10));
	ao_st7565_instruction(ST7565_RESISTOR_RATIO_SET(5));
	ao_st7565_instruction(ST7565_DISPLAY_ON);
	ao_st7565_set_brightness(0x10);
}

static uint8_t	rotbuf[AO_ST7565_WIDTH];

#define WIDTH	AO_ST7565_WIDTH
#define HEIGHT	AO_ST7565_HEIGHT
#define STRIDE	AO_BITMAP_STRIDE(WIDTH)

static uint32_t	previous_image[STRIDE * HEIGHT];

void
ao_st7565_update(struct ao_bitmap *bitmap)
{
	int16_t		col, c, page;
	int16_t		row;
	uint32_t	*line, *prev, *l;
	uint32_t	bits;
	uint8_t		*r;
	int16_t		min_col, min_row, max_col, max_row;
	int16_t		min_page, max_page;

	ao_st7565_setup();

	min_col = STRIDE - 1;
	max_col = 0;
	min_row = HEIGHT - 1;
	max_row = 0;
	line = bitmap->base;
	prev = previous_image;
	for (row = 0; row < HEIGHT; row++) {
		for (col = 0; col < STRIDE; col++) {
			bits = *line++;
			if (bits != *prev) {
				*prev = bits;
				if (row < min_row)
					min_row = row;
				if (row > max_row)
					max_row = row;
				if (col < min_col)
					min_col = col;
				if (col > max_col)
					max_col = col;
			}
			prev++;
		}
	}

	if (min_col > max_col || min_row > max_row)
		return;

	min_page = min_row >> 3;
	max_page = max_row >> 3;
	line = bitmap->base + min_page * 8 * STRIDE + min_col;

	uint8_t first_col = (uint8_t) (min_col * 32);
	uint8_t num_col = (uint8_t) (max_col + 1 - min_col) * 32;

	for (page = min_page; page <= max_page; page++) {
		uint8_t		i[4] = {
			ST7565_PAGE_ADDRESS_SET(7-(uint8_t) page),
			ST7565_COLUMN_ADDRESS_SET_MSN(first_col >> 4),
			ST7565_COLUMN_ADDRESS_SET_LSN(first_col & 0xf),
			ST7565_RMW
		};
		memset(rotbuf, 0, num_col);
		for (row = 7; row >= 0; row--) {
			r = rotbuf;
			l = line;
			line += STRIDE;
			for (col = min_col; col <= max_col; col++) {
				bits = ~*l++;
				for (c = 0; c < 32; c++) {
					*r++ |= ((bits >> c) & 1) << row;
				}
			}
		}
		ao_st7565_instructions(i, 4);
		ao_st7565_data(rotbuf, num_col);
	}
}

void
ao_st7565_init(void)
{
	memset(previous_image, 0xff, sizeof(previous_image));
	ao_enable_output(AO_ST7565_RESET_PORT, AO_ST7565_RESET_PIN, 1);
	ao_enable_output(AO_ST7565_A0_PORT, AO_ST7565_A0_PIN, 1);

	ao_enable_cs(AO_ST7565_CS_PORT, AO_ST7565_CS_PIN);
}
