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

static void
ao_st7565_set_brightness(uint8_t val)
{
	ao_st7565_instruction_param(ST7565_ELECTRONIC_VOLUME_SET, val);
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

void
ao_st7565_update(struct ao_bitmap *bitmap)
{
	uint8_t 	col, c, page;
	int		row;
	uint32_t	*line;
	uint8_t		*r;

	ao_st7565_setup();

	line = bitmap->base;
	for (page = 0; page < 8; page++) {
		uint8_t		i[4] = {
			ST7565_PAGE_ADDRESS_SET(7-page),
			ST7565_COLUMN_ADDRESS_SET_MSN(0 >> 4),
			ST7565_COLUMN_ADDRESS_SET_MSN(0 & 0xf),
			ST7565_RMW
		};
		memset(rotbuf, 0, sizeof(rotbuf));
		for (row = 7; row >= 0; row--) {
			r = rotbuf;
			for (col = 0; col < AO_BITMAP_STRIDE(AO_ST7565_WIDTH); col++) {
				uint32_t	bits = ~*line++;
				for (c = 0; c < 32; c++) {
					*r++ |= ((bits >> c) & 1) << row;
				}
			}
		}
		ao_st7565_instructions(i, 4);
		ao_st7565_data(rotbuf, AO_ST7565_WIDTH);
	}
}

void
ao_st7565_init(void)
{
	ao_enable_output(AO_ST7565_RESET_PORT, AO_ST7565_RESET_PIN, 1);
	ao_enable_output(AO_ST7565_A0_PORT, AO_ST7565_A0_PIN, 1);

	ao_enable_cs(AO_ST7565_CS_PORT, AO_ST7565_CS_PIN);
}
