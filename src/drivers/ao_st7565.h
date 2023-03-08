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

#ifndef _AO_ST7565_H_
#define _AO_ST7565_H_

#include <ao_draw.h>

#define AO_ST7565_DISPLAY_OFF			0xae
#define AO_ST7565_DISPLAY_ON			0xaf
#define AO_ST7565_DISPLAY_START_LINE_SET(line)	(0x40 | (line))
#define AO_ST7565_PAGE_ADDRESS_SET(page)	(0xb0 | (page))
#define AO_ST7565_COLUMN_ADDRESS_SET_MSN(nib)	(0x10 | (nib))
#define AO_ST7565_COLUMN_ADDRESS_SET_LSN(nib)	(0x00 | (nib))
#define AO_ST7565_ADC_SELECT_NORMAL		0xa0
#define AO_ST7565_ADC_SELECT_REVERSE		0xa1
#define AO_ST7565_DISPLAY_NORMAL		0xa6
#define AO_ST7565_DISPLAY_REVERSE		0xa7
#define AO_ST7565_DISPLAY_ALL_POINTS_OFF	0xa4
#define AO_ST7565_DISPLAY_ALL_POINTS_ON		0xa5
#define AO_ST7565_LCD_BIAS_1_9			0xa2
#define AO_ST7565_LCD_BIAS_1_7			0xa3
#define AO_ST7565_RMW				0xe0
#define AO_ST7565_RMW_END			0xee
#define AO_ST7565_RESET				0xe2
#define AO_ST7565_COMMON_MODE_NORMAL		0xc0
#define AO_ST7565_COMMON_MODE_REVERSE		0xc8
#define AO_ST7565_POWER_CONTROL_SET(pc)		(0x28 | (pc))
#define AO_ST7565_RESISTOR_RATIO_SET(rr)	(0x20 | (rr))
#define AO_ST7565_ELECTRONIC_VOLUME_SET		0x81
#define AO_ST7565_SLEEP_MODE			0xac
#define AO_ST7565_BOOSTER_RATIO_SET		0xf8
#define AO_ST7565_NOP				0xe3

#define AO_ST7565_SPI_SPEED			ao_spi_speed(20000000)

void
ao_st7565_update(struct ao_bitmap *bitmap);

void
ao_st7565_init(void);

#endif /* _AO_ST7565_H_ */
