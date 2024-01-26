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

/*
 * Wiring the NewHaven NHD-C12864LZ-FSW-FBW-3V3 display
 *
 * PIN	Symbol	Connection
 *  1	/CS1	Chip select. Active low.
 *  2	/RES	Reset. Active low.
 *  3	A0	Register select. 0: instruction, 1: data
 *  4	/WR	NC
 *  5	/RD	NC
 *  6	DB0	NC
 *  7	DB1	NC
 *  8	DB2	NC
 *  8	DB3	NC
 *  10	DB4	NC
 *  11	DB5	NC
 *  12	DB6	SPI clock. Mode 3 (idle high, rising edge)
 *  13	DB7	SPI data.
 *  14	Vdd	+3.3V
 *  15	Vss	Ground
 *  16	Vout	1uF to ground
 *  17  CAP3+	1uF to CAP1-
 *  18	CAP1-	1uF to CAP3+ and CAP1+
 *  19	CAP1+	1uF to CAP1-
 *  20	CAP2+	1uF to CAP2-
 *  21	CAP2-	1uF to CAP2+
 *  22	V4	1uF to ground
 *  23	V3	1uF to ground
 *  24	V2	1uF to ground
 *  25	V1	1uF to ground
 *  26	V0	1uF to ground
 *  27	C86	MPU select. Ground
 *  28	PS	Parallel/serial select. Ground
 */

#define ST7565_DISPLAY_OFF			0xae
#define ST7565_DISPLAY_ON			0xaf
#define ST7565_DISPLAY_START_LINE_SET(line)	(0x40 | (line))
#define ST7565_PAGE_ADDRESS_SET(page)		(0xb0 | (page))
#define ST7565_COLUMN_ADDRESS_SET_MSN(nib)	(0x10 | (nib))
#define ST7565_COLUMN_ADDRESS_SET_LSN(nib)	(0x00 | (nib))
#define ST7565_ADC_SELECT_NORMAL		0xa0
#define ST7565_ADC_SELECT_REVERSE		0xa1
#define ST7565_DISPLAY_NORMAL			0xa6
#define ST7565_DISPLAY_REVERSE			0xa7
#define ST7565_DISPLAY_ALL_POINTS_OFF		0xa4
#define ST7565_DISPLAY_ALL_POINTS_ON		0xa5
#define ST7565_LCD_BIAS_1_9			0xa2
#define ST7565_LCD_BIAS_1_7			0xa3
#define ST7565_RMW				0xe0
#define ST7565_RMW_END				0xee
#define ST7565_RESET				0xe2
#define ST7565_COMMON_MODE_NORMAL		0xc0
#define ST7565_COMMON_MODE_REVERSE		0xc8
#define ST7565_POWER_CONTROL_SET(pc)		(0x28 | (pc))
#define ST7565_RESISTOR_RATIO_SET(rr)		(0x20 | (rr))
#define ST7565_ELECTRONIC_VOLUME_SET		0x81
#define ST7565_SLEEP_MODE			0xac
#define ST7565_BOOSTER_RATIO_SET		0xf8
#define ST7565_NOP				0xe3

#define AO_ST7565_SPI_SPEED			ao_spi_speed(AO_ST7565_SPI_BUS, 10000000)

void
ao_st7565_update(struct ao_bitmap *bitmap);

void
ao_st7565_init(void);

#endif /* _AO_ST7565_H_ */
