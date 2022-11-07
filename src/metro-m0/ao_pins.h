/*
 * Copyright © 2022 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

#define LED_0_PORT	(&samd21_port_a)
#define LED_0_PIN	2

#define LED_BLUE	(1 << 0)

#define AO_LED_PANIC	LED_BLUE

#define HAS_BEEP	0

#define HAS_USB		1
#define USE_USB_STDIO	1

#define HAS_LED		1

#define AO_DFLL48M		48000000
#define AO_XOSC32K		32768

#define AO_AHB_PRESCALER	1
#define AO_APBA_PRESCALER	1

#define HAS_SPI_5		1
#define SPI_5_PB22_PB23_PB03	1

#define HAS_SPI_4		1
#define SPI_4_PB10_PB11_PA12	1

/*
 * SPI Flash memory
 */

#define M25_MAX_CHIPS		1

#if 1
#define AO_M25_SPI_CS_PORT	(&samd21_port_a)
#define AO_M25_SPI_CS_MASK	(1 << 13)
#define AO_M25_SPI_BUS		AO_SPI_5_PB22_PB23_PB03
#else

#define AO_M25_SPI_CS_PORT	(&samd21_port_a)
#define AO_M25_SPI_CS_MASK	(1 << 14) /* D2 */
#define AO_M25_SPI_BUS		AO_SPI_4_PB10_PB11_PA12

#endif

#endif /* _AO_PINS_H_ */
