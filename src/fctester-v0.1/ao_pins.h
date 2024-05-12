/*
 * Copyright © 2014 Bdale Garbee <bdale@gag.com>
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

#define HAS_BEEP	0

#define AO_STACK_SIZE	384

#define IS_FLASH_LOADER	0

/* Crystal on the board */
#define AO_LPC_CLKIN	12000000

/* Main clock frequency. 48MHz for USB so we don't use the USB PLL */
#define AO_LPC_CLKOUT	48000000

/* System clock frequency */
#define AO_LPC_SYSCLK	24000000

#define HAS_USB		1

#define HAS_USB_CONNECT	0
#define HAS_USB_VBUS	0
#define HAS_USB_PULLUP	1
#define AO_USB_PULLUP_PORT	0
#define AO_USB_PULLUP_PIN	6

/* USART */

#define HAS_SERIAL		1
#define USE_SERIAL_0_STDIN	0
#define SERIAL_0_18_19		1
#define SERIAL_0_14_15		0
#define SERIAL_0_17_18		0
#define SERIAL_0_26_27		0

/* SPI */

#define HAS_SPI_0		0
#define SPI_SCK0_P0_6		0
#define HAS_SPI_1		0
#define SPI_SCK1_P1_15		0
#define SPI_MISO1_P0_22		0
#define SPI_MOSI1_P0_21		0

/* LEDs and FET switches (treat them like LEDs to avoid code duplication)  */

#define LED_PORT		0
#define LED_PIN_RED		7
#define LED_PIN_GREEN		8
#define AO_LED_RED		(1 << LED_PIN_RED)
#define AO_LED_GREEN		(1 << LED_PIN_GREEN)

#define FET_PIN_A		11
#define FET_A			(1 << FET_PIN_A)
#define FET_PIN_B		22
#define FET_B			(1 << FET_PIN_B)
#define FET_PIN_C		12
#define FET_C			(1 << FET_PIN_C)
#define FET_PIN_D		9
#define FET_D			(1 << FET_PIN_D)
#define FET_PIN_E		13
#define FET_E			(1 << FET_PIN_E)
#define FET_PIN_F		16
#define FET_F			(1 << FET_PIN_F)

/*	ignore the 'short' FET for now 
#define FET_PORT_SHORT		1
#define FET_PIN_SHORT		16
#define FET_SHORT		(1 << FET_PIN_SHORT)
*/

#define LEDS_AVAILABLE		(AO_LED_RED | AO_LED_GREEN | FET_A | FET_B | FET_C | FET_D | FET_E | FET_F)

/*
 * ADC
 */

#define HAS_ADC                 1

#define AO_NUM_ADC              1
#define AO_DATA_RING		4

#define AO_ADC_0                3

struct ao_adc {
        int16_t         current;
};

#define AO_ADC_DUMP(p) \
        printf("tick: %5lu current: %5d\n", (p)->tick, (p)->adc.current)

/* Kludge the SPI driver to not configure any
 * pin for SCK or MOSI
 */
#define HAS_SCK1		0
#define HAS_MOSI1		0
