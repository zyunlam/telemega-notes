/*
 * Copyright © 2024 Bdale Garbee <bdale@gag.com>
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

#define AO_STACK_SIZE           352
#define SLEEP_HASH_SIZE         3
#define AO_NUM_TASKS            6

#define HAS_TASK_QUEUE		1
#define IS_FLASH_LOADER		0

/* Crystal on the board */
#define AO_LPC_CLKIN    12000000

/* Main clock frequency. 48MHz for USB so we don't use the USB PLL */
#define AO_LPC_CLKOUT   48000000

/* System clock frequency */
#define AO_LPC_SYSCLK   24000000

#define HAS_USB			1
#define HAS_USB_CONNECT		0
#define HAS_USB_VBUS		0
#define HAS_USB_PULLUP		1
#define AO_USB_PULLUP_PORT      0
#define AO_USB_PULLUP_PIN       20

#define PACKET_HAS_SLAVE	0

#define HAS_SERIAL		0

#define AO_CONFIG_DEFAULT_FLIGHT_LOG_MAX	(1984 * 1024)
#define AO_CONFIG_MAX_SIZE			1024
#define LOG_ERASE_MARK				0x55
#define LOG_MAX_ERASE				128
#define AO_LOG_FORMAT				AO_LOG_FORMAT_EASYMOTOR

#define HAS_EEPROM		1
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	0
#define USE_STORAGE_CONFIG	1
#define AO_PA11_PA12_RMP	1
#define HAS_BEEP		1
#define HAS_BATTERY_REPORT	0
#define HAS_PAD_REPORT		0

/* Beeper is on pio0_1 ct32b0_mat2 .. requires wire mod on board! */
#define AO_LPC_BEEP_PORT	0
#define AO_LPC_BEEP_PIN		1
#define AO_LPC_BEEP_TIMER	0
#define AO_LPC_BEEP_CHANNEL	2

#define LOW_LEVEL_DEBUG		0

/*
 * ADC
 */
#define HAS_ADC			1

#define AO_NUM_ADC		1

#define AO_ADC_0		3

#define AO_DATA_RING		32

#define AO_ADC_DUMP(p) \
	printf("tick: %5lu pyro_current: %5d\n", \
	       (p)->tick, \
	       (p)->adc.pyro_current); 

struct ao_adc {
	int16_t			pyro_current;
};

/*
 * ADC reference in decivolts
 */
#define AO_ADC_REFERENCE_DV	33

/* LEDs */
#define LED_PORT		0
#define LED_PIN_RED		7
#define AO_LED_RED		(1 << LED_PIN_RED)
#define LED_PIN_GREEN		8
#define AO_LED_GREEN		(1 << LED_PIN_GREEN)

#define LEDS_AVAILABLE		(AO_LED_RED | AO_LED_GREEN)

#endif /* _AO_PINS_H_ */
