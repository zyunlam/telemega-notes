/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
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

#define AO_STACK_SIZE	448

#define IS_FLASH_LOADER		0

#define LPC_EEPROM_BYTES	2048

/* Crystal on the board */
#define AO_LPC_CLKIN	12000000

/* Main clock frequency. 48MHz for USB so we don't use the USB PLL */
#define AO_LPC_CLKOUT	48000000

/* System clock frequency */
#define AO_LPC_SYSCLK	24000000

#define HAS_SERIAL_0		1
#define SERIAL_1_13_14		1
#define USE_SERIAL_0_STDIN	0

#define ao_gps_getchar		ao_serial0_getchar
#define ao_gps_putchar		ao_serial0_putchar
#define ao_gps_set_speed	ao_serial0_set_speed
#define ao_gps_fifo		(ao_usart_rx_fifo)

#define HAS_EEPROM		1
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	1
#define USE_STORAGE_CONFIG	0
#define LOG_ERASE_MARK		0
#define HAS_USB			1
#define HAS_BEEP		0
#define HAS_RADIO		1
#define HAS_TELEMETRY		1
#define HAS_RDF			1
#define HAS_APRS		1
#define HAS_LED			1

#define HAS_USB_PULLUP		1
#define AO_USB_PULLUP_PORT	1
#define AO_USB_PULLUP_PIN	23
#define HAS_USB_CONNECT		1
#define AO_USB_CONNECT_PORT	0
#define AO_USB_CONNECT_PIN	11

/* LED */
#define LED_0_PORT		1
#define LED_0_PIN		25
#define AO_LED_GREEN		(1 << 0)
#define AO_LED_PANIC		AO_LED_GREEN

/* Radio */
#define HAS_SPI_0		1
#define SPI_SCK0_P0_6		1
#define SPI_0_OSPEEDR		AO_SPI_OSPEED_12MHz

/* Flash */
#define HAS_SPI_1		1
#define SPI_SCK1_P1_20		1
#define SPI_MISO1_P0_22		1
#define SPI_MOSI1_P1_22		1

#define HAS_GPS			1
#define HAS_FLIGHT		0
#define HAS_LOG			1
#define FLIGHT_LOG_APPEND	1
#define HAS_TRACKER		1

#define AO_CONFIG_DEFAULT_APRS_INTERVAL		0
#define AO_LOG_FORMAT				AO_LOG_FORMAT_TELEGPS

/*
 * GPS
 */

#define AO_SERIAL_SPEED_UBLOX	AO_SERIAL_SPEED_9600

/*
 * Radio (cc1200)
 */

/* gets pretty close to 434.550 */

#define AO_RADIO_CAL_DEFAULT    5695733

#define AO_FEC_DEBUG            0
#define AO_CC1200_SPI_CS_PORT   1
#define AO_CC1200_SPI_CS_PIN    27
#define AO_CC1200_SPI_BUS       0
#define AO_CC1200_SPI		1

#define AO_CC1200_INT_PORT      1
#define AO_CC1200_INT_PIN       5

#define AO_CC1200_INT_GPIO      2
#define AO_CC1200_INT_GPIO_IOCFG        CC1200_IOCFG2

#define HAS_BOOT_RADIO          0

/*
 * Flash (M25)
 */
#define M25_MAX_CHIPS		1
#define AO_M25_SPI_CS_PORT	0
#define AO_M25_SPI_CS_MASK	(1 << 23)
#define AO_M25_SPI_BUS		1

#define PACKET_HAS_SLAVE	0

/*
 * ADC
 */

#define HAS_ADC			1
#define LOG_ADC			0

#define AO_DATA_RING		4

#define AO_ADC_2		1

struct ao_adc {
	int16_t			v_batt;
};

#define AO_ADC_DUMP(p) \
	printf("tick: %5lu batt: %5d\n", \
	       (p)->tick, \
	       (p)->adc.v_batt)

/*
 * Voltage divider on ADC battery sampler
 */
#define AO_BATTERY_DIV_PLUS	56	/* 5.6k */
#define AO_BATTERY_DIV_MINUS	100	/* 10k */

/*
 * ADC reference in decivolts
 */
#define AO_ADC_REFERENCE_DV	33

#endif /* _AO_PINS_H_ */
