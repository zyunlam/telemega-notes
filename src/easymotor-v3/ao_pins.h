/*
 * Copyright © 2022 Bdale Garbee <bdale@gag.com>
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
#define USE_SERIAL_0_STDIN	1
#define SERIAL_0_18_19          1
#define SERIAL_0_14_15          0
#define SERIAL_0_17_18          0
#define SERIAL_0_26_27          0

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
#define HAS_BATTERY_REPORT	1
#define HAS_PAD_REPORT		1

/* Beeper is on pio0_1 ct32b0_mat2 */
#define BEEPER_PORT		0
#define BEEPER_PIN		1
#define BEEPER_TIMER		0
#define BEEPER_OUTPUT		2

#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_APRS		0
#define HAS_COMPANION		0

#define LOW_LEVEL_DEBUG		0

#define HAS_GPS			0
#define HAS_FLIGHT		1
#define HAS_LOG			1

/*
 * ADC
 */
#define HAS_ADC			1

#define AO_NUM_ADC		2

#define AO_ADC_0		1
#define AO_ADC_1		1

#define AO_DATA_RING		32

struct ao_adc {
	int16_t			v_batt;
	int16_t			motor_pressure;
};

#define AO_ADC_DUMP(p) \
	printf("tick: %5lu batt: %5d motor_pressure: %5d\n", \
	       (p)->tick, \
	       (p)->adc.v_batt, \
	       (p)->adc.motor_pressure); 

/*
 * Voltage divider on ADC battery sampler
 */
#define AO_BATTERY_DIV_PLUS     56     /* 5.6k */
#define AO_BATTERY_DIV_MINUS    100    /* 10k */

/*
 * Voltage divider on pressure sensor input
 */
#define AO_PRESSURE_DIV_PLUS    56      /* 5.6k 0.1% */
#define AO_PRESSURE_DIV_MINUS   100     /* 10k  0.1% */ 

/*
 * ADC reference in decivolts
 */
#define AO_ADC_REFERENCE_DV	33

/* SPI */

#define HAS_SPI_0		1
#define SPI_0_MODE		((0 << LPC_SSP_CR0_CPOL) | (0 << LPC_SSP_CR0_CPHA))
#define SPI_SCK0_P0_6           1
#define HAS_SPI_1               1
#define SPI_SCK1_P1_15          1
#define SPI_MISO1_P0_22         1
#define SPI_MOSI1_P0_21         1
#define SPI_1_MODE		((1 << LPC_SSP_CR0_CPOL) | (1 << LPC_SSP_CR0_CPHA))

/*
 * SPI Flash memory
 */

#define M25_MAX_CHIPS		1
#define AO_M25_SPI_CS_PORT	0
#define AO_M25_SPI_CS_MASK	(1 << 3)
#define AO_M25_SPI_BUS		0

/* ADXL375 */

#define HAS_ADXL375		1
#define AO_ADXL375_SPI_INDEX	1
#define AO_ADXL375_CS_PORT	0
#define AO_ADXL375_CS_PIN	19

#define AO_ADXL375_AXIS		x
#define AO_ADXL375_ACROSS_AXIS	y
#define AO_ADXL375_THROUGH_AXIS	z
#define AO_ADXL375_INVERT	0
#define HAS_IMU			1
#define USE_ADXL375_IMU		1

/* Motor pressure */
#define HAS_MOTOR_PRESSURE	1
#define ao_data_motor_pressure(packet) ((packet)->adc.motor_pressure)

typedef int16_t	motor_pressure_t;

#endif /* _AO_PINS_H_ */
