/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

#define HAS_TASK_QUEUE		1

/* 8MHz High speed external crystal */
#define AO_HSE			8000000

/* PLLVCO = 96MHz (so that USB will work) */
#define AO_PLLMUL		12
#define AO_RCC_CFGR_PLLMUL	(STM_RCC_CFGR_PLLMUL_12)

/* SYSCLK = 24MHz */
#define AO_PLLDIV		4
#define AO_RCC_CFGR_PLLDIV	(STM_RCC_CFGR_PLLDIV_4)

/* HCLK = 24MHz (CPU clock) */
#define AO_AHB_PRESCALER	1
#define AO_RCC_CFGR_HPRE_DIV	STM_RCC_CFGR_HPRE_DIV_1

/* Run APB1 at HCLK/1 */
#define AO_APB1_PRESCALER	1
#define AO_RCC_CFGR_PPRE1_DIV	STM_RCC_CFGR_PPRE1_DIV_1

/* Run APB2 at HCLK/1 */
#define AO_APB2_PRESCALER	1
#define AO_RCC_CFGR_PPRE2_DIV	STM_RCC_CFGR_PPRE2_DIV_1

/* Allow for non-maskable interrupts at priority 0 */
#define AO_NONMASK_INTERRUPT	1

/* PS/2 keyboard connection */
#define AO_PS2_CLOCK_PORT	(&stm_gpiod)
#define AO_PS2_CLOCK_BIT	9
#define AO_PS2_DATA_PORT	(&stm_gpiod)
#define AO_PS2_DATA_BIT		8

#define HAS_SERIAL_1		1
#define USE_SERIAL_1_STDIN	1
#define SERIAL_1_PB6_PB7	1
#define SERIAL_1_PA9_PA10	0

#define HAS_SERIAL_2		1
#define USE_SERIAL_2_STDIN	1
#define SERIAL_2_PA2_PA3	0
#define SERIAL_2_PD5_PD6	1

#define HAS_SERIAL_3		0
#define USE_SERIAL_3_STDIN	0
#define SERIAL_3_PB10_PB11	0
#define SERIAL_3_PC10_PC11	0
#define SERIAL_3_PD8_PD9	0

#define HAS_EEPROM		0
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	0
#define USE_STORAGE_CONFIG	0
#define HAS_USB			1
#define HAS_BEEP		0
#define HAS_BATTERY_REPORT	0
#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_APRS		0
#define HAS_COMPANION		0

#define HAS_SPI_1		0
#define SPI_1_PA5_PA6_PA7	0
#define SPI_1_PB3_PB4_PB5	0
#define SPI_1_PE13_PE14_PE15	0
#define SPI_1_OSPEEDR		STM_OSPEEDR_10MHz

#define HAS_SPI_2		1
#define SPI_2_PB13_PB14_PB15	0
#define SPI_2_PD1_PD3_PD4	1	/* LED displays, microSD */
#define SPI_2_OSPEEDR		STM_OSPEEDR_10MHz

#define SPI_2_PORT		(&stm_gpiod)
#define SPI_2_SCK_PIN		1
#define SPI_2_MISO_PIN		3
#define SPI_2_MOSI_PIN		4

#define HAS_I2C_1		0
#define I2C_1_PB8_PB9		0

#define HAS_I2C_2		0
#define I2C_2_PB10_PB11		0

#define PACKET_HAS_SLAVE	0
#define PACKET_HAS_MASTER	0

#define LOW_LEVEL_DEBUG		0

#define HAS_GPS			0
#define HAS_FLIGHT		0
#define HAS_ADC			0
#define HAS_ADC_TEMP		0
#define HAS_LOG			0

#define NUM_CMDS		16

/* SD card */
#define AO_SDCARD_SPI_BUS	AO_SPI_2_PD1_PD3_PD4
#define AO_SDCARD_SPI_CS_PORT	(&stm_gpiod)
#define AO_SDCARD_SPI_CS_PIN	2
#define AO_SDCARD_SPI_PORT	(&stm_gpiod)
#define AO_SDCARD_SPI_SCK_PIN	1
#define AO_SDCARD_SPI_MISO_PIN	3
#define AO_SDCARD_SPI_MOSI_PIN	4

#endif /* _AO_PINS_H_ */
