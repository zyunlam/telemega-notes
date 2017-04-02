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

/* SYSCLK = 32MHz (no need to go faster than CPU) */
#define AO_PLLDIV		3
#define AO_RCC_CFGR_PLLDIV	(STM_RCC_CFGR_PLLDIV_3)

/* HCLK = 32MHz (CPU clock) */
#define AO_AHB_PRESCALER	1
#define AO_RCC_CFGR_HPRE_DIV	STM_RCC_CFGR_HPRE_DIV_1

/* Run APB1 at 16MHz (HCLK/2) */
#define AO_APB1_PRESCALER	2
#define AO_RCC_CFGR_PPRE1_DIV	STM_RCC_CFGR_PPRE2_DIV_2

/* Run APB2 at 16MHz (HCLK/2) */
#define AO_APB2_PRESCALER	2
#define AO_RCC_CFGR_PPRE2_DIV	STM_RCC_CFGR_PPRE2_DIV_2

#define HAS_SERIAL_1		1
#define USE_SERIAL_1_STDIN	0
#define SERIAL_1_PB6_PB7	1
#define SERIAL_1_PA9_PA10	0

#define HAS_SERIAL_2		1
#define USE_SERIAL_2_STDIN	0
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

#define LED_PORT_ENABLE		STM_RCC_AHBENR_GPIOCEN
#define LED_PORT		(&stm_gpioc)
#define LED_PIN_RED		0
#define LED_PIN_GREEN		0
#define AO_LED_RED		(1 << LED_PIN_RED)
#define AO_LED_GREEN		(1 << LED_PIN_GREEN)

#define LEDS_AVAILABLE		0

#define HAS_GPS			0
#define HAS_FLIGHT		0
#define HAS_ADC			0
#define HAS_ADC_TEMP		0
#define HAS_LOG			0

/*
 * Pressure sensor settings
 */
#define HAS_MS5607		0
#define HAS_MS5611		0
#define AO_MS5607_PRIVATE_PINS	0
#define AO_MS5607_CS_PORT	(&stm_gpioc)
#define AO_MS5607_CS_PIN	4
#define AO_MS5607_CS_MASK	(1 << AO_MS5607_CS)
#define AO_MS5607_MISO_PORT	(&stm_gpioa)
#define AO_MS5607_MISO_PIN	6
#define AO_MS5607_MISO_MASK	(1 << AO_MS5607_MISO)
#define AO_MS5607_SPI_INDEX	AO_SPI_1_PA5_PA6_PA7

/*
 * SPI Flash memory
 */

#define M25_MAX_CHIPS		0
#define AO_M25_SPI_CS_PORT	(&stm_gpiod)
#define AO_M25_SPI_CS_MASK	(1 << 3)
#define AO_M25_SPI_BUS		AO_SPI_2_PB13_PB14_PB15

#define NUM_CMDS		16

/*
 * Monitor
 */

#define HAS_MONITOR		0
#define LEGACY_MONITOR		0
#define HAS_MONITOR_PUT		0
#define AO_MONITOR_LED		0
#define HAS_RSSI		0

/*
 * Profiling Viterbi decoding
 */

#ifndef AO_PROFILE
#define AO_PROFILE	       	0
#endif

/*
 * PWM output
 */

#define NUM_PWM			0
#define PWM_MAX			20000
#define AO_PWM_TIMER		stm_tim4
#define AO_PWM_TIMER_ENABLE	STM_RCC_APB1ENR_TIM4EN
#define AO_PWM_TIMER_SCALE	32

#define AO_PWM_0_GPIO		(&stm_gpiod)
#define AO_PWM_0_PIN		12

#define AO_PWM_1_GPIO		(&stm_gpiod)
#define AO_PWM_1_PIN		13

#define AO_PWM_2_GPIO		(&stm_gpiod)
#define AO_PWM_2_PIN		14

#define AO_PWM_3_GPIO		(&stm_gpiod)
#define AO_PWM_3_PIN		15

#endif /* _AO_PINS_H_ */
