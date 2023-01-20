/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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

#define LED_PORT_ENABLE	STM_RCC_AHBENR_IOPBEN
#define LED_PORT	(&stm_gpiob)
#define LED_PIN_GREEN	3
#define AO_LED_GREEN	(1 << LED_PIN_GREEN)
#define AO_LED_PANIC	AO_LED_GREEN
#define AO_CMD_LEN	128
#define AO_LISP_POOL_TOTAL	3072
#define AO_LISP_SAVE	1
#define AO_STACK_SIZE	1024

#define LEDS_AVAILABLE	(AO_LED_GREEN)

#define AO_POWER_MANAGEMENT	0

/* 48MHz clock based on USB */
#define AO_HSI48	1

/* HCLK = 48MHz */
#define AO_AHB_PRESCALER	1
#define AO_RCC_CFGR_HPRE_DIV	STM_RCC_CFGR_HPRE_DIV_1

/* APB = 48MHz */
#define AO_APB_PRESCALER	1
#define AO_RCC_CFGR_PPRE_DIV	STM_RCC_CFGR_PPRE_DIV_1

#define HAS_USB				1
#define AO_USB_DIRECTIO			0
#define AO_PA11_PA12_RMP		0
#define HAS_BEEP			0

#define IS_FLASH_LOADER	0

#define HAS_SERIAL_2		0
#define SERIAL_2_PA2_PA15	1
#define USE_SERIAL_2_FLOW	0
#define USE_SERIAL_2_STDIN	1
#define DELAY_SERIAL_2_STDIN	0

#define HAS_SPI_1	1
#define SPI_1_PA5_PA6_PA7	1
#define SPI_1_OSPEEDR		STM_OSPEEDR_HIGH
#define SPI_1_PB3_PB4_PB5	0

#define HAS_SPI_2	0

#define HAS_BMI088	1
#define HAS_IMU		1

#define ao_data_along(packet)   ((packet)->bmi088.acc.x)
#define ao_data_across(packet)  (-(packet)->bmi088.acc.y)
#define ao_data_through(packet) ((packet)->bmi088.acc.z)

#define ao_data_roll(packet)    ((packet)->bmi088.gyr.x)
#define ao_data_pitch(packet)   (-(packet)->bmi088.gyr.y)
#define ao_data_yaw(packet)     ((packet)->bmi088.gyr.z)

#define AO_BMI088_ACC_CS_PORT	(&stm_gpioa)
#define AO_BMI088_ACC_CS_PIN	0
#define AO_BMI088_GYR_CS_PORT	(&stm_gpioa)
#define AO_BMI088_GYR_CS_PIN	1
#define AO_BMI088_SPI_BUS	(AO_SPI_1_PA5_PA6_PA7 | AO_SPI_MODE_0)

#define AO_DATA_RING	32

#endif /* _AO_PINS_H_ */
