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


/* 32MHz High speed external crystal */
#define AO_HSE			32000000

/* PLLVCO = 96MHz (so that USB will work) */
#define AO_PLLMUL		3
#define AO_RCC_CFGR_PLLMUL	(STM_RCC_CFGR_PLLMUL_3)

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
#define SERIAL_2_PA2_PA3	1
#define SERIAL_2_PD5_PD6	0
#define USE_SERIAL_2_FLOW	0
#define USE_SERIAL_2_SW_FLOW	0

#define HAS_SERIAL_3		1
#define USE_SERIAL_3_STDIN	0
#define SERIAL_3_PB10_PB11	1
#define SERIAL_3_PC10_PC11	0
#define SERIAL_3_PD8_PD9	0

#define HAS_EEPROM		1
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	1
#define USE_STORAGE_CONFIG	0
#define HAS_USB			1
#define HAS_BEEP		1
#define BEEPER_TIMER		4
#define BEEPER_CHANNEL		4
#define BEEPER_PORT		(&stm_gpiob)
#define BEEPER_PIN		9
#define HAS_BATTERY_REPORT	1
#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_APRS		0
#define HAS_COMPANION		0

#define HAS_SPI_1		0
#define SPI_1_PA5_PA6_PA7	1	/* Barometer */
#define SPI_1_PB3_PB4_PB5	1	/* Accelerometer */
#define SPI_1_PE13_PE14_PE15	1	/* MPU6000 */
#define SPI_1_OSPEEDR		STM_OSPEEDR_10MHz

#define HAS_SPI_2		0
#define SPI_2_PB13_PB14_PB15	1	/* Flash, Companion */
#ifndef MMC5983_I2C
#define SPI_2_PD1_PD3_PD4	1	/* MMC5983 */
#endif
#define SPI_2_OSPEEDR		STM_OSPEEDR_10MHz

#define HAS_I2C_1		0
#define I2C_1_PB8_PB9		0

#define HAS_I2C_2		0
#define I2C_2_PB10_PB11		0

#define PACKET_HAS_SLAVE	0
#define PACKET_HAS_MASTER	0

#define LOW_LEVEL_DEBUG		0

#define HAS_ADC			1
#define HAS_ADC_TEMP		1

/*
 * ADC
 */
#define AO_DATA_RING		32
#define AO_ADC_NUM_SENSE	3

struct ao_adc {
	int16_t			sense[AO_ADC_NUM_SENSE];
	int16_t			v_batt;
	int16_t			temp;
};

#define AO_ADC_DUMP(p) \
	printf("tick: %5lu A: %5d B: %5d C: %5d batt: %5d temp: %5d\n", \
	       (p)->tick, \
	       (p)->adc.sense[0], (p)->adc.sense[1], (p)->adc.sense[2], \
	       (p)->adc.v_batt, (p)->adc.temp)

#define AO_ADC_SENSE_A		4
#define AO_ADC_SENSE_A_PORT	(&stm_gpioa)
#define AO_ADC_SENSE_A_PIN	4

#define AO_ADC_SENSE_B		5
#define AO_ADC_SENSE_B_PORT	(&stm_gpioa)
#define AO_ADC_SENSE_B_PIN	5

#define AO_ADC_SENSE_C		6
#define AO_ADC_SENSE_C_PORT	(&stm_gpioa)
#define AO_ADC_SENSE_C_PIN	6

#define AO_ADC_V_BATT		0
#define AO_ADC_V_BATT_PORT	(&stm_gpioa)
#define AO_ADC_V_BATT_PIN	0

#define AO_ADC_TEMP		16

#define AO_ADC_RCC_AHBENR	((1 << STM_RCC_AHBENR_GPIOAEN) | \
				 (1 << STM_RCC_AHBENR_GPIOEEN) | \
				 (1 << STM_RCC_AHBENR_GPIOBEN))

#define AO_NUM_ADC_PIN		(AO_ADC_NUM_SENSE + 1)

#define AO_ADC_PIN0_PORT	AO_ADC_SENSE_A_PORT
#define AO_ADC_PIN0_PIN		AO_ADC_SENSE_A_PIN
#define AO_ADC_PIN1_PORT	AO_ADC_SENSE_B_PORT
#define AO_ADC_PIN1_PIN		AO_ADC_SENSE_B_PIN
#define AO_ADC_PIN2_PORT	AO_ADC_SENSE_C_PORT
#define AO_ADC_PIN2_PIN		AO_ADC_SENSE_C_PIN
#define AO_ADC_PIN3_PORT	AO_ADC_V_BATT_PORT
#define AO_ADC_PIN3_PIN		AO_ADC_V_BATT_PIN

#define AO_NUM_ADC	       	(AO_ADC_NUM_SENSE + 2)

#define AO_ADC_SQ1		AO_ADC_SENSE_A
#define AO_ADC_SQ2		AO_ADC_SENSE_B
#define AO_ADC_SQ3		AO_ADC_SENSE_C
#define AO_ADC_SQ4		AO_ADC_V_BATT
#define AO_ADC_SQ5		AO_ADC_TEMP

/*
 * Voltage divider on ADC battery sampler
 */
#define AO_BATTERY_DIV_PLUS	56	/* 5.6k */
#define AO_BATTERY_DIV_MINUS	100	/* 10k */

/*
 * Voltage divider on ADC sense inputs
 */
#define AO_PYRO_BATTERY_DIV_PLUS	56	/* 5.6k */
#define AO_PYRO_BATTERY_DIV_MINUS	100	/* 10k */

/*
 * ADC reference in decivolts
 */
#define AO_ADC_REFERENCE_DV	33

#endif /* _AO_PINS_H_ */
