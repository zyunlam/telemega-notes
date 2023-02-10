/*
 * Copyright © 2023 Bdale Garbee <bdale@gag.com>
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


/* 16MHz High speed external crystal */
#define AO_HSE			16000000

/* PLLVCO = 96MHz (so that USB will work) */
#define AO_PLLMUL		6
#define AO_RCC_CFGR_PLLMUL	(STM_RCC_CFGR_PLLMUL_6)

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

#define HAS_SERIAL_1		0
#define USE_SERIAL_1_STDIN	0
#define SERIAL_1_PB6_PB7	0
#define SERIAL_1_PA9_PA10	0

#define HAS_SERIAL_2		0
#define USE_SERIAL_2_STDIN	0
#define SERIAL_2_PA2_PA3	0
#define SERIAL_2_PD5_PD6	0
#define USE_SERIAL_2_FLOW	0
#define USE_SERIAL_2_SW_FLOW	0

#define HAS_SERIAL_3		1
#define USE_SERIAL_3_STDIN	0
#define SERIAL_3_PB10_PB11	0
#define SERIAL_3_PC10_PC11	0
#define SERIAL_3_PD8_PD9	1

#define HAS_USB			1
#define HAS_BEEP		0
#define HAS_BATTERY_REPORT	0
#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_APRS		0
#define HAS_COMPANION		0

#define HAS_SPI_1		0
#define HAS_SPI_2		0
#define HAS_I2C_1		0
#define HAS_I2C_2		0

#define LOW_LEVEL_DEBUG		0

#define LED_PORT_ENABLE		STM_RCC_AHBENR_GPIOBEN
#define LED_PORT		(&stm_gpiob)
#define LED_PIN_RED		13
#define LED_PIN_GREEN		12
#define AO_LED_RED		(1 << LED_PIN_RED)
#define AO_LED_GREEN		(1 << LED_PIN_GREEN)

#define LEDS_AVAILABLE		(AO_LED_RED | AO_LED_GREEN)

#define HAS_GPS			0
#define HAS_FLIGHT		0
#define HAS_ADC			1
#define HAS_ADC_TEMP		1
#define HAS_LOG			0

/*
 * ADC
 */
#define AO_DATA_RING		32

struct ao_adc {
	int16_t			v_dc;
	int16_t			temp;
};

#define AO_ADC_DUMP(p) \
	printf("tick: dc: %5d temp: %5d\n", \
	       (p)->adc.v_dc, (p)->adc.temp)

#define AO_ADC_V_DC		0
#define AO_ADC_V_DC_PORT	(&stm_gpioa)
#define AO_ADC_V_DC_PIN		0

#define AO_ADC_TEMP		1

#define AO_ADC_RCC_AHBENR	((1 << STM_RCC_AHBENR_GPIOAEN))

#define AO_NUM_ADC_PIN		2

#define AO_ADC_PIN0_PORT	AO_ADC_V_DC_PORT
#define AO_ADC_PIN0_PIN		AO_ADC_V_DC_PIN

#define AO_NUM_ADC	       	2

#define AO_ADC_SQ1		AO_ADC_V_DC
#define AO_ADC_SQ2		AO_ADC_TEMP

/*
 * Voltage divider on ADC DC sampler
 */
#define AO_DC_DIV_PLUS		100	/* 100k */
#define AO_DC_DIV_MINUS		10	/* 10k */

/*
 * ADC reference in decivolts
 */
#define AO_ADC_REFERENCE_DV	33

#endif /* _AO_PINS_H_ */
