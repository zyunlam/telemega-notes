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

#define HAS_RADIO		1
#define HAS_RADIO_RATE		1
#define HAS_TELEMETRY		0

#define HAS_FLIGHT		0
#define HAS_USB			1
#define HAS_BEEP		0
#define HAS_GPS			0
#define HAS_SERIAL_1		0
#define HAS_ADC			1
#define HAS_DBG			0
#define HAS_EEPROM		1
#define HAS_LOG			1
#define HAS_PAD			1
#define IGNITE_ON_P0		0
#define PACKET_HAS_MASTER	0
#define PACKET_HAS_SLAVE	0
#define AO_DATA_RING		32
#define HAS_FIXED_PAD_BOX	1

#define AO_CONFIG_DEFAULT_FLIGHT_LOG_MAX       (512 * 1024)
#define AO_CONFIG_MAX_SIZE                     1024
#define LOG_ERASE_MARK                         0x55
#define LOG_MAX_ERASE                          128
#define AO_LOG_FORMAT                          AO_LOG_FORMAT_TELEMETRUM

/* 16MHz High speed external crystal */
#define AO_HSE			16000000

/* PLLVCO = 96MHz (so that USB will work) */
#define AO_PLLMUL		6
#define AO_RCC_CFGR_PLLMUL	(STM_RCC_CFGR_PLLMUL_6)

#define AO_CC1200_FOSC		40000000

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

#define HAS_EEPROM		1
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	1
#define USE_STORAGE_CONFIG	0
#define HAS_USB			1
#define HAS_RADIO		1
#define HAS_RADIO_RATE		1
#define HAS_TELEMETRY		0
#define HAS_AES			1

#define HAS_SPI_1		1	/* ADS131A0X */
#define SPI_1_PA5_PA6_PA7	0
#define SPI_1_PB3_PB4_PB5	0
#define SPI_1_PE13_PE14_PE15	1
#define SPI_1_OSPEEDR		STM_OSPEEDR_10MHz

#define HAS_SPI_2		1	/* CC1200 and W25Q64 */
#define SPI_2_PB13_PB14_PB15	0
#define SPI_2_PD1_PD3_PD4	1
#define SPI_2_GPIO		(&stm_gpiod)
#define SPI_2_SCK		1
#define SPI_2_MISO		3
#define SPI_2_MOSI		4
#define SPI_2_OSPEEDR		STM_OSPEEDR_10MHz

#define HAS_I2C_1		0

#define HAS_I2C_2		0

#define PACKET_HAS_SLAVE	0
#define PACKET_HAS_MASTER	0

#define FAST_TIMER_FREQ		10000	/* .1ms for debouncing */

/*
 * ADS131A0X analog to digital converter
 */

#define HAS_ADS131A0X			1
#define AO_ADS131A0X_SPI_CS_PORT	(&stm_gpiob)
#define AO_ADS131A0X_SPI_CS_PIN		15
#define AO_ADS131A0X_SPI_CS_MASK	(1 << AO_ADS131A0X_SPI_CS_PIN)
#define AO_ADS131A0X_SPI_BUS		(AO_SPI_1_PE13_PE14_PE15 | AO_SPI_MODE_1)
#define AO_ADS131A0X_SPI_SPEED		AO_SPI_SPEED_8MHz

#define AO_ADS131A0X_DRDY_PORT		(&stm_gpiob)
#define AO_ADS131A0X_DRDY_PIN		10

#define AO_ADS131A0X_DONE_PORT		(&stm_gpiob)
#define AO_ADS131A0X_DONE_PIN		11

#define AO_ADS131A0X_RESET_PORT         (&stm_gpiob)
#define AO_ADS131A0X_RESET_PIN          12

#define AO_ADS131A0X_CHANNELS		4	/* how many inputs in use */

/*
 * SPI Flash memory
 */

#define M25_MAX_CHIPS           1
#define AO_M25_SPI_CS_PORT      (&stm_gpiob)
#define AO_M25_SPI_CS_PIN	9
#define AO_M25_SPI_CS_MASK      (1 << AO_M25_SPI_CS_PIN)
#define AO_M25_SPI_BUS          AO_SPI_2_PD1_PD3_PD4

/*
 * Radio is a cc1200 connected via SPI
 */

#define AO_RADIO_CAL_DEFAULT 	5695733

#define AO_FEC_DEBUG		0
#define AO_CC1200_SPI_CS_PORT	(&stm_gpiob)
#define AO_CC1200_SPI_CS_PIN	4
#define AO_CC1200_SPI_BUS	AO_SPI_2_PD1_PD3_PD4
#define AO_CC1200_SPI		stm_spi2

#define AO_CC1200_INT_PORT	(&stm_gpiob)
#define AO_CC1200_INT_PIN	5

#define AO_CC1200_INT_GPIO	2
#define AO_CC1200_INT_GPIO_IOCFG	CC1200_IOCFG2

#define HAS_LED			1

#define LED_0_PORT		(&stm_gpiod)
#define LED_0_PIN		0
#define AO_LED_RED		(1 << 0)

#define LED_1_PORT		(&stm_gpioa)
#define LED_1_PIN		15
#define AO_LED_AMBER		(1 << 1)

#define LED_2_PORT		(&stm_gpioh)
#define LED_2_PIN		2
#define AO_LED_GREEN		(1 << 2)

#define LED_3_PORT		(&stm_gpiob)
#define LED_3_PIN		13
#define AO_LED_ARMED		(1 << 3)

#define LED_4_PORT		(&stm_gpiob)
#define LED_4_PIN		14
#define AO_LED_CONTINUITY(c)	(1 << 4)

/* Alarm A */
#define AO_SIREN
#define AO_SIREN_PORT		(&stm_gpiod)
#define AO_SIREN_PIN		9

/* Alarm B */
#define AO_STROBE
#define AO_STROBE_PORT		(&stm_gpiod)
#define AO_STROBE_PIN		8

#define SPI_CONST	0x00

/*
 * ADC reference in decivolts
 *	actually, this is a lie!  the ADS131A04 is biased to 1/2 the 5V 
 *	rail, or 2.5V, so that it can swing 2.5 +/- 3.3 to cover 0-5V cleanly
 */
#define AO_ADC_REFERENCE_DV     33

#define AO_PAD_NUM		1
#define	AO_PAD_PORT		(&stm_gpiod)
#define AO_PAD_PIN_0		2

#define AO_PAD_ADC_0		0

#define AO_PAD_ALL_PINS		((1 << AO_PAD_PIN_0))
#define AO_PAD_ALL_CHANNELS	((1 << 0))

/* test these values with real igniters */
#define AO_PAD_RELAY_CLOSED	3524
#define AO_PAD_NO_IGNITER	16904
#define AO_PAD_GOOD_IGNITER	22514

#define AO_PAD_ADC_PYRO		1
#define AO_PAD_ADC_BATT		2

#define AO_ADC_FIRST_PIN	0

#define AO_NUM_ADC		3

#define AO_ADC_SQ1		AO_PAD_ADC_0
#define AO_ADC_SQ2		AO_PAD_ADC_PYRO
#define AO_ADC_SQ3		AO_PAD_ADC_BATT

#define AO_PAD_R_V_BATT_BATT_SENSE      200
#define AO_PAD_R_BATT_SENSE_GND         22

#define AO_PAD_R_V_BATT_V_PYRO          200
#define AO_PAD_R_V_PYRO_PYRO_SENSE      200
#define AO_PAD_R_PYRO_SENSE_GND         22

#undef AO_PAD_R_V_PYRO_IGNITER
#define AO_PAD_R_IGNITER_IGNITER_SENSE  200
#define AO_PAD_R_IGNITER_SENSE_GND      22

#define HAS_ADC_TEMP		0

struct ao_adc {
	int16_t		sense[AO_PAD_NUM];
	int16_t		pyro;
	int16_t		batt;
};

#define AO_ADC_DUMP(p)							\
	printf ("tick: %5u 0: %5d pyro: %5d batt %5d\n", \
		(p)->tick,						\
		(p)->adc.sense[0],					\
		(p)->adc.pyro,						\
		(p)->adc.batt)

#define AO_ADC_PINS	((1 << AO_PAD_ADC_0) | \
			 (1 << AO_PAD_ADC_PYRO) | \
			 (1 << AO_PAD_ADC_BATT))

#endif /* _AO_PINS_H_ */
