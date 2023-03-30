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

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

/* 16MHz crystal */

#define AO_HSE		1
#define AO_HSE_BYPASS	0

#define AO_SYSCLK	72000000
#define AO_HCLK		72000000
#define AO_APB1CLK	36000000
#define AO_APB2CLK	72000000
#define AO_ADCCLK	12000000

/* PLLMUL is 9, PLLXTPRE (pre divider) is 2, so the
 * overall PLLCLK is 16 * 9/2 = 72MHz (used as SYSCLK)
 *
 * HCLK is SYSCLK / 1 (HPRE_DIV) = 72MHz (72MHz max)
 * USB is PLLCLK / 1.5 (USBPRE)= 48MHz (must be 48MHz)
 * APB2 is HCLK / 1 (PPRE2_DIV) = 72MHz (72MHz max)
 * APB1 is HCLK / 2 (PPRE1_DIV) = 36MHz (36MHz max)
 * ADC is APB2 / 6 (ADCPRE) = 12MHz (14MHz max)
 */

#define AO_RCC_CFGR_USBPRE	STM_RCC_CFGR_USBPRE_1_5
#define AO_RCC_CFGR_PLLMUL	STM_RCC_CFGR_PLLMUL_9
#define AO_RCC_CFGR_PLLXTPRE	STM_RCC_CFGR_PLLXTPRE_2
#define AO_RCC_CFGR_PPRE2_DIV	STM_RCC_CFGR_PPRE2_DIV_1
#define AO_RCC_CFGR_PPRE1_DIV	STM_RCC_CFGR_PPRE1_DIV_2
#define AO_RCC_CFGR_HPRE_DIV	STM_RCC_CFGR_HPRE_DIV_1
#define AO_RCC_CFGR_ADCPRE	STM_RCC_CFGR_ADCPRE_6

#define AO_CONFIG_DEFAULT_FLIGHT_LOG_MAX	(1024 * 1024)
#define AO_CONFIG_MAX_SIZE                     1024
#define LOG_ERASE_MARK                         0x55
#define LOG_MAX_ERASE                          128
#define AO_LOG_FORMAT				AO_LOG_FORMAT_EASYMEGA_3
#define AO_LOG_NORMALIZED			1

#define HAS_EEPROM		1
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	0
#define USE_STORAGE_CONFIG	1
#define HAS_USB			1
#define HAS_BEEP		1
#define BEEPER_TIMER		2
#define BEEPER_CHANNEL		3
#define BEEPER_PORT		(&stm_gpioa)
#define BEEPER_PIN		2
#define HAS_BATTERY_REPORT	1
#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_APRS		0
#define HAS_COMPANION		1

#define HAS_USB_PULLUP	1
#define AO_USB_PULLUP_PORT	(&stm_gpioa)
#define AO_USB_PULLUP_PIN	8

#define HAS_SPI_1		1
#define SPI_1_PA5_PA6_PA7	1	/* Barometer */
#define SPI_1_PB3_PB4_PB5	1	/* Accelerometer */
#define SPI_1_PE13_PE14_PE15	0
#define SPI_1_MODE_OUTPUT	STM_GPIO_CR_MODE_OUTPUT_10MHZ

#define HAS_SPI_2		1
#define SPI_2_PB13_PB14_PB15	1	/* Flash, IMU, Companion */
#define SPI_2_MODE_OUTPUT	STM_GPIO_CR_MODE_OUTPUT_50MHZ

#define HAS_I2C_1		1
#define I2C_1_PB8_PB9		1

#define HAS_I2C_2		0
#define I2C_2_PB10_PB11		0

#define PACKET_HAS_SLAVE	0
#define PACKET_HAS_MASTER	0

#define LOW_LEVEL_DEBUG		0

#define LED_0_PORT		(&stm_gpioa)
#define LED_0_PIN		9
#define LED_1_PORT		(&stm_gpioa)
#define LED_1_PIN		10
#define AO_LED_RED		(1 << LED_0_PIN)
#define AO_LED_GREEN		(1 << LED_1_PIN)

#define LEDS_AVAILABLE		(AO_LED_RED | AO_LED_GREEN)

#define HAS_GPS			0
#define HAS_FLIGHT		1
#define HAS_ADC			1
#define HAS_ADC_TEMP		1
#define HAS_LOG			1

/*
 * Igniter
 */

#define HAS_IGNITE		1
#define HAS_IGNITE_REPORT	1

#define AO_SENSE_PYRO(p,n)	((p)->adc.sense[n])
#define AO_SENSE_DROGUE(p)	((p)->adc.sense[4])
#define AO_SENSE_MAIN(p)	((p)->adc.sense[5])
#define AO_IGNITER_CLOSED	400
#define AO_IGNITER_OPEN		60

/* Pyro A */
#define AO_PYRO_PORT_0	(&stm_gpioa)
#define AO_PYRO_PIN_0	15

/* Pyro B */
#define AO_PYRO_PORT_1	(&stm_gpioc)
#define AO_PYRO_PIN_1	10

/* Pyro C */
#define AO_PYRO_PORT_2	(&stm_gpiob)
#define AO_PYRO_PIN_2	11

/* Pyro D */
#define AO_PYRO_PORT_3	(&stm_gpiob)
#define AO_PYRO_PIN_3	10

/* Drogue */
#define AO_IGNITER_DROGUE_PORT	(&stm_gpioa)
#define AO_IGNITER_DROGUE_PIN	0

/* Main */
#define AO_IGNITER_MAIN_PORT	(&stm_gpioa)
#define AO_IGNITER_MAIN_PIN	1

/* Number of general purpose pyro channels available */
#define AO_PYRO_NUM	4

/*
 * ADC
 */
#define AO_DATA_RING		32
#define AO_ADC_NUM_SENSE	6

struct ao_adc {
	int16_t			sense[AO_ADC_NUM_SENSE];
	int16_t			v_batt;
	int16_t			v_pbatt;
	int16_t			temp;
};

#define AO_ADC_DUMP(p) \
	printf("tick: %5lu A: %5d B: %5d C: %5d D: %5d drogue: %5d main: %5d batt: %5d pbatt: %5d temp: %5d\n", \
	       (p)->tick, \
	       (p)->adc.sense[0], (p)->adc.sense[1], (p)->adc.sense[2], \
	       (p)->adc.sense[3], (p)->adc.sense[4], (p)->adc.sense[5], \
	       (p)->adc.v_batt, (p)->adc.v_pbatt, (p)->adc.temp)

#define AO_ADC_SENSE_A		14
#define AO_ADC_SENSE_A_PORT	(&stm_gpioc)
#define AO_ADC_SENSE_A_PIN	4

#define AO_ADC_SENSE_B		15
#define AO_ADC_SENSE_B_PORT	(&stm_gpioc)
#define AO_ADC_SENSE_B_PIN	5

#define AO_ADC_SENSE_C		13
#define AO_ADC_SENSE_C_PORT	(&stm_gpioc)
#define AO_ADC_SENSE_C_PIN	3

#define AO_ADC_SENSE_D		12
#define AO_ADC_SENSE_D_PORT	(&stm_gpioc)
#define AO_ADC_SENSE_D_PIN	2

#define AO_ADC_SENSE_DROGUE	11
#define AO_ADC_SENSE_DROGUE_PORT	(&stm_gpioc)
#define AO_ADC_SENSE_DROGUE_PIN	1

#define AO_ADC_SENSE_MAIN	10
#define AO_ADC_SENSE_MAIN_PORT	(&stm_gpioc)
#define AO_ADC_SENSE_MAIN_PIN	0

#define AO_ADC_V_BATT		8
#define AO_ADC_V_BATT_PORT	(&stm_gpiob)
#define AO_ADC_V_BATT_PIN	0

#define AO_ADC_V_PBATT		9
#define AO_ADC_V_PBATT_PORT	(&stm_gpiob)
#define AO_ADC_V_PBATT_PIN	1

#define AO_ADC_TEMP		16

#define AO_NUM_ADC_PIN		(AO_ADC_NUM_SENSE + 2)

#define AO_ADC_PIN0_PORT	AO_ADC_SENSE_A_PORT
#define AO_ADC_PIN0_PIN		AO_ADC_SENSE_A_PIN
#define AO_ADC_PIN1_PORT	AO_ADC_SENSE_B_PORT
#define AO_ADC_PIN1_PIN		AO_ADC_SENSE_B_PIN
#define AO_ADC_PIN2_PORT	AO_ADC_SENSE_C_PORT
#define AO_ADC_PIN2_PIN		AO_ADC_SENSE_C_PIN
#define AO_ADC_PIN3_PORT	AO_ADC_SENSE_D_PORT
#define AO_ADC_PIN3_PIN		AO_ADC_SENSE_D_PIN
#define AO_ADC_PIN4_PORT	AO_ADC_SENSE_DROGUE_PORT
#define AO_ADC_PIN4_PIN		AO_ADC_SENSE_DROGUE_PIN
#define AO_ADC_PIN5_PORT	AO_ADC_SENSE_MAIN_PORT
#define AO_ADC_PIN5_PIN		AO_ADC_SENSE_MAIN_PIN
#define AO_ADC_PIN6_PORT	AO_ADC_V_BATT_PORT
#define AO_ADC_PIN6_PIN		AO_ADC_V_BATT_PIN
#define AO_ADC_PIN7_PORT	AO_ADC_V_PBATT_PORT
#define AO_ADC_PIN7_PIN		AO_ADC_V_PBATT_PIN

#define AO_NUM_ADC	       	(AO_ADC_NUM_SENSE + 3)

#define AO_ADC_SQ1		AO_ADC_SENSE_A
#define AO_ADC_SQ2		AO_ADC_SENSE_B
#define AO_ADC_SQ3		AO_ADC_SENSE_C
#define AO_ADC_SQ4		AO_ADC_SENSE_D
#define AO_ADC_SQ5		AO_ADC_SENSE_DROGUE
#define AO_ADC_SQ6		AO_ADC_SENSE_MAIN
#define AO_ADC_SQ7		AO_ADC_V_BATT
#define AO_ADC_SQ8		AO_ADC_V_PBATT
#define AO_ADC_SQ9		AO_ADC_TEMP

/*
 * Voltage divider on ADC battery sampler
 */
#define AO_BATTERY_DIV_PLUS	56	/* 5.6k */
#define AO_BATTERY_DIV_MINUS	100	/* 10k */

/*
 * Voltage divider on ADC pyro battery sampler
 */
#define AO_PYRO_BATTERY_DIV_PLUS	100	/* 100k */
#define AO_PYRO_BATTERY_DIV_MINUS	27	/* 27k */

/*
 * Voltage divider on ADC igniter samplers
 */
#define AO_IGNITE_DIV_PLUS	100	/* 100k */
#define AO_IGNITE_DIV_MINUS	27	/* 27k */

/*
 * ADC reference in decivolts
 */
#define AO_ADC_REFERENCE_DV	33

/*
 * Pressure sensor settings
 */
#define HAS_MS5607		1
#define HAS_MS5611		0
#define AO_MS5607_PRIVATE_PINS	1
#define AO_MS5607_CS_PORT	(&stm_gpioa)
#define AO_MS5607_CS_PIN	3
#define AO_MS5607_CS_MASK	(1 << AO_MS5607_CS_PIN)
#define AO_MS5607_MISO_PORT	(&stm_gpioa)
#define AO_MS5607_MISO_PIN	6
#define AO_MS5607_MISO_MASK	(1 << AO_MS5607_MISO_PIN)
#define AO_MS5607_SPI_INDEX	AO_SPI_1_PA5_PA6_PA7

/*
 * SPI Flash memory
 */

#define M25_MAX_CHIPS		1
#define AO_M25_SPI_CS_PORT	(&stm_gpiob)
#define AO_M25_SPI_CS_PIN	12
#define AO_M25_SPI_CS_MASK	(1 << AO_M25_SPI_CS_PIN)
#define AO_M25_SPI_BUS		AO_SPI_2_PB13_PB14_PB15

/* BMI088 */

#define HAS_BMI088		1
#define AO_BMI088_SPI_BUS	(AO_SPI_2_PB13_PB14_PB15 | AO_SPI_MODE_0)
#define AO_BMI088_ACC_CS_PORT	(&stm_gpioc)
#define AO_BMI088_ACC_CS_PIN	14
#define AO_BMI088_GYR_CS_PORT	(&stm_gpioc)
#define AO_BMI088_GYR_CS_PIN	13
#define HAS_IMU			1

#define ao_bmi088_along(m)	((m)->acc.x)
#define ao_bmi088_across(m)	(-(m)->acc.y)
#define ao_bmi088_through(m)	((m)->acc.z)

#define ao_bmi088_roll(m)	((m)->gyr.x)
#define ao_bmi088_pitch(m)	(-(m)->gyr.y)
#define ao_bmi088_yaw(m)	((m)->gyr.z)

#define ao_data_along(packet)	ao_bmi088_along(&(packet)->bmi088)
#define ao_data_across(packet)	ao_bmi088_across(&(packet)->bmi088)
#define ao_data_through(packet)	ao_bmi088_through(&(packet)->bmi088)

#define ao_data_roll(packet)	ao_bmi088_roll(&(packet)->bmi088)
#define ao_data_pitch(packet)	ao_bmi088_pitch(&(packet)->bmi088)
#define ao_data_yaw(packet)	ao_bmi088_yaw(&(packet)->bmi088)

/*
 * MMC5983
 *
 *	pin 1 NE corner of chip
 *
 *	+along		-Y
 *	+across		+X
 *	+through	-Z
 */

#define HAS_MMC5983		1
#define MMC5983_I2C		1
#define AO_MMC5983_I2C_INDEX	STM_I2C_INDEX(1)

#define ao_mmc5983_along(m)		(-(m)->y)
#define ao_mmc5983_across(m)		((m)->x)
#define ao_mmc5983_through(m)		(-(m)->z)

#define ao_data_mag_along(packet)	ao_mmc5983_along(&(packet)->mmc5983)
#define ao_data_mag_across(packet)	ao_mmc5983_across(&(packet)->mmc5983)
#define ao_data_mag_through(packet)	ao_mmc5983_through(&(packet)->mmc5983)

/* ADXL375 */

#define HAS_ADXL375		1
#define AO_ADXL375_SPI_INDEX	(AO_SPI_1_PB3_PB4_PB5 | AO_SPI_MODE_3)
#define AO_ADXL375_CS_PORT	(&stm_gpioc)
#define AO_ADXL375_CS_PIN	12

#define AO_ADXL375_INT1_PORT	(&stm_gpiob)
#define AO_ADXL375_INT1_PIN	8

#define AO_ADXL375_INT2_PORT	(&stm_gpiob)
#define AO_ADXL375_INT2_PIN	9

#define AO_ADXL375_AXIS		x
#define AO_ADXL375_INVERT	1

#define NUM_CMDS		16

/*
 * Companion
 */

#define AO_COMPANION_CS_PORT	(&stm_gpiob)
#define AO_COMPANION_CS_PIN	(6)
#define AO_COMPANION_SPI_BUS	AO_SPI_2_PB13_PB14_PB15

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

#endif /* _AO_PINS_H_ */
