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

#define AO_XOSC			1
#define AO_XOSC_FREQ		16000000
#define AO_XOSC_DIV		256
#define AO_XOSC_MUL		768

#define AO_AHB_PRESCALER	1
#define AO_APBA_PRESCALER	1

#define AO_CONFIG_MAX_SIZE	1024

#define HAS_EEPROM		1
#define USE_INTERNAL_FLASH	0
#define USE_EEPROM_CONFIG	0
#define USE_STORAGE_CONFIG	1
#define HAS_USB			1
#define HAS_BEEP		1
#define HAS_BATTERY_REPORT	1
#define AO_BEEP_TCC		(&samd21_tcc2)
#define AO_BEEP_TCC_APBC_MASK	SAMD21_PM_APBCMASK_TCC2
#define AO_BEEP_PORT		(&samd21_port_a)
#define AO_BEEP_PIN		16
#define AO_BEEP_FUNC		SAMD21_PORT_PMUX_FUNC_E
#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_APRS		0
#define HAS_COMPANION		0

#define HAS_SPI_0		1
#define HAS_SPI_3		1
#define HAS_SPI_5		1

#define LEDS_AVAILABLE		0

#define HAS_GPS			0
#define HAS_FLIGHT		1
#define HAS_ADC			1
#define HAS_ADC_TEMP		1
#define HAS_LOG			1

/*
 * Igniter
 */

#define HAS_IGNITE		0
#define HAS_IGNITE_REPORT	1
#define AO_PYRO_NUM		2

#define AO_SENSE_PYRO(p,n)	((p)->adc.sense[n])
#define AO_IGNITER_CLOSED	400
#define AO_IGNITER_OPEN		60

/* Pyro A */
#define AO_PYRO_PORT_0  	(&samd21_port_a)
#define AO_PYRO_PIN_0   	1

#define AO_ADC_SENSE_A          0
#define AO_ADC_SENSE_A_PORT     (&samd21_port_a)
#define AO_ADC_SENSE_A_PIN      2

/* Pyro B */
#define AO_PYRO_PORT_1  	(&samd21_port_b)
#define AO_PYRO_PIN_1   	9

#define AO_ADC_SENSE_B          2
#define AO_ADC_SENSE_B_PORT     (&samd21_port_b)
#define AO_ADC_SENSE_B_PIN      8


/*
 * ADC
 */
#define AO_DATA_RING		32
#define AO_ADC_NUM_SENSE	2

struct ao_adc {
	int16_t			sense[AO_ADC_NUM_SENSE];
	int16_t			v_batt;
	int16_t			temp;
};

#define AO_ADC_DUMP(p) \
	printf("tick: %5lu A: %5d B: %5d batt: %5d\n", \
	       (p)->tick, \
               (p)->adc.sense[0], (p)->adc.sense[1], \
	       (p)->adc.v_batt);

#define AO_ADC_V_BATT		10
#define AO_ADC_V_BATT_PORT	(&samd21_port_b)
#define AO_ADC_V_BATT_PIN	2

#define AO_ADC_TEMP		SAMD21_ADC_INPUTCTRL_MUXPOS_TEMP

#define AO_NUM_ADC_PIN		(AO_ADC_NUM_SENSE + 1)

#define AO_ADC_PIN0_PORT        AO_ADC_SENSE_A_PORT
#define AO_ADC_PIN0_PIN         AO_ADC_SENSE_A_PIN
#define AO_ADC_PIN1_PORT        AO_ADC_SENSE_B_PORT
#define AO_ADC_PIN1_PIN         AO_ADC_SENSE_B_PIN
#define AO_ADC_PIN2_PORT	AO_ADC_V_BATT_PORT
#define AO_ADC_PIN2_PIN		AO_ADC_V_BATT_PIN

#define AO_NUM_ADC	       	(AO_NUM_ADC_PIN + 1)

#define AO_ADC_SQ0              AO_ADC_SENSE_A
#define AO_ADC_SQ1              AO_ADC_SENSE_B
#define AO_ADC_SQ2		AO_ADC_V_BATT
#define AO_ADC_SQ3		AO_ADC_TEMP

/*
 * Voltage divider on ADC battery sampler
 */
#define AO_BATTERY_DIV_PLUS	100	/* 100k */
#define AO_BATTERY_DIV_MINUS	27	/* 27k */

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
 * SPI Flash memory
 */

#define M25_MAX_CHIPS		1
#define AO_M25_SPI_CS_PORT	(&samd21_port_b)
#define AO_M25_SPI_CS_MASK	(1 << 10)
#define AO_M25_SPI_BUS		AO_SPI_0_PA04_PA05_PA06

/*
 *
 * Here are the required sensor signs:
 *
 * +along	nose up
 * +across 	switch screws down
 * +through	TH down
 *
 * With the board aligned to have positive accel for the relevant
 * axis, looking down from above we have:
 *
 * +roll	counter clockwise (nose up)
 * +pitch	counter clockwise (switch screws down)
 * +yaw		counter clockwise (TH down)
 */


/*
 * On EasyTimer v2, bmi088 pin 1 (NE corner of chip) is placed away
 * from the USB edge of the board. Relative to bmi088 specs, to get
 * the above values, we need to flip the X and Y axes, assigning
 * values as follows:
 *
 *	+along		-Y	+roll	-Y
 *	+across		-X	+pitch	-X
 *	+through	+Z	+yaw	+Z
 */

#define HAS_BMI088		1
#define AO_BMI088_SPI_BUS	AO_SPI_5_PB22_PB23_PB03
#define AO_BMI088_ACC_CS_PORT	(&samd21_port_a)
#define AO_BMI088_ACC_CS_PIN	10
#define AO_BMI088_GYR_CS_PORT	(&samd21_port_a)
#define AO_BMI088_GYR_CS_PIN	11
#define HAS_IMU			1

#define ao_bmi088_along(m)	(-(m)->acc.y)
#define ao_bmi088_across(m)	(-(m)->acc.x)
#define ao_bmi088_through(m)	((m)->acc.z)

#define ao_bmi088_roll(m)	(-(m)->gyr.y)
#define ao_bmi088_pitch(m)	(-(m)->gyr.x)
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
 *	+along		+Y
 *	+across		+X
 *	+through	-Z
 */

#define HAS_MMC5983			1
#define AO_MMC5983_INT_PORT		(&samd21_port_a)
#define AO_MMC5983_INT_PIN		9
#define AO_MMC5983_SPI_CLK_PORT		(&samd21_port_a)
#define AO_MMC5983_SPI_CLK_PIN		23
#define AO_MMC5983_SPI_MISO_PORT	(&samd21_port_a)
#define AO_MMC5983_SPI_MISO_PIN		20
#define AO_MMC5983_SPI_MOSI_PORT	(&samd21_port_a)
#define AO_MMC5983_SPI_MOSI_PIN		22
#define AO_MMC5983_SPI_INDEX		(AO_SPI_3_PA22_PA23_PA20 | AO_SPI_MODE_3)
#define AO_MMC5983_SPI_CS_PORT		(&samd21_port_a)
#define AO_MMC5983_SPI_CS_PIN		8

#define ao_mmc5983_along(m)		((m)->y)
#define ao_mmc5983_across(m)		((m)->x)
#define ao_mmc5983_through(m)		(-(m)->z)

#define ao_data_mag_along(packet)	ao_mmc5983_along(&(packet)->mmc5983)
#define ao_data_mag_across(packet)	ao_mmc5983_across(&(packet)->mmc5983)
#define ao_data_mag_through(packet)	ao_mmc5983_through(&(packet)->mmc5983)

/*
 * Monitor
 */

#define HAS_MONITOR		0
#define LEGACY_MONITOR		0
#define HAS_MONITOR_PUT		1
#define AO_MONITOR_LED		0
#define HAS_RSSI		0

/*
 * Logging
 */

#define AO_CONFIG_DEFAULT_FLIGHT_LOG_MAX	(1024 * 1024)
#define AO_CONFIG_MAX_SIZE			1024
#define LOG_ERASE_MARK				0x55
#define LOG_MAX_ERASE				128
#define AO_LOG_FORMAT				AO_LOG_FORMAT_EASYTIMER_2
#define AO_LOG_NORMALIZED			1

#endif /* _AO_PINS_H_ */
