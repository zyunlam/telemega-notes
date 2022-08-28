/*
 * Copyright © 2022 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
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

#ifndef _AO_BMI088_H_
#define _AO_BMI088_H_

#include <math.h>

struct ao_bmi088_acc_sample {
	int16_t		x;
	int16_t		y;
	int16_t		z;
};

struct ao_bmi088_gyr_sample {
	int16_t		x;
	int16_t		y;
	int16_t		z;
};

struct ao_bmi088_sample {
	struct ao_bmi088_acc_sample acc;
	struct ao_bmi088_gyr_sample gyr;
};

extern struct ao_bmi088_sample ao_bmi088_current;

void
ao_bmi088_init(void);

#define BMI088_ACC_CHIP_ID	0x00
#define  BMI088_ACC_CHIP_ID_BMI088	0x1e

#define BMI088_ACC_ERR_REG	0x02
# define BMI088_ACC_ERR_REG_ERROR_CODE	2
# define BMI088_ACC_ERR_REG_FATAL_ERR	0

#define BMI088_ACC_STATUS	0x03
# define BMI088_ACC_STATUS_ACC_DRDY	7

#define BMI088_ACC_DATA		0x12
#define BMI088_ACC_TIME		0x18

#define BMI088_ACC_INT_STAT_1	0x1d
# define BMI088_ACC_INT_STAT_1_ACC_DRDY	7

#define BMI088_ACC_TEMP		0x22

#define BMI088_ACC_CONF		0x40
# define BMI088_ACC_CONF_BWP		4
# define BMI088_ACC_CONF_BWP_OSR4		0x08
# define BMI088_ACC_CONF_BWP_OSR2		0x08
# define BMI088_ACC_CONF_BWP_NORMAL		0x0a
# define BMI088_ACC_CONF_ODR		0
# define BMI088_ACC_CONF_ODR_12_5		0x05
# define BMI088_ACC_CONF_ODR_25			0x06
# define BMI088_ACC_CONF_ODR_50			0x07
# define BMI088_ACC_CONF_ODR_100		0x08
# define BMI088_ACC_CONF_ODR_200		0x09
# define BMI088_ACC_CONF_ODR_400		0x0a
# define BMI088_ACC_CONF_ODR_800		0x0b
# define BMI088_ACC_CONF_ODR_1600		0x0c

#define BMI088_ACC_RANGE	0x41
# define BMI088_ACC_RANGE_3			0x00
# define BMI088_ACC_RANGE_6			0x01
# define BMI088_ACC_RANGE_12			0x02
# define BMI088_ACC_RANGE_24			0x03

#define BMI088_INT1_IO_CONF	0x53
# define BMI088_INT1_IO_CONF_INT1_IN	4
# define BMI088_INT1_IO_CONF_INT1_OUT	3
# define BMI088_INT1_IO_CONF_INT1_OD	2
# define BMI088_INT1_IO_CONF_INT1_LVL	1

#define BMI088_INT2_IO_CONF	0x54
# define BMI088_INT2_IO_CONF_INT2_IN	4
# define BMI088_INT2_IO_CONF_INT2_OUT	3
# define BMI088_INT2_IO_CONF_INT2_OD	2
# define BMI088_INT2_IO_CONF_INT2_LVL	1

#define BMI088_INT2_INT2_MAP_DATA	0x58
# define BMI088_INT2_INT2_MAP_DATA_INT2_DRDY	6
# define BMI088_INT2_INT2_MAP_DATA_INT1_DRDY	2

#define BMI088_ACC_SELF_TEST	0x6d
# define BMI088_ACC_SELF_TEST_OFF	0x00
# define BMI088_ACC_SELF_TEST_POSITIVE	0x0d
# define BMI088_ACC_SELF_TEST_NEGATIVE	0x09

#define BMI088_ACC_PWR_CONF	0x7c
# define BMI088_ACC_PWR_CONF_SUSPEND	0x03
# define BMI088_ACC_PWR_CONF_ACTIVE	0x00

#define BMI088_ACC_PWR_CTRL	0x7d
# define BMI088_ACC_PWR_CTRL_OFF	0x00
# define BMI088_ACC_PWR_CTRL_ON		0x04

#define BMI088_ACC_SOFTRESET	0x7e
# define BMI088_ACC_SOFTRESET_RESET	0xb6

#define BMI088_GYRO_CHIP_ID	0x00
# define BMI088_GYRO_CHIP_ID_BMI088	0x0f

#define BMI088_GYRO_DATA	0x02
#define BMI088_GYRO_DATA_X	0x02
#define BMI088_GYRO_DATA_Y	0x04
#define BMI088_GYRO_DATA_Z	0x06

#define BMI088_GYRO_INT_STAT_1	0x0a
# define BMI088_GYRO_INT_STAT_1_GYRO_DRDY	7

#define BMI088_GYRO_RANGE	0x0f
# define BMI088_GYRO_RANGE_2000		0x00
# define BMI088_GYRO_RANGE_1000		0x01
# define BMI088_GYRO_RANGE_500		0x02
# define BMI088_GYRO_RANGE_250		0x03
# define BMI088_GYRO_RANGE_125		0x04

#define BMI088_GYRO_BANDWIDTH	0x10
# define BMI088_GYRO_BANDWIDTH_2000_532	0x00
# define BMI088_GYRO_BANDWIDTH_2000_230	0x01
# define BMI088_GYRO_BANDWIDTH_1000_116	0x02
# define BMI088_GYRO_BANDWIDTH_400_47	0x03
# define BMI088_GYRO_BANDWIDTH_200_23	0x04
# define BMI088_GYRO_BANDWIDTH_100_12	0x05
# define BMI088_GYRO_BANDWIDTH_200_64	0x06
# define BMI088_GYRO_BANDWIDTH_100_32	0x07

#define BMI088_GYRO_LPM1	0x11
# define BMI088_GYRO_LPM1_NORMAL	0x00
# define BMI088_GYRO_LPM1_SUSPEND	0x80
# define BMI088_GYRO_LPM1_DEEP_SUSPEND	0x20

#define BMI088_GYRO_SOFTRESET	0x14
# define BMI088_GYRO_SOFTRESET_RESET	0xb6

#define BMI088_GYRO_INT_CTRL	0x15
# define BMI088_GYRO_INT_CTRL_DISABLE	0x00
# define BMI088_GYRO_INT_CTRL_ENABLE	0x80

#define BMI088_INT3_INT4_IO_CONF	0x16
# define BMI088_INT3_INT4_IO_CONF_INT4_OD	3
# define BMI088_INT3_INT4_IO_CONF_INT4_LVL	2
# define BMI088_INT3_INT4_IO_CONF_INT3_OD	1
# define BMI088_INT3_INT4_IO_CONF_INT3_LVL	0

#define BMI088_INT3_INT4_IO_MAP		0x18
# define BMI088_INT3_INT4_IO_MAP_NONE		0x00
# define BMI088_INT3_INT4_IO_MAP_INT3		0x01
# define BMI088_INT3_INT4_IO_MAP_INT4		0x80
# define BMI088_INT3_INT4_IO_MAP_INT3_INT4	0x81

#define BMI088_GYRO_SELF_TEST	0x3c
# define BMI088_GYRO_SELF_TEST_RATE_OK		4
# define BMI088_GYRO_SELF_TEST_BIST_FAIL	2
# define BMI088_GYRO_SELF_TEST_BIST_RDY		1
# define BMI088_GYRO_SELF_TEST_TRIG_BIST	0

#define BMI088_GYRO_FULLSCALE	((float) 2000.0f * (float) M_PI / 180.0f)

static inline float
ao_bmi088_gyro(float sensor) {
	return sensor * ((float) (BMI088_GYRO_FULLSCALE / 32767.0));
}

#define ao_bmi_gyro_to_sample(gyro) ((gyro) * (32767.0f / (BMI088_GYRO_FULLSCALE

#define BMI088_ACCEL_FULLSCALE	24

static inline float
ao_bmi088_accel(int16_t sensor) {
	return (float) sensor * ((float) (BMI088_ACCEL_FULLSCALE * GRAVITY / 32767.0));
}

#define ao_bmi_accel_to_sample(accel) ((accel) * (32767.0f / (BMI088_ACCEL_FULLSCALE * GRAVITY)))

#endif /* _AO_BMI088_H_ */
