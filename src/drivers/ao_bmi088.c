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

#include <ao.h>
#include <ao_bmi088.h>
#include <ao_data.h>

#define AO_BMI088_SPI_SPEED	ao_spi_speed(100000)

#define ao_bmi088_spi_get()	ao_spi_get(AO_BMI088_SPI_BUS, AO_BMI088_SPI_SPEED)
#define ao_bmi088_spi_put()	ao_spi_put(AO_BMI088_SPI_BUS)

#define ao_bmi088_acc_start()	ao_spi_set_cs(AO_BMI088_ACC_CS_PORT,	\
					      (1 << AO_BMI088_ACC_CS_PIN))
#define ao_bmi088_acc_end()	ao_spi_clr_cs(AO_BMI088_ACC_CS_PORT,	\
					      (1 << AO_BMI088_ACC_CS_PIN))
#define ao_bmi088_gyr_start()	ao_spi_set_cs(AO_BMI088_GYR_CS_PORT,	\
					      (1 << AO_BMI088_GYR_CS_PIN))
#define ao_bmi088_gyr_end()	ao_spi_clr_cs(AO_BMI088_GYR_CS_PORT,	\
					      (1 << AO_BMI088_GYR_CS_PIN))

static uint8_t
_ao_bmi088_acc_reg_read(uint8_t addr)
{
	uint8_t	v[2];
	addr |= 0x80;
	ao_bmi088_acc_start();
	ao_spi_send(&addr, 1, AO_BMI088_SPI_BUS);
	ao_spi_recv(v, 2, AO_BMI088_SPI_BUS);	/* part sends garbage for first byte */
	ao_bmi088_acc_end();
	return v[1];
}

static void
_ao_bmi088_acc_reg_write(uint8_t addr, uint8_t value)
{
	uint8_t	d[2] = { addr, value };
	ao_bmi088_acc_start();
	ao_spi_send(d, 2, AO_BMI088_SPI_BUS);
	ao_bmi088_acc_end();
}

static void
_ao_bmi088_acc_sample_read(struct ao_bmi088_acc_sample *sample)
{
	uint8_t dummy;
	uint8_t addr = BMI088_ACC_DATA | 0x80;

	ao_bmi088_acc_start();
	ao_spi_send(&addr, 1, AO_BMI088_SPI_BUS);
	ao_spi_recv(&dummy, 1, AO_BMI088_SPI_BUS);	/* part sends garbage for first byte */
	ao_spi_recv(sample, sizeof(struct ao_bmi088_acc_sample), AO_BMI088_SPI_BUS);
	ao_bmi088_acc_end();
}

static void
_ao_bmi088_gyr_read(uint8_t addr, void *data, uint8_t len)
{
	addr |= 0x80;
	ao_bmi088_gyr_start();
	ao_spi_send(&addr, 1, AO_BMI088_SPI_BUS);
	ao_spi_recv(data, len, AO_BMI088_SPI_BUS);
	ao_bmi088_gyr_end();
}

static uint8_t
_ao_bmi088_gyr_reg_read(uint8_t addr)
{
	uint8_t	v;
	_ao_bmi088_gyr_read(addr, &v, 1);
	return v;
}

static void
_ao_bmi088_gyr_reg_write(uint8_t addr, uint8_t value)
{
	uint8_t	d[2] = { addr, value };
	ao_bmi088_gyr_start();
	ao_spi_send(d, 2, AO_BMI088_SPI_BUS);
	ao_bmi088_gyr_end();
}

static void
_ao_bmi088_gyr_sample_read(struct ao_bmi088_gyr_sample *sample)
{
	_ao_bmi088_gyr_read(BMI088_GYRO_DATA, sample, sizeof (struct ao_bmi088_gyr_sample));
}

static void
ao_bmi088_reset(void)
{
	ao_bmi088_spi_get();

	/* reset the two devices */
	_ao_bmi088_acc_reg_write(BMI088_ACC_SOFTRESET, BMI088_ACC_SOFTRESET_RESET);
	_ao_bmi088_gyr_reg_write(BMI088_GYRO_SOFTRESET, BMI088_GYRO_SOFTRESET_RESET);

	/* wait 30ms (that's how long the gyro takes */
	ao_delay(AO_MS_TO_TICKS(30));

	/* force acc part to SPI mode */
	ao_bmi088_acc_start();

	ao_delay(AO_MS_TO_TICKS(1));

	ao_bmi088_acc_end();

	ao_bmi088_spi_put();
}

static bool
ao_bmi088_accel_good(int16_t pos, int16_t neg, int32_t good)
{
	int32_t	diff = (int32_t) pos - (int32_t) neg;

	return diff >= good;
}

static void
ao_bmi088_setup(void)
{
	bool working = false;
	struct ao_bmi088_acc_sample	acc_pos, acc_neg;

	ao_bmi088_reset();

	ao_bmi088_spi_get();

	/* Make sure the two devices are alive */
	if (_ao_bmi088_acc_reg_read(BMI088_ACC_CHIP_ID) != BMI088_ACC_CHIP_ID_BMI088)
		goto failure;

	if (_ao_bmi088_gyr_reg_read(BMI088_GYRO_CHIP_ID) != BMI088_GYRO_CHIP_ID_BMI088)
		goto failure;

	/* Turn on the accelerometer */

	_ao_bmi088_acc_reg_write(BMI088_ACC_PWR_CTRL, BMI088_ACC_PWR_CTRL_ON);

	/* Wait 5ms after changing accel power mode */
	ao_delay(AO_MS_TO_TICKS(5));

	/* Accel self test. Procedure:
	 *
	 * 1) Set ±24g range by writing 0x03 to register ACC_RANGE (0x41)
         * 2) Set ODR=1.6kHz, continuous sampling mode, “normal mode” (norm_avg4) by writing 0xA7 to
         * register ACC_CONF (0x40)
         * • Continuous filter function: set bit7 in ACC_CONF
         * • “normal avg4 mode”: ACC_CONF |= 0x02<<4
         * • ODR=1.6kHz: ACC_CONF |= 0x0C
         * 3) Wait for > 2 ms
         * 4) Enable the positive self-test polarity (i.e. write 0x0D to register ACC_SELF_TEST (0x6D))
         * 5) Wait for > 50ms
         * 6) Read the accelerometer offset values for each axis (positive self-test response)
         * 7) Enable the negative self-test polarity (i.e. write 0x09 to register ACC_SELF_TEST (0x6D))
         * 8) Wait for > 50ms
         * 9) Read the accelerometer offset values for each axis (negative self-test response)
         * 10) Disable the self-test (i.e. write 0x00 to register ACC_SELF_TEST (0x6D))
         * 11) Calculate difference of positive and negative self-test response and compare with the expected
         * values (see table below)
         * 12) Wait for > 50ms to let the sensor settle to normal mode steady state operation
	 */

	_ao_bmi088_acc_reg_write(BMI088_ACC_RANGE, BMI088_ACC_RANGE_24);

	_ao_bmi088_acc_reg_write(BMI088_ACC_CONF,
				 (BMI088_ACC_CONF_BWP_NORMAL << BMI088_ACC_CONF_BWP) |
				 (BMI088_ACC_CONF_ODR_1600 << BMI088_ACC_CONF_ODR));

	ao_delay(AO_MS_TO_TICKS(2));

	_ao_bmi088_acc_reg_write(BMI088_ACC_SELF_TEST, BMI088_ACC_SELF_TEST_POSITIVE);

	ao_delay(AO_MS_TO_TICKS(50));

	_ao_bmi088_acc_sample_read(&acc_pos);

	_ao_bmi088_acc_reg_write(BMI088_ACC_SELF_TEST, BMI088_ACC_SELF_TEST_NEGATIVE);

	ao_delay(AO_MS_TO_TICKS(50));

	_ao_bmi088_acc_sample_read(&acc_neg);

	_ao_bmi088_acc_reg_write(BMI088_ACC_SELF_TEST, BMI088_ACC_SELF_TEST_OFF);

	/* Self test X and Y must show at least 1000 mg difference,
	 * Z must show at least 500mg difference
	 */
	if (!ao_bmi088_accel_good(acc_pos.x, acc_neg.x, (int32_t) ao_bmi_accel_to_sample(GRAVITY)))
		goto failure;

	if (!ao_bmi088_accel_good(acc_pos.y, acc_neg.y, (int32_t) ao_bmi_accel_to_sample(GRAVITY)))
		goto failure;

	if (!ao_bmi088_accel_good(acc_pos.z, acc_neg.z, (int32_t) ao_bmi_accel_to_sample(GRAVITY/2)))
		goto failure;

	/*
	 * Self-test gyro
	 *
	 * To trigger the self-test, bit #0 (‘bite_trig’) in address
	 * GYRO_SELF_TEST must be set. When the test is finished, bit
	 * #1 (‘bist_rdy’) will be set by the gyro and the test result
	 * can then be found in bit #2 (‘bist_fail’).  A ‘0’ indicates
	 * that the test was passed without issues. If a failure
	 * occurred, the bit ‘bist_fail’ will be set to ‘1’.
	 */

	_ao_bmi088_gyr_reg_write(BMI088_GYRO_SELF_TEST,
				 (1 << BMI088_GYRO_SELF_TEST_TRIG_BIST));

	uint8_t gyro_self_test;
	for(;;) {
		gyro_self_test = _ao_bmi088_gyr_reg_read(BMI088_GYRO_SELF_TEST);
		if ((gyro_self_test & (1 << BMI088_GYRO_SELF_TEST_BIST_RDY)) != 0)
			break;
	}
	if ((gyro_self_test & (1 << BMI088_GYRO_SELF_TEST_BIST_FAIL)) != 0)
		goto failure;


	/* Put accel in desired mode */

	/* 200 Hz sampling rate, normal filter */
	_ao_bmi088_acc_reg_write(BMI088_ACC_CONF,
				 (BMI088_ACC_CONF_BWP_NORMAL << BMI088_ACC_CONF_BWP) |
				 (BMI088_ACC_CONF_ODR_200 << BMI088_ACC_CONF_ODR));


	/* 24 g range */
	_ao_bmi088_acc_reg_write(BMI088_ACC_RANGE, BMI088_ACC_RANGE_24);

	/* Put gyro in desired mode */

	/* 2000°/s range */
	_ao_bmi088_gyr_reg_write(BMI088_GYRO_RANGE, BMI088_GYRO_RANGE_2000);

	/* 200 Hz sampling rate, 64Hz filter */
	_ao_bmi088_gyr_reg_write(BMI088_GYRO_BANDWIDTH, BMI088_GYRO_BANDWIDTH_200_64);

	working = true;
failure:
	if (!working)
		AO_SENSOR_ERROR(AO_DATA_BMI088);

	ao_bmi088_spi_put();
}

struct ao_bmi088_sample	ao_bmi088_current;

static void
ao_bmi088(void)
{
	struct ao_bmi088_sample	sample;

	ao_bmi088_setup();
	for (;;)
	{
		ao_bmi088_spi_get();
		_ao_bmi088_acc_sample_read(&sample.acc);
		_ao_bmi088_gyr_sample_read(&sample.gyr);
		ao_bmi088_spi_put();
		ao_arch_block_interrupts();
		ao_bmi088_current = sample;
		AO_DATA_PRESENT(AO_DATA_BMI088);
		AO_DATA_WAIT();
		ao_arch_release_interrupts();
	}
}

static struct ao_task ao_bmi088_task;

static void
ao_bmi088_show(void)
{
#ifdef AO_LOG_NORMALIZED
	printf ("BMI088: %7d %7d %7d %7d %7d %7d\n",
		ao_bmi088_along(&ao_bmi088_current),
		ao_bmi088_across(&ao_bmi088_current),
		ao_bmi088_through(&ao_bmi088_current),
		ao_bmi088_roll(&ao_bmi088_current),
		ao_bmi088_pitch(&ao_bmi088_current),
		ao_bmi088_yaw(&ao_bmi088_current));
#else
	printf ("Accel: %7d %7d %7d Gyro: %7d %7d %7d\n",
		ao_bmi088_current.acc.x,
		ao_bmi088_current.acc.y,
		ao_bmi088_current.acc.z,
		ao_bmi088_current.gyr.x,
		ao_bmi088_current.gyr.y,
		ao_bmi088_current.gyr.z);
#endif
}

static const struct ao_cmds bmi_cmds[] = {
	{ ao_bmi088_show,		"I\0Show BMI088 status" },
	{ 0, 0 }
};

void
ao_bmi088_init(void)
{
	AO_TICK_TYPE	then;
	ao_spi_init_cs(AO_BMI088_ACC_CS_PORT, (1 << AO_BMI088_ACC_CS_PIN));
	ao_spi_init_cs(AO_BMI088_GYR_CS_PORT, (1 << AO_BMI088_GYR_CS_PIN));

	/* force acc part to SPI mode */
	ao_bmi088_acc_start();
	then = ao_time();
	while ((ao_time() - then) < 2)
		;
	ao_bmi088_acc_end();

	ao_add_task(&ao_bmi088_task, ao_bmi088, "bmi088");

	ao_cmd_register(bmi_cmds);
}
