/*
 * Copyright © 2019 Keith Packard <keithp@keithp.com>
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

#include <ao.h>
#include <ao_bmx160.h>
#include <ao_exti.h>

static uint8_t	ao_bmx160_configured;

#define ao_bmx160_spi_get()	ao_spi_get(AO_BMX160_SPI_BUS, AO_SPI_SPEED_8MHz)
#define ao_bmx160_spi_put()	ao_spi_put(AO_BMX160_SPI_BUS)

#define ao_bmx160_spi_start() 	ao_spi_set_cs(AO_BMX160_SPI_CS_PORT,	\
					      (1 << AO_BMX160_SPI_CS_PIN))

#define ao_bmx160_spi_end() 	ao_spi_clr_cs(AO_BMX160_SPI_CS_PORT,	\
					      (1 << AO_BMX160_SPI_CS_PIN))

static void
_ao_bmx160_reg_write(uint8_t addr, uint8_t value)
{
	uint8_t	d[2] = { addr, value };
	ao_bmx160_spi_start();
	ao_spi_send(d, 2, AO_BMX160_SPI_BUS);
	ao_bmx160_spi_end();
}

static void
_ao_bmx160_read(uint8_t addr, void *data, uint8_t len)
{
	addr |= 0x80;
	ao_bmx160_spi_start();
	ao_spi_send(&addr, 1, AO_BMX160_SPI_BUS);
	ao_spi_recv(data, len, AO_BMX160_SPI_BUS);
	ao_bmx160_spi_end();
}

static uint8_t
_ao_bmx160_reg_read(uint8_t addr)
{
	uint8_t	value;
	addr |= 0x80;
	ao_bmx160_spi_start();
	ao_spi_send(&addr, 1, AO_BMX160_SPI_BUS);
	ao_spi_recv(&value, 1, AO_BMX160_SPI_BUS);
	ao_bmx160_spi_end();
	return value;
}

static void
_ao_bmx160_cmd(uint8_t cmd)
{
	_ao_bmx160_reg_write(BMX160_CMD, cmd);
	ao_delay(AO_MS_TO_TICKS(100));
}

static void
_ao_bmx160_mag_setup(void)
{
	_ao_bmx160_reg_write(BMX160_MAG_IF_0, 0x80);
}

static void
_ao_bmm150_wait_manual(void)
{
	while (_ao_bmx160_reg_read(BMX160_STATUS) & (1 << BMX160_STATUS_MAG_MAN_OP))
		;
}

static void
_ao_bmm150_reg_write(uint8_t addr, uint8_t data)
{
	_ao_bmx160_reg_write(BMX160_MAG_IF_3, data);
	_ao_bmx160_reg_write(BMX160_MAG_IF_2, addr);
	_ao_bmm150_wait_manual();
}

#if BMX160_TEST
static uint8_t
_ao_bmm150_reg_read(uint8_t addr)
{
	_ao_bmx160_reg_write(BMX160_MAG_IF_1, addr);
	_ao_bmm150_wait_manual();
	return _ao_bmx160_reg_read(BMX160_DATA_0);
}
#endif

static void
_ao_bmx160_sample(struct ao_bmx160_sample *sample)
{
	_ao_bmx160_read(BMX160_MAG_X_0_7, sample, sizeof (*sample));
#if __BYTE_ORDER != __LITTLE_ENDIAN
	int		i = sizeof (*sample) / 2;
	uint16_t	*d = (uint16_t *) sample;

	/* byte swap */
	while (i--) {
		uint16_t	t = *d;
		*d++ = (t >> 8) | (t << 8);
	}
#endif
}

#define G	981	/* in cm/s² */

#if 0
static int16_t /* cm/s² */
ao_bmx160_accel(int16_t v)
{
	return (int16_t) ((v * (int32_t) (16.0 * 980.665 + 0.5)) / 32767);
}

static int16_t	/* deg*10/s */
ao_bmx160_gyro(int16_t v)
{
	return (int16_t) ((v * (int32_t) 20000) / 32767);
}

static uint8_t
ao_bmx160_accel_check(int16_t normal, int16_t test)
{
	int16_t	diff = test - normal;

	if (diff < BMX160_ST_ACCEL(16) / 4) {
		return 1;
	}
	if (diff > BMX160_ST_ACCEL(16) * 4) {
		return 1;
	}
	return 0;
}

static uint8_t
ao_bmx160_gyro_check(int16_t normal, int16_t test)
{
	int16_t	diff = test - normal;

	if (diff < 0)
		diff = -diff;
	if (diff < BMX160_ST_GYRO(2000) / 4) {
		return 1;
	}
	if (diff > BMX160_ST_GYRO(2000) * 4) {
		return 1;
	}
	return 0;
}
#endif

static void
_ao_bmx160_wait_alive(void)
{
	uint8_t	i;

	/* Wait for the chip to wake up */
	for (i = 0; i < 30; i++) {
		ao_delay(AO_MS_TO_TICKS(100));
		if (_ao_bmx160_reg_read(BMX160_CHIPID) == BMX160_CHIPID_BMX160)
			break;
	}
	if (i == 30)
		ao_panic(AO_PANIC_SELF_TEST_BMX160);
}

#define ST_TRIES	10
#define MAG_TRIES	10

static void
_ao_bmx160_setup(void)
{
	if (ao_bmx160_configured)
		return;

	/* Make sure the chip is responding */
	_ao_bmx160_wait_alive();

	/* Reboot */
	_ao_bmx160_cmd(BMX160_CMD_SOFTRESET);

	/* Force SPI mode */
	_ao_bmx160_reg_write(BMX160_NV_CONF, 1 << BMX160_NV_CONF_SPI_EN);

	/* Configure accelerometer:
	 *
	 * 	undersampling disabled
	 * 	normal filter
	 *	200Hz sampling rate
	 *	16g range
	 *
	 * This yields a 3dB cutoff frequency of 80Hz
	 */
	_ao_bmx160_reg_write(BMX160_ACC_CONF,
			     (0 << BMX160_ACC_CONF_ACC_US) |
			     (BMX160_ACC_CONF_ACC_BWP_NORMAL << BMX160_ACC_CONF_ACC_BWP) |
			     (BMX160_ACC_CONF_ACC_ODR_200 << BMX160_ACC_CONF_ACC_ODR));
	_ao_bmx160_reg_write(BMX160_ACC_RANGE,
			     BMX160_ACC_RANGE_16G);

	/* Configure gyro:
	 *
	 * 	200Hz sampling rate
	 *	Normal filter mode
	 *	±2000°/s
	 */
	_ao_bmx160_reg_write(BMX160_GYR_CONF,
			     (BMX160_GYR_CONF_GYR_BWP_NORMAL << BMX160_GYR_CONF_GYR_BWP) |
			     (BMX160_GYR_CONF_GYR_ODR_200 << BMX160_GYR_CONF_GYR_ODR));
	_ao_bmx160_reg_write(BMX160_GYR_RANGE,
			     BMX160_GYR_RANGE_2000);


	/* Configure magnetometer:
	 *
	 *	30Hz sampling rate
	 *	power on
	 *	axes enabled
	 */
	_ao_bmx160_cmd(BMX160_CMD_MAG_IF_SET_PMU_MODE(BMX160_PMU_STATUS_MAG_IF_PMU_STATUS_NORMAL));

	/* Enter setup mode */
	_ao_bmx160_mag_setup();

	/* Place in suspend mode to reboot the chip */
	_ao_bmm150_reg_write(BMM150_POWER_MODE,
			     (0 << BMM150_POWER_MODE_POWER_CONTROL));

	/* Power on */
	_ao_bmm150_reg_write(BMM150_POWER_MODE,
			     (1 << BMM150_POWER_MODE_POWER_CONTROL));

	/* Set data rate and place in sleep mode */
	_ao_bmm150_reg_write(BMM150_CONTROL,
			     (BMM150_CONTROL_DATA_RATE_30 << BMM150_CONTROL_DATA_RATE) |
			     (BMM150_CONTROL_OP_MODE_SLEEP << BMM150_CONTROL_OP_MODE));

	/* enable all axes (should already be enabled) */
	_ao_bmm150_reg_write(BMM150_INT_CONF,
			     (0 << BMM150_INT_CONF_X_DISABLE) |
			     (0 << BMM150_INT_CONF_Y_DISABLE) |
			     (0 << BMM150_INT_CONF_Z_DISABLE));

	/* Set repetition values (?) */
	_ao_bmm150_reg_write(BMM150_REPXY, BMM150_REPXY_VALUE(9));
	_ao_bmm150_reg_write(BMM150_REPZ, BMM150_REPZ_VALUE(15));

	/* To get data out of the magnetometer, set the control op mode to 'forced', then read
	 * from the data registers
	 */
	_ao_bmx160_reg_write(BMX160_MAG_IF_3, (BMM150_CONTROL_OP_MODE_FORCED << BMM150_CONTROL_OP_MODE));
	_ao_bmx160_reg_write(BMX160_MAG_IF_2, BMM150_CONTROL);
	_ao_bmx160_reg_write(BMX160_MAG_IF_1, BMM150_DATA_X_0_4);

	/* Set data rate to 200Hz */
	_ao_bmx160_reg_write(BMX160_MAG_CONF,
			     (BMX160_MAG_CONF_MAG_ODR_200 << BMX160_MAG_CONF_MAG_ODR));

	/* Put magnetometer interface back into 'normal mode'
	 */
	_ao_bmx160_reg_write(BMX160_MAG_IF_0,
			     (0 << BMX160_MAG_IF_0_MAG_MANUAL_EN) |
			     (0 << BMX160_MAG_IF_0_MAG_OFFSET) |
			     (0 << BMX160_MAG_IF_0_MAG_RD_BURST));

	/* Enable acc and gyr
	 */

	_ao_bmx160_cmd(BMX160_CMD_ACC_SET_PMU_MODE(BMX160_PMU_STATUS_ACC_PMU_STATUS_NORMAL));
	_ao_bmx160_cmd(BMX160_CMD_GYR_SET_PMU_MODE(BMX160_PMU_STATUS_GYR_PMU_STATUS_NORMAL));
	ao_bmx160_configured = 1;
}

struct ao_bmx160_sample	ao_bmx160_current;

static void
ao_bmx160(void)
{
	struct ao_bmx160_sample	sample;

	/* ao_bmx160_init already grabbed the SPI bus and mutex */
	_ao_bmx160_setup();
	ao_bmx160_spi_put();
	for (;;)
	{
		ao_bmx160_spi_get();
		_ao_bmx160_sample(&sample);
		ao_bmx160_spi_put();
		ao_arch_block_interrupts();
		ao_bmx160_current = sample;
		AO_DATA_PRESENT(AO_DATA_BMX160);
		AO_DATA_WAIT();
		ao_arch_release_interrupts();
	}
}

static struct ao_task ao_bmx160_task;

static void
ao_bmx160_show(void)
{
	printf ("Accel: %7d %7d %7d Gyro: %7d %7d %7d Mag: %7d %7d %7d\n",
		ao_bmx160_current.acc_x,
		ao_bmx160_current.acc_y,
		ao_bmx160_current.acc_z,
		ao_bmx160_current.gyr_x,
		ao_bmx160_current.gyr_y,
		ao_bmx160_current.gyr_z,
		ao_bmx160_current.mag_x,
		ao_bmx160_current.mag_y,
		ao_bmx160_current.mag_z);
}

#if BMX160_TEST

static void
ao_bmx160_read(void)
{
	uint8_t	addr;
	uint8_t val;

	addr = ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	ao_bmx160_spi_get();
	val = _ao_bmx160_reg_read(addr);
	ao_bmx160_spi_put();
	printf("Addr %02x val %02x\n", addr, val);
}

static void
ao_bmx160_write(void)
{
	uint8_t	addr;
	uint8_t val;

	addr = ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	val = ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	printf("Addr %02x val %02x\n", addr, val);
	ao_bmx160_spi_get();
	_ao_bmx160_reg_write(addr, val);
	ao_bmx160_spi_put();
}

static void
ao_bmm150_read(void)
{
	uint8_t	addr;
	uint8_t val;

	addr = ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	ao_bmx160_spi_get();
	val = _ao_bmm150_reg_read(addr);
	ao_bmx160_spi_put();
	printf("Addr %02x val %02x\n", addr, val);
}

static void
ao_bmm150_write(void)
{
	uint8_t	addr;
	uint8_t val;

	addr = ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	val = ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	printf("Addr %02x val %02x\n", addr, val);
	ao_bmx160_spi_get();
	_ao_bmm150_reg_write(addr, val);
	ao_bmx160_spi_put();
}

#endif /* BMX160_TEST */

static const struct ao_cmds ao_bmx160_cmds[] = {
	{ ao_bmx160_show,	"I\0Show BMX160 status" },
#if BMX160_TEST
	{ ao_bmx160_read,	"R <addr>\0Read BMX160 register" },
	{ ao_bmx160_write,	"W <addr> <val>\0Write BMX160 register" },
	{ ao_bmm150_read,	"M <addr>\0Read BMM150 register" },
	{ ao_bmm150_write,	"N <addr> <val>\0Write BMM150 register" },
#endif
	{ 0, NULL }
};

void
ao_bmx160_init(void)
{
	ao_add_task(&ao_bmx160_task, ao_bmx160, "bmx160");

	ao_spi_init_cs(AO_BMX160_SPI_CS_PORT, (1 << AO_BMX160_SPI_CS_PIN));

	/* Pretend to be the bmx160 task. Grab the SPI bus right away and
	 * hold it for the task so that nothing else uses the SPI bus before
	 * we get the I2C mode disabled in the chip
	 */

	ao_cur_task = &ao_bmx160_task;
	ao_bmx160_spi_get();
	ao_cur_task = NULL;
	ao_cmd_register(&ao_bmx160_cmds[0]);
}
