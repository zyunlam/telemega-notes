/*
 * Copyright © 2021 Keith Packard <keithp@keithp.com>
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
#include <ao_mmc5983.h>
#include <ao_exti.h>

#if HAS_MMC5983

struct ao_mmc5983_sample	ao_mmc5983_current;

static uint8_t	ao_mmc5983_configured;

#ifdef MMC5983_I2C
#include <ao_i2c_bit.h>

static void
ao_mmc5983_reg_write(uint8_t addr, uint8_t data)
{
	uint8_t d[2];

	d[0] = addr;
	d[1] = data;

	ao_i2c_bit_start(MMC5983_I2C_ADDR);
	ao_i2c_bit_send(d, 2);
	ao_i2c_bit_stop();
}

static uint8_t
ao_mmc5983_reg_read(uint8_t addr)
{
	uint8_t d[1];

	ao_i2c_bit_start(MMC5983_I2C_ADDR);
	d[0] = addr;
	ao_i2c_bit_send(d, 1);
	ao_i2c_bit_restart(MMC5983_I2C_ADDR | 1);
	ao_i2c_bit_recv(d, 1);
	ao_i2c_bit_stop();
	return d[0];
}

static void
ao_mmc5983_sample(struct ao_mmc5983_sample *sample)
{
	struct ao_mmc5983_raw	raw;

	ao_i2c_bit_start(MMC5983_I2C_ADDR);
	raw.addr = MMC5983_X_OUT_0;
	ao_i2c_bit_send(&raw.addr, 1);
	ao_i2c_bit_restart(MMC5983_I2C_ADDR | 1);
	ao_i2c_bit_recv(&raw.x0, 7);
	ao_i2c_bit_stop();

	sample->x = raw.x0 << 10 | raw.x1 << 2 | ((raw.xyz2 >> 6) & 3);
	sample->y = raw.y0 << 10 | raw.y1 << 2 | ((raw.xyz2 >> 4) & 3);
	sample->z = raw.z0 << 10 | raw.z1 << 2 | ((raw.xyz2 >> 2) & 3);
}

#else
#define AO_MMC5983_SPI_SPEED	ao_spi_speed(2000000)

static void
ao_mmc5983_start(void) {
	ao_spi_get_bit(AO_MMC5983_SPI_CS_PORT,
		       AO_MMC5983_SPI_CS_PIN,
		       AO_MMC5983_SPI_INDEX,
		       AO_MMC5983_SPI_SPEED);
}

static void
ao_mmc5983_stop(void) {
	ao_spi_put_bit(AO_MMC5983_SPI_CS_PORT,
		       AO_MMC5983_SPI_CS_PIN,
		       AO_MMC5983_SPI_INDEX);
}


static void
ao_mmc5983_reg_write(uint8_t addr, uint8_t data)
{
	uint8_t	d[2];

	d[0] = addr;
	d[1] = data;
	ao_mmc5983_start();
	ao_spi_send(d, 2, AO_MMC5983_SPI_INDEX);
	ao_mmc5983_stop();
}

static uint8_t
ao_mmc5983_reg_read(uint8_t addr)
{
	uint8_t	d[2];

	d[0] = addr | MMC5983_READ;
	ao_mmc5983_start();
	ao_spi_duplex(d, d, 2, AO_MMC5983_SPI_INDEX);
	ao_mmc5983_stop();

	return d[1];
}

static void
ao_mmc5983_duplex(uint8_t *dst, uint8_t len)
{
	ao_mmc5983_start();
	ao_spi_duplex(dst, dst, len, AO_MMC5983_SPI_INDEX);
	ao_mmc5983_stop();
}

static void
ao_mmc5983_sample(struct ao_mmc5983_sample *sample)
{
	struct ao_mmc5983_raw	raw;

	raw.addr = MMC5983_X_OUT_0 | MMC5983_READ;
	ao_mmc5983_duplex((uint8_t *) &raw, sizeof (raw));

	sample->x = raw.x0 << 10 | raw.x1 << 2 | ((raw.xyz2 >> 6) & 3);
	sample->y = raw.y0 << 10 | raw.y1 << 2 | ((raw.xyz2 >> 4) & 3);
	sample->z = raw.z0 << 10 | raw.z1 << 2 | ((raw.xyz2 >> 2) & 3);
}
#endif

static uint8_t product_id;

static uint8_t
ao_mmc5983_setup(void)
{

	if (ao_mmc5983_configured)
		return 1;

	/* Delay for power up time (10ms) */
	ao_delay(AO_MS_TO_TICKS(10));

	ao_mmc5983_reg_write(MMC5983_CONTROL_1,
			     1 << MMC5983_CONTROL_1_SW_RST);

	/* Delay for power up time (10ms) */
	ao_delay(AO_MS_TO_TICKS(10));

	/* Check product ID */
	product_id = ao_mmc5983_reg_read(MMC5983_PRODUCT_ID);
	if (product_id != MMC5983_PRODUCT_ID_PRODUCT_I2C &&
	    product_id != MMC5983_PRODUCT_ID_PRODUCT_SPI)
	{
		AO_SENSOR_ERROR(AO_DATA_MMC5983);
	}

	/* Set bandwidth to 200Hz */
	ao_mmc5983_reg_write(MMC5983_CONTROL_1,
			     MMC5983_CONTROL_1_BW_200 << MMC5983_CONTROL_1_BW);

	/* Measure at 200Hz so we get recent samples by just reading
	 * the registers
	 */
	ao_mmc5983_reg_write(MMC5983_CONTROL_2,
			     (1 << MMC5983_CONTROL_2_CMM_EN) |
			     (MMC5983_CONTROL_2_CM_FREQ_200HZ << MMC5983_CONTROL_2_CM_FREQ));

	ao_mmc5983_configured = 1;
	return 1;
}

struct ao_mmc5983_sample ao_mmc5983_current;

static void
ao_mmc5983(void)
{
	struct ao_mmc5983_sample	sample;
	ao_mmc5983_setup();
	for (;;) {
		ao_mmc5983_sample(&sample);
		ao_arch_block_interrupts();
		ao_mmc5983_current = sample;
		AO_DATA_PRESENT(AO_DATA_MMC5983);
		AO_DATA_WAIT();
		ao_arch_release_interrupts();
	}
}

static struct ao_task ao_mmc5983_task;

static void
ao_mmc5983_show(void)
{
	printf ("MMC5983: %d %d %d\n",
		ao_mmc5983_along(&ao_mmc5983_current),
		ao_mmc5983_across(&ao_mmc5983_current),
		ao_mmc5983_through(&ao_mmc5983_current));
}

static const struct ao_cmds ao_mmc5983_cmds[] = {
	{ ao_mmc5983_show,	"M\0Show MMC5983 status" },
	{ 0, NULL }
};

void
ao_mmc5983_init(void)
{
	ao_mmc5983_configured = 0;

#ifdef MMC5983_I2C
	ao_enable_output(AO_MMC5983_SPI_CS_PORT, AO_MMC5983_SPI_CS_PIN, 1);
#else
	ao_enable_input(AO_MMC5983_SPI_MISO_PORT,
			AO_MMC5983_SPI_MISO_PIN,
			AO_EXTI_MODE_PULL_NONE);

	ao_enable_output(AO_MMC5983_SPI_CLK_PORT,
			 AO_MMC5983_SPI_CLK_PIN,
			 1);

	ao_enable_output(AO_MMC5983_SPI_MOSI_PORT,
			 AO_MMC5983_SPI_MOSI_PIN,
			 0);

	ao_spi_init_cs(AO_MMC5983_SPI_CS_PORT, (1 << AO_MMC5983_SPI_CS_PIN));
#endif

	ao_add_task(&ao_mmc5983_task, ao_mmc5983, "mmc5983");
	ao_cmd_register(&ao_mmc5983_cmds[0]);
}

#endif
