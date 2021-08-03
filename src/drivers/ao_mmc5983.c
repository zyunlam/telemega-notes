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

#define AO_MMC5983_SPI_SPEED	ao_spi_speed(10000000)

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

struct ao_mmc5983_sample	ao_mmc5983_current;

static uint8_t	ao_mmc5983_configured;

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

static uint8_t
ao_mmc5983_setup(void)
{
	uint8_t	product_id;

	if (ao_mmc5983_configured)
		return 1;

	/* Place device in 3-wire mode */
	ao_mmc5983_reg_write(MMC5983_CONTROL_3,
			     1 << MMC5983_CONTROL_3_SPI_3W);

	/* Check product ID */
	product_id = ao_mmc5983_reg_read(MMC5983_PRODUCT_ID);
	if (product_id != MMC5983_PRODUCT_ID_PRODUCT)
		AO_SENSOR_ERROR(AO_DATA_MMC5983);

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
	printf ("X: %d Z: %d Y: %d\n",
		ao_mmc5983_current.x, ao_mmc5983_current.z, ao_mmc5983_current.y);
}

static const struct ao_cmds ao_mmc5983_cmds[] = {
	{ ao_mmc5983_show,	"M\0Show MMC5983 status" },
	{ 0, NULL }
};

void
ao_mmc5983_init(void)
{
	ao_mmc5983_configured = 0;

	ao_spi_init_cs(AO_MMC5983_SPI_CS_PORT, (1 << AO_MMC5983_SPI_CS_PIN));

	ao_add_task(&ao_mmc5983_task, ao_mmc5983, "mmc5983");
	ao_cmd_register(&ao_mmc5983_cmds[0]);
}

#endif
