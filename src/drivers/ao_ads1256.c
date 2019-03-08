/*
 * Copyright © 2019 Bdale Garbee <bdale@gag.com>
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
 */

#include <ao.h>
#include <ao_exti.h>
#include "ao_ads1256.h"

#define DEBUG_LOW	1
#define DEBUG_HIGH	2

#define DEBUG		0

#if DEBUG
#define PRINTD(l, ...) do { if (DEBUG & (l)) { printf ("\r%5u %s: ", ao_tick_count, __func__); printf(__VA_ARGS__); flush(); } } while(0)
#else
#define PRINTD(l,...)
#endif

struct ao_ads1256_sample	ao_ads1256_current;
uint8_t		nextchan = 0;
uint8_t		ao_ads1256_drdy;

static void
ao_ads1256_start(void) {
	ao_spi_get_bit(AO_ADS1256_SPI_CS_PORT,
		       AO_ADS1256_SPI_CS_PIN,
		       AO_ADS1256_SPI_BUS,
		       AO_ADS1256_SPI_SPEED);
}

static void
ao_ads1256_stop(void) {
	ao_spi_put_bit(AO_ADS1256_SPI_CS_PORT,
		       AO_ADS1256_SPI_CS_PIN,
		       AO_ADS1256_SPI_BUS);
}


static uint8_t
ao_ads1256_reg_read(uint8_t addr)
{
	uint8_t	d[2];

	d[0] = addr | AO_ADS1256_RREG;
	d[1] = 0;			
	ao_ads1256_start();
	ao_spi_send(d, 2, AO_ADS1256_SPI_BUS);
	d[0] = 0;
	ao_spi_recv(d, 1, AO_ADS1256_SPI_BUS);
	ao_ads1256_stop();

	PRINTD(DEBUG_LOW, "read %x = %x\n", addr, d[0]);

	return d[0];
}

/*
static void
ao_ads1256_reg_write(uint8_t addr, uint8_t value)
{
	uint8_t	d[3];

	PRINTD(DEBUG_LOW, "write %x %x\n", addr, value);
	d[0] = addr | AO_ADS1256_WREG;
	d[1] = 0;			
	d[2] = value;
	ao_ads1256_start();
	ao_spi_send(d, 3, AO_ADS1256_SPI_BUS);
	ao_ads1256_stop();

}
*/

static void 
ao_ads1256_isr(void)
{
	ao_ads1256_drdy = 1;
	ao_wakeup(&ao_ads1256_drdy);
}

static void
ao_ads1256_setup(void)
{
	uint8_t	d[20];

	ao_delay(1);

	/* set up interrupt on DRDY going low */

	ao_exti_setup(AO_ADS1256_DRDY_PORT, AO_ADS1256_DRDY_PIN,
		AO_EXTI_MODE_FALLING|AO_EXTI_PRIORITY_HIGH,
		ao_ads1256_isr);

	/* read status register and confirm device id matches */

        uint8_t devid = ao_ads1256_reg_read(AO_ADS1256_STATUS) >> 4;
        if (devid != AO_ADS1256_ID_ADS1256)
                ao_panic(AO_PANIC_SELF_TEST_ADS);

	/* write configuration registers, tell converter to start working */

	d[0] = AO_ADS1256_STATUS | AO_ADS1256_WREG;
	d[1] = 4;	/* write 5 registers starting with STATUS */
	d[2] = 0x00;	/* msb first, auto-cal off, analog buffer disabled */
	d[3] = 0x08;	/* mux AIN0 relative to AINCOM */
	d[4] = 0x20;	/* clock out = fclkin, sensor detect off, pga gain 1 */
	d[5] = 0xf0;	/* data rate 7500 SPS */
	d[6] = 0xf0;	/* all gpio pins are inputs */
	d[7] = AO_ADS1256_SYNC;
	d[8] = AO_ADS1256_WAKEUP;
	ao_ads1256_start();
	ao_spi_send(d, 9, AO_ADS1256_SPI_BUS);
	ao_ads1256_stop();
}

static void
ao_ads1256(void)
{
	uint8_t	d[6], curchan;

	ao_ads1256_setup();

	ao_exti_enable(AO_ADS1256_DRDY_PORT, AO_ADS1256_DRDY_PIN);

	for (;;) {
		ao_arch_block_interrupts();
		ao_ads1256_drdy = 0;
		while (ao_ads1256_drdy == 0)
			ao_sleep(&ao_ads1256_drdy);
		ao_arch_release_interrupts();

		curchan = nextchan;
		nextchan = (nextchan + 1) % AO_ADS1256_CHANNELS;

		d[0] = AO_ADS1256_MUX | AO_ADS1256_WREG;
		d[1] = 0;			/* write one register */
		d[2] = nextchan << 4 | 0x08; ;	/* relative to AINCOM */
		d[3] = AO_ADS1256_SYNC;
		d[4] = AO_ADS1256_WAKEUP;
		d[5] = AO_ADS1256_RDATA;
		ao_ads1256_start();
		ao_spi_send(d, 6, AO_ADS1256_SPI_BUS);
		ao_spi_recv(d, 3, AO_ADS1256_SPI_BUS);
		ao_ads1256_stop();

		ao_ads1256_current.ain[curchan] = 
			d[0] << 16 | d[1] << 8 | d[2];

		// FIXME
		//	If nextchan == 0, we have a complete set of inputs
		//	and we need to log them somewhere

		ao_ads1256_drdy = 0;
	}
}

static struct ao_task ao_ads1256_task;

static void
ao_ads1256_dump(void)	
{
	static int done;

	if (!done) {
		done = 1;
		ao_add_task(&ao_ads1256_task, ao_ads1256, "ads1256");
	}
		
	printf ("ADS1256 value 0x%x 0x%x 0x%x 0x%x\n",
		ao_ads1256_current.ain[0],
		ao_ads1256_current.ain[1],
		ao_ads1256_current.ain[2],
		ao_ads1256_current.ain[3]);
}

const struct ao_cmds ao_ads1256_cmds[] = {
	{ ao_ads1256_dump,	"D\0Display ADS1256 data" },
	{ 0, NULL },
};

void
ao_ads1256_init(void)
{
	ao_cmd_register(ao_ads1256_cmds);

//	ao_enable_output(AO_ADS1256_RESET_PORT, AO_ADS1256_RESET_PIN, 0);

	ao_spi_init_cs(AO_ADS1256_SPI_CS_PORT, 
		(1 << AO_ADS1256_SPI_CS_PIN));

//	ao_add_task(&ao_ads1256_task, ao_ads1256, "ads1256");
}
