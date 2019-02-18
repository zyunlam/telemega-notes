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
#include "ao_ads124s0x.h"

#define DEBUG_LOW	1
#define DEBUG_HIGH	2

#define DEBUG		0

#if DEBUG
#define PRINTD(l, ...) do { if (DEBUG & (l)) { printf ("\r%5u %s: ", ao_tick_count, __func__); printf(__VA_ARGS__); flush(); } } while(0)
#else
#define PRINTD(l,...)
#endif

struct ao_ads124s0x_sample	ao_ads124s0x_current;

static void
ao_ads124s0x_start(void) {
	ao_spi_get_bit(AO_ADS124S0X_SPI_CS_PORT,
		       AO_ADS124S0X_SPI_CS_PIN,
		       AO_ADS124S0X_SPI_BUS,
		       AO_ADS124S0X_SPI_SPEED);
}

static void
ao_ads124s0x_stop(void) {
	ao_spi_put_bit(AO_ADS124S0X_SPI_CS_PORT,
		       AO_ADS124S0X_SPI_CS_PIN,
		       AO_ADS124S0X_SPI_BUS);
}

/*
static uint8_t
ao_ads124s0x_reg_read(uint8_t addr)
{
	uint8_t	d[2];

	d[0] = addr | AO_ADS124S0X_RREG;
	d[1] = 0;			
	ao_ads124s0x_start();
	ao_spi_duplex(d, d, 2, AO_ADS124S0X_SPI_BUS);
	ao_ads124s0x_stop();

	PRINTD(DEBUG_LOW, "read %x = %x\n", addr, d);

	return d[1];
}

static void
ao_ads124s0x_reg_write(uint8_t addr, uint8_t value)
{
	uint8_t	d[3];

	PRINTD(DEBUG_LOW, "write %x %x\n", addr, value);
	d[0] = addr | AO_ADS124S0X_WREG;
	d[1] = 0;			
	d[2] = value;
	ao_ads124s0x_start();
	ao_spi_send(d, 3, AO_ADS124S0X_SPI_BUS);
	ao_ads124s0x_stop();

#if DEBUG & DEBUG_LOW
	d[0] = addr | AO_ADS124S0X_RREG
	d[1] = 0;
	ao_ads124s0x_start();
	ao_spi_duplex(d, d, 2, AO_ADS124S0X_SPI_BUS);
	ao_ads124s0x_stop();
	PRINTD(DEBUG_LOW, "readback %x %x\n", d[0], d[1]);
#endif
}
*/

// FIXME 
//	We need to be in continuous conversion mode, and use the WREG
//	command to set the next conversion input while reading each 
//	which I don't see an example for elsewhere?

static void
ao_ads124s0x_setup(void)
{
	uint8_t	d[20];

/*	we have nowhere to report this error to since ao_sensor_errors is
	normally part of ao_flight?

	uint8_t	devid = ao_ads124s0x_reg_read(AO_ADS124S0X_ID);
	if (devid != AO_ADS124S0X_ID_ADS124S06)
		ao_sensor_errors = 1;
*/

	/* 1ksps each across 4 inputs using full duplex ala 9.5.4.3 */

	d[0] = AO_ADS124S0X_INPMUX | AO_ADS124S0X_WREG;
	d[1] = 8;	/* write 8 registers starting with INPMUX */
	d[2] = 0x0c;	/* input mux AIN0 relative to AINCOM */
	d[3] = 0x00;	/* default first conversion delay, pga disabled */
	d[4] = 0x1e;	/* gchop disabled, internal clock, continuous 
			   conversion, low-latency filter, 4000 SPS */
	d[5] = 0x00;	/* ref monitor disabled, ref buffers bypassed, ref 
			   set to REFP0/REFN0, internal reference off */
	d[6] = 0x00;	/* pga otuput rail, low side power switch, excitation
			   current source all off */
	d[7] = 0xff;	/* idac1 and idac2 disconnected */
	d[8] = 0x00;	/* all vbias disconnected */
	d[9] = 0x10;	/* sys monitor off, spi timeout disabled, crc disabled,
			   prepending status byte disabled */
	ao_ads124s0x_start();
	ao_spi_send(d, 10, AO_ADS124S0X_SPI_BUS);
	ao_ads124s0x_stop();

	/* start conversions */
	
	d[0] = AO_ADS124S0X_START;
	ao_ads124s0x_start();
	ao_spi_send(d, 1, AO_ADS124S0X_SPI_BUS);
	ao_ads124s0x_stop();
}

static void
ao_ads124s0x(void)
{
	ao_ads124s0x_setup();
/*
	for (;;) {
		ao_ads124s0x_value(&ao_ads124s0x_current);
		ao_arch_critical(
			AO_DATA_PRESENT(AO_DATA_ADS124S0X);
			AO_DATA_WAIT();
			);
	}
*/
}

static struct ao_task ao_ads124s0x_task;

static void
ao_ads124s0x_dump(void)			// FIXME
{
	printf ("ADS124S0X value %d %d %d %d\n",
		ao_ads124s0x_current.ain0,
		ao_ads124s0x_current.ain1,
		ao_ads124s0x_current.ain2,
		ao_ads124s0x_current.ain3);
}

const struct ao_cmds ao_ads124s0x_cmds[] = {
	{ ao_ads124s0x_dump,	"I\0Display ADS124S0X data" },
	{ 0, NULL },
};

void
ao_ads124s0x_init(void)
{
	ao_cmd_register(ao_ads124s0x_cmds);
	ao_spi_init_cs(AO_ADS124S0X_SPI_CS_PORT, 
		(1 << AO_ADS124S0X_SPI_CS_PIN));

	ao_add_task(&ao_ads124s0x_task, ao_ads124s0x, "ads124s0x");
}
