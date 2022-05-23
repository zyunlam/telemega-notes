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
#include "ao_ads131a0x.h"

#define AO_ADS131A0X_SPI_SPEED ao_spi_speed(8000000)

#define DEBUG_LOW	1
#define DEBUG_HIGH	2

#define DEBUG		0

#if DEBUG
#define PRINTD(l, ...) do { if (DEBUG & (l)) { printf ("\r%5u %s: ", ao_tick_count, __func__); printf(__VA_ARGS__); flush(); } } while(0)
#else
#define PRINTD(l,...)
#endif

struct ao_ads131a0x_sample	ao_ads131a0x_current;
uint8_t		nextchan = 0;
uint8_t		ao_ads131a0x_drdy;

static void
ao_ads131a0x_start(void) {
	ao_spi_get_bit(AO_ADS131A0X_SPI_CS_PORT,
		       AO_ADS131A0X_SPI_CS_PIN,
		       AO_ADS131A0X_SPI_BUS,
		       AO_ADS131A0X_SPI_SPEED);
}

static void
ao_ads131a0x_stop(void) {
	ao_spi_put_bit(AO_ADS131A0X_SPI_CS_PORT,
		       AO_ADS131A0X_SPI_CS_PIN,
		       AO_ADS131A0X_SPI_BUS);
}

static uint8_t
ao_ads131a0x_reg_read(uint8_t addr)
{
	uint8_t	d[15];

	d[0] = addr | (AO_ADS131A0X_RREG >> 8);
	d[1] = 0;			
	d[2] = 0;			
	d[3] = 0;			
	d[4] = 0;			
	d[5] = 0;			
	d[6] = 0;			
	d[7] = 0;			
	d[8] = 0;			
	d[9] = 0;			
	d[10] = 0;			
	d[11] = 0;			
	d[12] = 0;			
	d[13] = 0;			
	d[14] = 0;			
	ao_ads131a0x_start();
	ao_spi_send(d, 15, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();

	/* data isn't actually returned until the next frame */
	d[0] = AO_ADS131A0X_NULL >> 8;
	d[1] = AO_ADS131A0X_NULL & 0xff;
	ao_ads131a0x_start();
	ao_spi_duplex(d, d, 15, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();

	/* first byte is register number, second byte is the data */
	PRINTD(DEBUG_LOW, "read %x = %x\n", addr, d[1]);

	return d[1];
}

/*
static void
ao_ads131a0x_reg_write(uint8_t addr, uint8_t value)
{
	uint8_t	d[15];

	PRINTD(DEBUG_LOW, "write %x %x\n", addr, value);
	d[0] = addr | (AO_ADS131A0X_WREG >> 8);
	d[1] = value;
	d[2] = 0;			
	d[3] = 0;			
	d[4] = 0;			
	d[5] = 0;			
	d[6] = 0;			
	d[7] = 0;			
	d[8] = 0;			
	d[9] = 0;			
	d[10] = 0;			
	d[11] = 0;			
	d[12] = 0;			
	d[13] = 0;			
	d[14] = 0;			
	ao_ads131a0x_start();
	ao_spi_send(d, 15, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();
}
*/

static void 
ao_ads131a0x_isr(void)
{
	ao_ads131a0x_drdy = 1;
	ao_wakeup(&ao_ads131a0x_drdy);
}

static void
ao_ads131a0x_setup(void)
{
	uint8_t	d[20];

	ao_delay(1);

	ao_gpio_set(AO_ADS131A0X_RESET_PORT, AO_ADS131A0X_RESET_PIN, 1);

	ao_delay(1);

	/* confirm we're talking to the chip we're expecting */
	uint8_t	devid = ao_ads131a0x_reg_read(AO_ADS131A0X_ID_MSB);
	if (devid != AO_ADS131A0X_ID_ADS131A04)
		ao_panic(AO_PANIC_SELF_TEST_ADS);

	/* send unlock command so we can configure the chip */

	d[0] = AO_ADS131A0X_UNLOCK >> 8;
	d[1] = AO_ADS131A0X_UNLOCK & 0xff;
	d[2] = 0;			
	ao_ads131a0x_start();
	ao_spi_send(d, 3, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();

	/* data isn't actually returned until the next frame */
	d[0] = AO_ADS131A0X_NULL >> 8;
	d[1] = AO_ADS131A0X_NULL & 0xff;
	ao_ads131a0x_start();
	ao_spi_duplex(d, d, 3, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();

	ao_exti_setup(AO_ADS131A0X_DRDY_PORT, AO_ADS131A0X_DRDY_PIN,
		AO_EXTI_MODE_FALLING|AO_EXTI_PRIORITY_HIGH,
		ao_ads131a0x_isr);

	/* register writes use top 16 bits of each 24-bit word */

	d[0] = AO_ADS131A0X_A_SYS_CFG | (AO_ADS131A0X_WREGS >> 8);
	d[1] = 8;	/* write (n-1) registers starting with A_SYS_CFG */
	d[2] = 0;

	d[3] = 0x78;	/* A_SYS_CFG - charge pump off, high res, 4.0V ref,
			   internal ref on, 5/95% fault thresholds */
	d[4] = 0x00;	/* D_SYS_CFG - watchdog off, crc on device words, 
			   shortest done and hi-z delays, non-fixed size, 
			   crc disabled */
	d[5] = 0;

	d[6] = 0x02;	/* CLK1 - crystal osc on, ICLK = CLKIN / 2 */
	d[7] = 0x20;	/* CLK2 - MOD = ICLK / 2, DATA = MOD / 4096 */
	d[8] = 0;

	d[9] = 0x0f;	/* ADC_ENA - all channels powered up */
	d[10] = 0;	/* reserved */
	d[11] = 0;

	d[12] = 0x00;	/* ADC1 - gain = 1 */
	d[13] = 0x00;	/* ADC2 - gain = 1 */
	d[14] = 0;

	d[15] = 0x00;	/* ADC3 - gain = 1 */
	d[16] = 0x00;	/* ADC4 - gain = 1 */
	d[17] = 0;

	ao_ads131a0x_start();
	ao_spi_send(d, 18, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();

	/* start conversions */
	
	d[0] = AO_ADS131A0X_WAKEUP >> 8;
	d[1] = AO_ADS131A0X_WAKEUP & 0xff;
	d[2] = 0;
	ao_ads131a0x_start();
	ao_spi_send(d, 3, AO_ADS131A0X_SPI_BUS);
	ao_ads131a0x_stop();

	/* documented initialization sends the LOCK command here, but I can 
	   see no value in doing that! */
}

static void
ao_ads131a0x(void)
{
	uint8_t	d[15];

	ao_ads131a0x_setup();

	ao_exti_enable(AO_ADS131A0X_DRDY_PORT, AO_ADS131A0X_DRDY_PIN);

	for (;;) {
		ao_arch_block_interrupts();
		ao_ads131a0x_drdy = 0;
		while (ao_ads131a0x_drdy == 0)
			ao_sleep(&ao_ads131a0x_drdy);
		ao_arch_release_interrupts();

		d[0] = AO_ADS131A0X_NULL >> 8;
		d[1] = AO_ADS131A0X_NULL & 0xff;
		d[2] = 0;			
		d[3] = 0;			
		d[4] = 0;			
		d[5] = 0;			
		d[6] = 0;			
		d[7] = 0;			
		d[8] = 0;			
		d[9] = 0;			
		d[10] = 0;			
		d[11] = 0;			
		d[12] = 0;			
		d[13] = 0;			
		d[14] = 0;			

		ao_ads131a0x_start();
		ao_spi_duplex(d, d, 15, AO_ADS131A0X_SPI_BUS);
		ao_ads131a0x_stop();

		ao_ads131a0x_current.ain[0] = 
			d[3] << 16 | d[4] << 8 | d[5];
		ao_ads131a0x_current.ain[1] = 
			d[6] << 16 | d[7] << 8 | d[8];
		ao_ads131a0x_current.ain[2] = 
			d[9] << 16 | d[10] << 8 | d[11];
		ao_ads131a0x_current.ain[3] = 
			d[12] << 16 | d[13] << 8 | d[14];

		// FIXME
		//	 we need to log this data somewhere

		ao_ads131a0x_drdy = 0;
	}
}

static struct ao_task ao_ads131a0x_task;

static void
ao_ads131a0x_dump(void)	
{
	static int done;

	// add task here while debugging so we can get to command prompt!
	if (!done) {
		done = 1;
		ao_add_task(&ao_ads131a0x_task, ao_ads131a0x, "ads131a0x");
	}
		
	printf ("ADS131A0X value %8lx %8lx %8lx %8lx\n",
		ao_ads131a0x_current.ain[0],
		ao_ads131a0x_current.ain[1],
		ao_ads131a0x_current.ain[2],
		ao_ads131a0x_current.ain[3]);
}

const struct ao_cmds ao_ads131a0x_cmds[] = {
	{ ao_ads131a0x_dump,	"A\0Display ADS131A0X data" },
	{ 0, NULL },
};

void
ao_ads131a0x_init(void)
{
	ao_cmd_register(ao_ads131a0x_cmds);

	ao_enable_output(AO_ADS131A0X_RESET_PORT, AO_ADS131A0X_RESET_PIN, 0);

	ao_spi_init_cs(AO_ADS131A0X_SPI_CS_PORT, 
		(1 << AO_ADS131A0X_SPI_CS_PIN));

//	ao_add_task(&ao_ads131a0x_task, ao_ads131a0x, "ads131a0x");
}
