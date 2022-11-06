/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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
#include <ao_led.h>
#include <ao_dma_samd21.h>

#define SNEK_CS_PORT	(&samd21_port_a)
#define SNEK_CS_PIN	(11)
#define SNEK_SPI_INDEX	AO_SPI_0_PA08_PA09_PA10
#define SNEK_SPI_SPEED	ao_spi_speed(1000000)

static const uint8_t spi_test[] = {
	0x55,
};

static void
ao_spi_test(void)
{
	ao_spi_get_bit(SNEK_CS_PORT, SNEK_CS_PIN, SNEK_SPI_INDEX, SNEK_SPI_SPEED);
	ao_spi_send(spi_test, sizeof(spi_test), SNEK_SPI_INDEX);
	ao_spi_put_bit(SNEK_CS_PORT, SNEK_CS_PIN, SNEK_SPI_INDEX);
}

const struct ao_cmds ao_spi_cmds[] = {
	{ ao_spi_test,	"s \0Send some bytes over spi" },
	{ 0, NULL },
};

int main(void)
{
	ao_led_init();
	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_dma_init();
	ao_spi_init();
	ao_usb_init();
	ao_cmd_register(ao_spi_cmds);
	ao_spi_init_cs(&samd21_port_a, 1 << 11); /* analog 8 for CS */
	ao_cmd_init();
	ao_start_scheduler();
	return 0;
}
