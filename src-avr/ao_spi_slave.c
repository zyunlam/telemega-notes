/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
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

#include "ao.h"

__xdata struct ao_fifo	ao_spi_slave_rx_fifo;
__xdata struct ao_fifo	ao_spi_slave_tx_fifo;

static volatile uint8_t	ao_spi_slave_tx_started;

static void
ao_spi_slave_tx_start(void)
{
	if (!ao_spi_slave_tx_started && !ao_fifo_empty(ao_spi_slave_tx_fifo)) {
		ao_spi_slave_tx_started = 1;
		ao_fifo_remove(ao_spi_slave_tx_fifo, SPDR);
	}
}

ISR(SPI_STC_vect)
{
	uint8_t	spsr;

	spsr = SPSR;
	if (SPIF & (1 << SPIF)) {
		uint8_t	byte = SPDR;
		if (!ao_fifo_full(ao_spi_slave_rx_fifo))
			ao_fifo_insert(ao_spi_slave_rx_fifo, byte);
		ao_spi_slave_tx_started = 0;
		ao_spi_slave_tx_start();
		ao_wakeup(&ao_spi_slave_rx_fifo);
		ao_wakeup(&ao_spi_slave_tx_fifo);
	}
}

static void
ao_spi_slave_put(uint8_t b) __critical
{
	cli();
	while (ao_fifo_full(ao_spi_slave_tx_fifo))
		ao_sleep(&ao_spi_slave_tx_fifo);
	ao_fifo_insert(ao_spi_slave_tx_fifo, b);
	ao_spi_slave_tx_start();
	sei();
}

static uint8_t
ao_spi_slave_get(void) __critical
{
	uint8_t	b;

	cli();
	while (ao_fifo_empty(ao_spi_slave_rx_fifo))
		ao_sleep(&ao_spi_slave_rx_fifo);
	ao_fifo_remove(ao_spi_slave_rx_fifo, b);
	sei();
	return b;
}

void
ao_spi_slave_read(uint8_t *data, int len)
{
	while (len--) {
		ao_spi_slave_put(0);
		*data++ = ao_spi_slave_get();
	}
}

void
ao_spi_slave_write(uint8_t *data, int len)
{
	while (len--) {
		ao_spi_slave_put(*data++);
		(void) ao_spi_slave_get();
	}
}

void
ao_spi_slave_init(void)
{
	SPCR = (1 << SPIE) |		/* Enable SPI interrupts */
		(1 << SPE) |		/* Enable SPI */
		(0 << DORD) |		/* MSB first */
		(0 << MSTR) |		/* Slave mode */
		(0 << CPOL) |		/* Clock low when idle */
		(0 << CPHA);		/* Sample at leading clock edge */
}
