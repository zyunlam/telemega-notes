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
#include "ao_product.h"

static struct ao_companion_command	ao_companion_command;
static const struct ao_companion_setup	ao_companion_setup = {
	.board_id 		= AO_idProduct_NUMBER,
	.board_id_inverse	= ~AO_idProduct_NUMBER,
	.update_period		= 50,
	.channels		= NUM_ADC
};

static uint8_t ao_spi_slave_recv(void)
{
	uint8_t	*buf;
	uint8_t len;

	len = sizeof (ao_companion_command);
	buf = (uint8_t *) &ao_companion_command;
	while (len--) {
		while (!(SPSR & (1 << SPIF)))
			if ((PINB & (1 << PINB0)))
				return 0;
		*buf++ = SPDR;
	}

	/* Figure out the outbound data */
	switch (ao_companion_command.command) {
	case AO_COMPANION_SETUP:
		buf = (uint8_t *) &ao_companion_setup;
		len = sizeof (ao_companion_setup);
		break;
	case AO_COMPANION_FETCH:
		buf = (uint8_t *) &ao_adc_ring[ao_adc_ring_prev(ao_adc_head)].adc;
		len = NUM_ADC * sizeof (uint16_t);
		break;
	case AO_COMPANION_STATE:
		break;
	default:
		return 0;
	}

	if (len) {
		/* Send the outbound data */
		while (len--) {
			SPDR = *buf++;
			while (!(SPSR & (1 << SPIF)))
				if ((PINB & (1 << PINB0)))
					return 0;
		}
		(void) SPDR;
	}
	ao_log_store.tm_tick = ao_companion_command.tick;
	if (ao_log_store.tm_state != ao_companion_command.flight_state) {
		ao_log_store.tm_state = ao_companion_command.flight_state;
		return 1;
	}
	return 0;
}

static uint8_t ao_spi_slave_running;

ISR(PCINT0_vect)
{
	if ((PINB & (1 << PINB0)) == 0) {
		if (!ao_spi_slave_running) {
			uint8_t	changed;
			ao_spi_slave_running = 1;
			cli();
			changed = ao_spi_slave_recv();
			sei();
			if (changed && ao_flight_boost <= ao_log_store.tm_state) {
				if (ao_log_store.tm_state < ao_flight_landed)
					ao_log_start();
				else
					ao_log_stop();
			}
		}
	} else {
		ao_spi_slave_running = 0;
	}
}

void ao_spi_slave_debug(void) {
	printf ("slave running %d\n", ao_spi_slave_running);
}

void
ao_spi_slave_init(void)
{
	PCMSK0 |= (1 << PCINT0);	/* Enable PCINT0 pin change */
	PCICR |= (1 << PCIE0);		/* Enable pin change interrupt */

	DDRB = (DDRB & 0xf0) | (1 << 3);
	SPCR = (0 << SPIE) |		/* Disable SPI interrupts */
		(1 << SPE) |		/* Enable SPI */
		(0 << DORD) |		/* MSB first */
		(0 << MSTR) |		/* Slave mode */
		(0 << CPOL) |		/* Clock low when idle */
		(0 << CPHA);		/* Sample at leading clock edge */
}
