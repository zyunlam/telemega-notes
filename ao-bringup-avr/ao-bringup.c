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

#include "ao-bringup.h"

static void
clock_init(void)
{
	/* disable RC clock */
	CLKSEL0 &= ~(1 << RCE);

	/* Disable PLL */
	PLLCSR &= ~(1 << PLLE);

	/* Enable external clock */
	CLKSEL0 |= (1 << EXTE);

	/* wait for external clock to be ready */
	while ((CLKSTA & (1 << EXTON)) == 0)
		;

	/* select external clock */
	CLKSEL0 |= (1 << CLKS);

	/* Disable the clock prescaler */
	cli();
	CLKPR = (1 << CLKPCE);
	CLKPR = 0;
	sei();

	/* Set up the PLL to use the crystal */

	/* Use primary system clock as PLL source */
	PLLFRQ = ((0 << PINMUX) |	/* Use primary clock */
		  (0 << PLLUSB) |	/* No divide by 2 for USB */
		  (0 << PLLTM0) |	/* Disable high speed timer */
		  (0x4 << PDIV0));	/* 48MHz PLL clock */

	/* Set the frequency of the crystal */
#if TEENSY
	PLLCSR |= (1 << PINDIV);	/* For 16MHz crystal on Teensy board */
#else
	PLLCSR &= ~(1 << PINDIV);	/* For 8MHz crystal on TeleScience board */
#endif

	/* Enable the PLL */
	PLLCSR |= (1 << PLLE);
	while (!(PLLCSR & (1 << PLOCK)))
		;
}

void
uart_send(char c)
{
	LEDPORT ^= (1 << LEDOUT);
	loop_until_bit_is_set(UCSR1A, UDRE1);
	UDR1 = c;
}

int
uart_put(char c, FILE *stream)
{
	if (c == '\n')
		uart_send('\r');
	uart_send(c);
	return 0;
}

int
uart_get(FILE *stream)
{
	loop_until_bit_is_set(UCSR1A, RXC1);
	return (int) UDR1 & 0xff;
}

void
uart_init(uint16_t baud)
{
	PRR1 &= ~(1 << PRUSART1);
	UBRR1L = baud;
	UBRR1H = baud >> 8;
	UCSR1A = 0;
	UCSR1B = ((1 << RXEN1) |	/* Enable receiver */
		  (1 << TXEN1));	/* Enable transmitter */
	UCSR1C = ((0 << UMSEL10) |	/* Asynchronous mode */
		  (0 << UPM10) |	/* No parity */
		  (0 << USBS1) |	/* 1 stop bit */
		  (3 << UCSZ10) |	/* 8 bit characters */
		  (0 << UCPOL1));	/* MBZ for async mode */
}

static FILE mystdout = FDEV_SETUP_STREAM(uart_put, NULL, _FDEV_SETUP_WRITE);

static FILE mystdin = FDEV_SETUP_STREAM(NULL, uart_get, _FDEV_SETUP_READ);

void	ao_bringup_init(void)
{
	clock_init();
	uart_init(F_CPU / (16UL * 9600UL) - 1);

	stdout = &mystdout;
	stdin = &mystdin;

	if (LEDDDR & (1 << LEDDDRPIN)) {
		printf ("oops, starting all over\n");
		for (;;)
			;
	}
	LEDDDR |= (1 << LEDDDRPIN);
}
