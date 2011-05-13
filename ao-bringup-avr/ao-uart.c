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

#include <avr/io.h>
#include <avr/interrupt.h>
#define TEENSY 1
#if TEENSY
#define F_CPU 16000000UL	// 16 MHz
#else
#define F_CPU  8000000UL	// 8 MHz
#endif
#include <util/delay.h>

#define LEDOUT		PORTB7
#define LEDPORT		PORTB
#define LEDDDR		DDRB
#define LEDDDRPIN	DD7

static void
timer1_init(void)
{
	TCCR1A = ((0 << WGM11) |	/* CTC mode, OCR1A */
		  (0 << WGM10));	/* CTC mode, OCR1A */
	TCCR1B = ((0 << ICNC1) |	/* no input capture noise canceler */
		  (0 << ICES1) |	/* input capture on falling edge (don't care) */
		  (0 << WGM13) |	/* CTC mode, OCR1A */
		  (1 << WGM12) |	/* CTC mode, OCR1A */
		  (3 << CS10));		/* clk/64 from prescaler */

#if TEENSY
	OCR1A = 2500;			/* 16MHz clock */
#else
	OCR1A = 1250;			/* 8MHz clock */
#endif

	TIMSK1 = (1 << OCIE1A);		/* Interrupt on compare match */
}

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
uart_send(char data)
{
	while (!(UCSR1A & (1 << UDRE1)))
		;
	UDR1 = data;
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

int main(void)
{
	clock_init();
	timer1_init();
	uart_init(832);

	LEDDDR |= (1 << LEDDDRPIN);

	for (;;) {
		LEDPORT ^= (1 << LEDOUT);
		uart_send('H');
	}
}
