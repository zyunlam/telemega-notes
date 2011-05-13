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
#define TEENSY 0
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
adc_start(void);

ISR(TIMER1_COMPA_vect)
{
	adc_start();
}

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

static void
timer0_init(void)
{
	TCCR0A = ((1 << COM0A1) |
		  (0 << COM0A0) |
		  (1 << WGM01) |
		  (1 << WGM00));

	OCR0A = 0x10;
	OCR0B = 0;
	TIMSK0 = 0;

	TCCR0B = ((0 << WGM02) |
		  (5 << CS00));
}

ISR(ADC_vect)
{
	OCR0A = ADCH;
}

#define ADC_CHANNEL	0x25		/* Channel ADC13 */
#define ADC_CHANNEL_LOW(c)	(((c) & 0x1f) << MUX0)
#define ADC_CHANNEL_HIGH(c)	((((c) & 0x20) >> 5) << MUX5)

#define ADCSRA_INIT	((1 << ADEN) |		/* Enable ADC */ 		\
			 (0 << ADATE) |		/* No auto ADC trigger */ 	\
			 (1 << ADIE) |		/* Enable interrupt */	\
			 (6 << ADPS0))		/* Prescale clock by 64 */

#define ADCSRB_INIT	((0 << ADHSM) |		/* No high-speed mode */ \
			 (0 << ACME) |		/* Some comparitor thing */ \
			 (0 << ADTS0))		/* Free running mode (don't care) */

static void
adc_start(void)
{
	ADMUX = ((0 << REFS1) |				/* AVcc reference */
		 (1 << REFS0) |				/* AVcc reference */
		 (1 << ADLAR) |				/* Left-shift results */
		 (ADC_CHANNEL_LOW(ADC_CHANNEL)));	/* Select channel */

	ADCSRB = ADCSRB_INIT | ADC_CHANNEL_HIGH(ADC_CHANNEL);

	ADCSRA = ADCSRA_INIT | (1 << ADSC);		/* Start conversion */
}

static void
adc_init(void)
{
	ADCSRA = ADCSRA_INIT;
	ADCSRB = ADCSRB_INIT;
	DIDR0 |= (1 << 0);
}

int main(void)
{
	clock_init();
	adc_init();
	timer1_init();
	timer0_init();

	LEDDDR |= (1 << LEDDDRPIN);

	for (;;)
		;
}
