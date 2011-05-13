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

#define AO_STACK_SIZE	128

uint8_t	new_stack[512];
int	stack_count;

#define PUSH8(stack, val)	(*--(stack) = (val))

#define PUSH16(stack, val)	PUSH8(stack, ((uint16_t) (val))); PUSH8(stack, ((uint16_t) (val)) >> 8)

void
count(int count)
{

}

void
blink(void)
{
	while (1) {
		LEDPORT ^= (1 << LEDOUT);
		_delay_ms(200);
	}
}

void
function(void)
{
	return;
}

void
init_stack(void (*f) (void))
{
	uint8_t		*stack = new_stack + AO_STACK_SIZE;
	uint8_t		i;

	/* Return address */
	PUSH16(stack, f);

#if 0
	/* Clear register values */
	i = 32;
	while (i--)
		PUSH8(stack, 0);

	/* SREG with interrupts enabled */
	PUSH8(stack, 0x80);
#endif
	stack_count = stack - new_stack;
}

void
switch_stack(void) __attribute__((naked));

void
switch_stack(void)
{
#if 0
	uint16_t	a;
	uint8_t		l, h;

	a = (uint16_t) function;
	l = a;
	h = a >> 8;
	asm("push %0" : : "r" (l));
	asm("push %0" : : "r" (h));
	asm("ret");
#endif
#if 0
	asm("push r31; push r30");
	asm("push r29; push r28; push r27; push r26; push r25");
	asm("push r24; push r23; push r22; push r21; push r20");
	asm("push r19; push r18; push r17; push r16; push r15");
	asm("push r14; push r13; push r12; push r11; push r10");
	asm("push r9; push r8; push r7; push r6; push r5");
	asm("push r4; push r3; push r2; push r1; push r0");
	asm("in r0, __SREG__" "\n\t"
	    "push r0");
#endif
	uint8_t	*sp = (new_stack + stack_count);
	uint8_t	sp_l, sp_h;

	sp_l = (uint16_t) sp;
	sp_h = ((uint16_t) sp) >> 8;
	asm("out __SP_H__,%0" : : "r" (sp_h) );
	asm("out __SP_L__,%0" : : "r" (sp_l) );
	blink();
#if 0
	asm("pop r0"	"\n\t"
	    "out __SREG__, r0");
	asm("pop r0; pop r1; pop r2; pop r3; pop r4");
	asm("pop r5; pop r6; pop r7; pop r8; pop r9");
	asm("pop r10; pop r11; pop r12; pop r13; pop r14");
	asm("pop r15; pop r16; pop r17; pop r18; pop r19");
	asm("pop r20; pop r21; pop r22; pop r23; pop r24");
	asm("pop r25; pop r26; pop r27; pop r28; pop r29");
	asm("pop r30; pop r31");
#endif
	asm("ret");
}

void back(void)
{
	switch_stack();
	blink();
}

void main(void)
{
#if 0
	clock_init();
	adc_init();
	timer1_init();
	timer0_init();
#endif

	LEDDDR |= (1 << LEDDDRPIN);

	init_stack(blink);
	switch_stack();
}
