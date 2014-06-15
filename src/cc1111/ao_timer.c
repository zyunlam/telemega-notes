/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
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

volatile __data uint16_t ao_tick_count;

uint16_t ao_time(void) __critical
{
	return ao_tick_count;
}

#define T1_CLOCK_DIVISOR	8	/* 24e6/8 = 3e6 */
#define T1_SAMPLE_TIME		30000	/* 3e6/30000 = 100 */

#if HAS_ADC
volatile __data uint8_t	ao_adc_interval = 1;
volatile __data uint8_t	ao_adc_count;
#endif

void ao_timer_isr(void) __interrupt 9
{
	++ao_tick_count;
#if HAS_ADC
	if (++ao_adc_count == ao_adc_interval) {
		ao_adc_count = 0;
		ao_adc_poll();
#if (AO_DATA_ALL & ~(AO_DATA_ADC))
		ao_wakeup(DATA_TO_XDATA(&ao_adc_count));
#endif
	}
#endif
}

#if HAS_ADC
void
ao_timer_set_adc_interval(uint8_t interval) __critical
{
	ao_adc_interval = interval;
	ao_adc_count = 0;
}
#endif

void
ao_timer_init(void)
{
	/* NOTE:  This uses a timer only present on cc1111 architecture. */

	/* disable timer 1 */
	T1CTL = 0;

	/* set the sample rate */
	T1CC0H = T1_SAMPLE_TIME >> 8;
	T1CC0L = (uint8_t) T1_SAMPLE_TIME;

	T1CCTL0 = T1CCTL_MODE_COMPARE;
	T1CCTL1 = 0;
	T1CCTL2 = 0;

	/* clear timer value */
	T1CNTL = 0;

	/* enable overflow interrupt */
	OVFIM = 1;
	/* enable timer 1 interrupt */
	T1IE = 1;

	/* enable timer 1 in module mode, dividing by 8 */
	T1CTL = T1CTL_MODE_MODULO | T1CTL_DIV_8;
}

#ifndef NEEDS_CC1111_CLOCK_HACK
#define NEEDS_CC1111_CLOCK_HACK		1
#endif

#if NEEDS_CC1111_CLOCK_HACK
static void
ao_clock_delay(void)
{
	uint16_t	i = 0;

	while (--i)
		ao_arch_nop();
}
#endif

/*
 * AltOS always cranks the clock to the max frequency
 */
void
ao_clock_init(void)
{
#if NEEDS_CC1111_CLOCK_HACK
	/* Power up both oscillators */
	SLEEP &= ~(SLEEP_OSC_PD);

	/* Switch to the HFRC oscillator */
	CLKCON = (CLKCON & ~CLKCON_OSC_MASK) | (CLKCON_OSC_RC);

	/* Wait for the HFRC oscillator to be stable */
	while (!(SLEEP & SLEEP_HFRC_STB))
		;

	/* Delay for 'a while' waiting for the crystal to
	 * stabilize -- the XOSC_STB bit isn't reliable
	 *
	 *  http://www.ti.com/lit/er/swrz022c/swrz022c.pdf
	 */

	ao_clock_delay();
#endif

	/* Switch system clock to crystal oscilator */
	CLKCON = (CLKCON & ~CLKCON_OSC_MASK) | (CLKCON_OSC_XTAL);

	/* Wait for the HFRC oscillator to be stable */
	while (!(SLEEP & SLEEP_XOSC_STB))
		;

	/* Power down the unused HFRC oscillator */
	SLEEP |= SLEEP_OSC_PD;

	/* Crank up the timer tick and system clock speed */
	CLKCON = ((CLKCON & ~(CLKCON_TICKSPD_MASK | CLKCON_CLKSPD_MASK)) |
		  (CLKCON_TICKSPD_1 | CLKCON_CLKSPD_1));

	while ((CLKCON & (CLKCON_TICKSPD_MASK|CLKCON_CLKSPD_MASK)) !=
	       (CLKCON_TICKSPD_1 | CLKCON_CLKSPD_1))
		;
}
