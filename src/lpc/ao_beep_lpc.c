/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#define _cat(a,b) a##b
#define cat(a,b) _cat(a,b)
#define _cat4(a,b,c,d) a##b##c##d
#define cat4(a,b,c,d) _cat4(a,b,c,d)
#define _cat8(a,b,c,d,e,f,g,h) a##b##c##d##e##f##g##h
#define cat8(a,b,c,d,e,f,g,h) _cat8(a,b,c,d,e,f,g,h)

#define AO_TIMER_CLKCTRL	cat(LPC_SCB_SYSAHBCLKCTRL_CT32B, BEEPER_TIMER)
#define AO_TIMER		cat(lpc_ct32b, BEEPER_TIMER)
#define AO_TIMER_EMC 		cat(LPC_CT32B_EMR_EMC, BEEPER_OUTPUT)
#define AO_TIMER_PIO		cat4(pio, BEEPER_PORT, _, BEEPER_PIN)
/* LPC_IOCONF_FUNC_PIO0_14_CT32B1_MAT1 */
#define AO_TIMER_FUNC		cat8(LPC_IOCONF_FUNC_PIO, BEEPER_PORT, _, BEEPER_PIN, _CT32B, BEEPER_TIMER, _MAT, BEEPER_OUTPUT)

void
ao_beep(uint8_t beep)
{
	if (beep == 0) {
		AO_TIMER.tcr = ((0 << LPC_CT32B_TCR_CEN) |
				  (1 << LPC_CT32B_TCR_CRST));
		lpc_scb.sysahbclkctrl &= ~(1UL << AO_TIMER_CLKCTRL);
	} else {
		lpc_scb.sysahbclkctrl |= (1 << AO_TIMER_CLKCTRL);

		/* Set prescaler to match cc1111 clocks
		 */
		AO_TIMER.pr = AO_LPC_SYSCLK / 750000 - 1;

		/* Write the desired data in the match registers */

		/* Reset after two time units */
		AO_TIMER.mr[0] = beep << 1;

		/* PWM width is half of that */
		AO_TIMER.mr[1] = beep;

		/* Flip output 1 on PWM match */
		AO_TIMER.emr = (LPC_CT32B_EMR_EMC_TOGGLE << AO_TIMER_EMC);

		/* Reset on match 0 */
		AO_TIMER.mcr = (1 << LPC_CT32B_MCR_MR0R);

		/* PWM on match 1 */
		AO_TIMER.pwmc = (1 << LPC_CT32B_PWMC_PWMEN1);

		/* timer mode */
		AO_TIMER.ctcr = 0;

		/* And turn the timer on */
		AO_TIMER.tcr = ((1 << LPC_CT32B_TCR_CEN) |
				  (0 << LPC_CT32B_TCR_CRST));
	}
}

void
ao_beep_for(uint8_t beep, AO_TICK_TYPE ticks)
{
	ao_beep(beep);
	ao_delay(ticks);
	ao_beep(0);
}

void
ao_beep_init(void)
{
	lpc_ioconf.AO_TIMER_PIO = ((AO_TIMER_FUNC << LPC_IOCONF_FUNC) |
			      (LPC_IOCONF_MODE_INACTIVE << LPC_IOCONF_MODE) |
			      (0 << LPC_IOCONF_HYS) |
			      (0 << LPC_IOCONF_INV) |
			      (1 << LPC_IOCONF_ADMODE) |
			      (0 << LPC_IOCONF_OD));
}
