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

#define _cat2(a,b) a##b
#define cat2(a,b) _cat2(a,b)
#define _cat4(a,b,c,d) a##b##c##d
#define cat4(a,b,c,d) _cat4(a,b,c,d)
#define _cat8(a,b,c,d,e,f,g,h) a##b##c##d##e##f##g##h
#define cat8(a,b,c,d,e,f,g,h) _cat8(a,b,c,d,e,f,g,h)

#ifndef AO_LPC_BEEP_TIMER
#define AO_LPC_BEEP_TIMER 1
#define AO_LPC_BEEP_CHANNEL 1
#define AO_LPC_BEEP_PORT	0
#define AO_LPC_BEEP_PIN		14
#endif

#define AO_LPC_CT_BEEP 		cat2(lpc_ct32b, AO_LPC_BEEP_TIMER)
#define AO_LPC_CT_BEEP_CLKCTRL	cat2(LPC_SCB_SYSAHBCLKCTRL_CT32B, AO_LPC_BEEP_TIMER)
#define AO_LPC_CT_BEEP_EMR	cat2(LPC_CT32B_EMR_EMC, AO_LPC_BEEP_CHANNEL)
#define AO_LPC_CT_BEEP_MR	AO_LPC_BEEP_CHANNEL
#define AO_LPC_CT_BEEP_PWMC	cat2(LPC_CT32B_PWMC_PWMEN, AO_LPC_BEEP_CHANNEL)
#define AO_LPC_CT_BEEP_IOCONF	cat4(pio,AO_LPC_BEEP_PORT,_,AO_LPC_BEEP_PIN)
#define AO_LPC_CT_BEEP_FUNC	cat8(LPC_IOCONF_FUNC_PIO,AO_LPC_BEEP_PORT,_,AO_LPC_BEEP_PIN,_CT32B,AO_LPC_BEEP_TIMER,_MAT,AO_LPC_BEEP_CHANNEL)

void
ao_beep(uint8_t beep)
{
	if (beep == 0) {
		AO_LPC_CT_BEEP.tcr = ((0 << LPC_CT32B_TCR_CEN) |
				  (1 << LPC_CT32B_TCR_CRST));
		lpc_scb.sysahbclkctrl &= ~(1UL << AO_LPC_CT_BEEP_CLKCTRL);
	} else {
		lpc_scb.sysahbclkctrl |= (1UL << AO_LPC_CT_BEEP_CLKCTRL);

		/* Set prescaler to match cc1111 clocks
		 */
		AO_LPC_CT_BEEP.pr = AO_LPC_SYSCLK / 750000 - 1;

		/* Write the desired data in the match registers */

		/* Reset after two time units */
		AO_LPC_CT_BEEP.mr[0] = beep << 1;

		/* PWM width is half of that */
		AO_LPC_CT_BEEP.mr[AO_LPC_CT_BEEP_MR] = beep;

		/* Flip output on PWM match */
		AO_LPC_CT_BEEP.emr = (LPC_CT32B_EMR_EMC_TOGGLE << AO_LPC_CT_BEEP_EMR);

		/* Reset on match 0 */
		AO_LPC_CT_BEEP.mcr = (1 << LPC_CT32B_MCR_MR0R);

		/* PWM on match */
		AO_LPC_CT_BEEP.pwmc = (1 << AO_LPC_CT_BEEP_PWMC);

		/* timer mode */
		AO_LPC_CT_BEEP.ctcr = 0;

		/* And turn the timer on */
		AO_LPC_CT_BEEP.tcr = ((1 << LPC_CT32B_TCR_CEN) |
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
	/* Our beeper is on c32b1_mat1
	 * which is on pin pio0_14
	 */

	lpc_ioconf.AO_LPC_CT_BEEP_IOCONF = ((AO_LPC_CT_BEEP_FUNC << LPC_IOCONF_FUNC) |
			      (LPC_IOCONF_MODE_INACTIVE << LPC_IOCONF_MODE) |
			      (0 << LPC_IOCONF_HYS) |
			      (0 << LPC_IOCONF_INV) |
			      (1 << LPC_IOCONF_ADMODE) |
			      (0 << LPC_IOCONF_OD));

	lpc_scb.sysahbclkctrl |= (1 << AO_LPC_CT_BEEP_CLKCTRL);

	/* Disable the counter and reset the value */
	AO_LPC_CT_BEEP.tcr = ((0 << LPC_CT32B_TCR_CEN) |
			  (1 << LPC_CT32B_TCR_CRST));
}
