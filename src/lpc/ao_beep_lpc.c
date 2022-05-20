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

#ifdef AO_LPC_BEEP_TIMER
#define AO_LPC_BEEP_TIMER 1
#define AO_LPC_BEEP_CHANNEL 1
#endif

#ifndef AO_LPC_CT_BEEP
#define AO_LPC_CT_BEEP 		cat(lpc_ct32b, AO_LPC_BEEP_TIMER)
#define AO_LPC_CT_BEEP_CLKCTRL	cat(LPC_SCB_SYSAHBCLKCTRL_CT32B, AO_LPC_BEEP_TIMER)
#define AO_LPC_CT_BEEP_EMR	cat(LPC_CT32B_EMR_EMC, AO_LPC_BEEP_CHANNEL)
#define AO_LPC_CT_BEEP_MR	AO_LPC_BEEP_CHANNEL
#define AO_LPC_CT_BEEP_PWMC	cat(LPC_CT32B_PWMC_PWMEN, AO_LPC_BEEP_CHANNEL)
#endif

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

	lpc_ioconf.pio0_14 = ((LPC_IOCONF_FUNC_PIO0_14_CT32B1_MAT1 << LPC_IOCONF_FUNC) |
			      (LPC_IOCONF_MODE_INACTIVE << LPC_IOCONF_MODE) |
			      (0 << LPC_IOCONF_HYS) |
			      (0 << LPC_IOCONF_INV) |
			      (1 << LPC_IOCONF_ADMODE) |
			      (0 << LPC_IOCONF_OD));

	lpc_scb.sysahbclkctrl |= (1 << LPC_SCB_SYSAHBCLKCTRL_CT32B1);

	/* Disable the counter and reset the value */
	AO_LPC_CT_BEEP.tcr = ((0 << LPC_CT32B_TCR_CEN) |
			  (1 << LPC_CT32B_TCR_CRST));
}
