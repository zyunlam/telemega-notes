/*
 * Copyright © 2022 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
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
#include "ao_beep.h"

#define BEEP_SCALE	(AO_SYSCLK / 750000)

void
ao_beep(uint8_t beep)
{
	struct samd21_tcc *tcc = AO_BEEP_TCC;
	if (beep) {
		tcc->per = (beep * BEEP_SCALE);
		tcc->ctrla = (1 << SAMD21_TCC_CTRLA_ENABLE);
	} else {
		tcc->ctrla = (0 << SAMD21_TCC_CTRLA_ENABLE);
	}
}

void
ao_beep_for(uint8_t beep, AO_TICK_TYPE ticks)
{
	ao_beep(beep);
	ao_delay(ticks);
	ao_beep(0);
}

static void
ao_tcc_init(struct samd21_tcc *tcc, uint32_t apbcmask)
{
	samd21_pm.apbcmask |= apbcmask;

	/* Reset the device */
	tcc->ctrla = (1 << SAMD21_TCC_CTRLA_SWRST);

	while ((tcc->ctrla & (1 << SAMD21_TCC_CTRLA_SWRST)) != 0 ||
	       (tcc->syncbusy & (1 << SAMD21_TCC_SYNCBUSY_SWRST)) != 0)
		;

	tcc->per = 94 * BEEP_SCALE;

	tcc->wave = ((SAMD21_TCC_WAVE_WAVEGEN_NFRQ << SAMD21_TCC_WAVE_WAVEGEN) |
		     (0 << SAMD21_TCC_WAVE_RAMP) |
		     (0 << SAMD21_TCC_WAVE_CIPEREN) |
		     (0 << SAMD21_TCC_WAVE_CCCEN(0)) |
		     (0 << SAMD21_TCC_WAVE_CCCEN(1)) |
		     (0 << SAMD21_TCC_WAVE_CCCEN(2)) |
		     (0 << SAMD21_TCC_WAVE_CCCEN(3)) |
		     (0 << SAMD21_TCC_WAVE_POL(0)) |
		     (0 << SAMD21_TCC_WAVE_POL(1)) |
		     (0 << SAMD21_TCC_WAVE_POL(1)) |
		     (0 << SAMD21_TCC_WAVE_POL(3)) |
		     (0 << SAMD21_TCC_WAVE_SWAP(0)) |
		     (0 << SAMD21_TCC_WAVE_SWAP(1)) |
		     (0 << SAMD21_TCC_WAVE_SWAP(1)) |
		     (0 << SAMD21_TCC_WAVE_SWAP(3)));

	tcc->dbgctrl = (1 << SAMD21_TCC_DBGCTRL_DBGRUN);
}

void
ao_beep_init(void)
{
	struct samd21_port	*port = AO_BEEP_PORT;
	uint8_t			pin = AO_BEEP_PIN;
	struct samd21_tcc 	*tcc = AO_BEEP_TCC;
	uint32_t		apbc_mask = 1UL << AO_BEEP_TCC_APBC_MASK;

	if (tcc == &samd21_tcc0 || tcc == &samd21_tcc1) {
		samd21_gclk_clkctrl(0, SAMD21_GCLK_CLKCTRL_ID_TCC0_TCC1);
	} else {
		samd21_gclk_clkctrl(0, SAMD21_GCLK_CLKCTRL_ID_TCC2_TC3);
	}

	ao_tcc_init(tcc, apbc_mask);

	ao_enable_output(port, pin, 0);

	samd21_port_pmux_set(port, pin, AO_BEEP_FUNC);
}
