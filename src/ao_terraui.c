/*
 * Copyright © 2010 Keith Packard <keithp@keithp.com>
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

static __code uint8_t ready_beeps[] = {
	AO_BEEP_g,	AO_MS_TO_TICKS(100),
	AO_BEEP_bb,	AO_MS_TO_TICKS(100),
	AO_BEEP_dd,	AO_MS_TO_TICKS(100),
	AO_BEEP_gg,	AO_MS_TO_TICKS(200),
	AO_BEEP_dd,	AO_MS_TO_TICKS(100),
	AO_BEEP_gg,	AO_MS_TO_TICKS(400),
};

#define NUM_READY	(sizeof(ready_beeps) / 2)

__xdata uint8_t	ao_terraui_wakeup;

void
ao_terraui(void)
{
	uint8_t	i;

	for (i = 0; i < sizeof(ready_beeps); i += 2)
		ao_beep_for(ready_beeps[i], ready_beeps[i+1]);
	for (;;)
		ao_sleep(&ao_terraui_wakeup);
}

static __xdata struct ao_task	terraui_task;

void
ao_terraui_init(void)
{
	ao_add_task(&terraui_task, ao_terraui, "terraui");
}
