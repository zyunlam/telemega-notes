/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
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
#include <ao_task.h>

#ifndef HAS_TICK
#define HAS_TICK 1
#endif

volatile AO_TICK_TYPE ao_tick_count;

AO_TICK_TYPE
ao_time(void)
{
	return ao_tick_count;
}

uint64_t
ao_time_ns(void)
{
	AO_TICK_TYPE	before, after;
	uint32_t	val;

	do {
		before = ao_tick_count;
		val = stm_systick.val;
		after = ao_tick_count;
	} while (before != after);

	return (uint64_t) after * (1000000000ULL / AO_HERTZ) +
		(uint64_t) val * (1000000000ULL / AO_SYSTICK);
}

#if AO_DATA_ALL
volatile uint8_t	ao_data_interval = 1;
volatile uint8_t	ao_data_count;
#endif

void stm_systick_isr(void)
{
	if (stm_systick.ctrl & (1 << STM_SYSTICK_CTRL_COUNTFLAG)) {
		++ao_tick_count;
		ao_task_check_alarm();
#if AO_DATA_ALL
		if (++ao_data_count == ao_data_interval && ao_data_interval) {
			ao_data_count = 0;
#if HAS_FAKE_FLIGHT
			if (ao_fake_flight_active)
				ao_fake_flight_poll();
			else
#endif
				ao_adc_poll();
#if (AO_DATA_ALL & ~(AO_DATA_ADC))
			ao_wakeup((void *) &ao_data_count);
#endif
		}
#endif
#ifdef AO_TIMER_HOOK
		AO_TIMER_HOOK;
#endif
	}
}

#if HAS_ADC
void
ao_timer_set_adc_interval(uint8_t interval)
{
	ao_arch_critical(
		ao_data_interval = interval;
		ao_data_count = 0;
		);
}
#endif

#define SYSTICK_RELOAD (AO_SYSTICK / 100 - 1)

void
ao_timer_init(void)
{
	stm_systick.load = SYSTICK_RELOAD;
	stm_systick.val = 0;
	stm_systick.ctrl = ((1 << STM_SYSTICK_CTRL_ENABLE) |
			    (1 << STM_SYSTICK_CTRL_TICKINT) |
			    (STM_SYSTICK_CTRL_CLKSOURCE_HCLK_8 << STM_SYSTICK_CTRL_CLKSOURCE));
	stm_scb.shpr3 |= (uint32_t) AO_STM_NVIC_CLOCK_PRIORITY << 24;
}
