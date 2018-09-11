/*
 * Copyright © 2018 Keith Packard <keithp@keithp.com>
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
 */

#include <ao.h>

static struct ao_task red_task;
static struct ao_task green_task;

static void
red(void)
{
	for (;;) {
		ao_led_toggle(LED_RED);
		ao_delay(AO_MS_TO_TICKS(500));
	}
}

static void
green(void)
{
	for (;;) {
		ao_led_toggle(LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(450));
	}
}

void main(void)
{
	ao_clock_init();
	ao_timer_init();
	ao_led_init();
	ao_task_init();

	ao_add_task(&red_task, red, "red");
	ao_add_task(&green_task, green, "green");
	ao_start_scheduler();
}
