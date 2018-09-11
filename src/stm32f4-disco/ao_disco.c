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

void main(void)
{
	float		x;
	int		r = 1;
	int		g = 0;

	ao_clock_init();

	ao_timer_init();

	ao_enable_output(LED_GREEN_PORT, LED_GREEN_PIN, 0);
	ao_enable_output(LED_RED_PORT, LED_RED_PIN, 1);
	for (;;) {
		ao_gpio_set(LED_GREEN_PORT, LED_GREEN_PIN, g);
		ao_gpio_set(LED_RED_PORT, LED_RED_PIN, r);
		g ^= 1;
		r ^= 1;
		for (x = 0.0f; x < 100000.0f; x = x + 0.1f)
			ao_arch_nop();
	}
}
