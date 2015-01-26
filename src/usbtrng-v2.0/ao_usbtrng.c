/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

#include <ao.h>

static void ao_blink(void)
{
	for (;;) {
		ao_led_toggle(LEDS_AVAILABLE);
		ao_delay(AO_MS_TO_TICKS(500));
	}
}

struct ao_task	blink_task;

void main(void)
{
	ao_led_init(LEDS_AVAILABLE);
	ao_led_on(AO_LED_RED);
	ao_clock_init();
	ao_task_init();
	ao_timer_init();

	ao_cmd_init();

	ao_usb_init();

	ao_add_task(&blink_task, ao_blink, "blink");

	ao_start_scheduler();
}
