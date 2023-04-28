/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

#include <ao.h>
#include <ao_usb.h>

#if 0
static void
hello(void)
{
	for (;;) {
		const char *s = "hello, world\r\n";
		char c;
		while ((c = *s++))
			ao_serial0_putchar(c);
	}
}

struct ao_task hello_task;
#endif

int
main(void)
{
	ao_clock_init();
	ao_timer_init();

	ao_led_init();
	ao_led_on(AO_LED_RED);

	ao_task_init();

//	ao_add_task(&hello_task, hello, "hello");

	ao_serial_init();
	ao_usb_init();
	ao_cmd_init();

	ao_start_scheduler();
}
