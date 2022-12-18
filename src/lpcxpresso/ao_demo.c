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
#include <ao_exti.h>
#include <ao_i2c.h>
#include <ao_i2c.h>

static void
ao_i2c_test(void)
{
	ao_i2c_get(0);
	ao_i2c_start(0, 0x44);
	ao_i2c_send("hello", 5, 0, 1);
	ao_i2c_put(0);
}

const struct ao_cmds ao_test_cmds[] = {
	{ ao_i2c_test,	"s \0Send some bytes over i2c" },
	{ 0, NULL },
};

int
main(void)
{
	ao_led_init();
	ao_led_on(AO_LED_RED);

	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_exti_init();

	ao_usb_init();
	ao_serial_init();
	ao_i2c_init();
	ao_cmd_init();
	ao_cmd_register(ao_test_cmds);
	ao_start_scheduler();
}
