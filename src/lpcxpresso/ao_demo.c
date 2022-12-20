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
	uint8_t	reg[2] = { 0, 0 };
	uint8_t	val[1];
	unsigned i = 0x0;
	unsigned j;
	uint8_t success;

	i = ao_cmd_hex();
	if (ao_cmd_status == ao_cmd_lex_error) {
		i = 0;
		ao_cmd_status = ao_cmd_success;
	}
	reg[0] = (uint8_t) (i >> 8);
	reg[1] = (uint8_t) i;
	ao_i2c_get(0);
	ao_i2c_start(0, 0x29<<1);
	success = ao_i2c_send(&reg, 2, 0, 0) && ao_i2c_recv(val, sizeof(val), 0, 1);
	ao_i2c_put(0);
	if (!success) {
		printf("i2c transaction failed\n");
		return;
	}
	for (j = 0; j < sizeof(val); j++)
		printf("reg 0x%04x = 0x%02x\n", i+j, val[j]);
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
