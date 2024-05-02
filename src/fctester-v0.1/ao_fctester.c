/*
 * Copyright © 2014 Bdale Garbee <bdale@gag.com>
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

uint8_t		fet_output;

// switch fet to selected output, red LED on as a side effect
static void
ao_fet_control(uint8_t output)
{
	switch (output) {
	case 1:
		ao_led_on(FET_A);
		ao_led_on(AO_LED_RED);
		break;
	default:
		ao_led_off(FET_A);
		ao_led_off(AO_LED_RED);
	}
}

static void
ao_fet_select(void) 
{
	uint8_t output;

	output = (uint8_t) ao_cmd_decimal();
        if (ao_cmd_status != ao_cmd_success)
                return;
	if (output > 1) 
		printf ("Invalid fet position %u\n", output);
	else
		ao_fet_control(output);
}

static const struct ao_cmds ao_fet_cmds[] = {
	{ ao_fet_select, "R <output>\0Select fet output" },
	{ 0, NULL }
};

int
main(void)
{
	ao_clock_init();
	ao_task_init();
	ao_timer_init();

	ao_usb_init();

	ao_led_init();			// also handles FET switches 
	ao_led_on(AO_LED_GREEN);	// indicate we're alive

	ao_cmd_init();

	ao_cmd_register(ao_fet_cmds);

	ao_start_scheduler();
}
