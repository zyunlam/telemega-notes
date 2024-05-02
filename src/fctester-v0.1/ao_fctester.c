/*
 * Copyright © 2024 Bdale Garbee <bdale@gag.com>
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

// all FET switches assumed to be on the same port
int	 fets[] = { FET_A, FET_B, FET_C, FET_D, FET_E, FET_F };

int howmanyon = 0;

// switch selected fet to selected state, turn red LED on if any FETs are on
static void
ao_fet_control(uint32_t output, uint8_t value)
{
	switch (value) {
	case 1:
		ao_led_on((AO_PORT_TYPE) fets[output]);
		howmanyon++;
		break;
	default:
		ao_led_off((AO_PORT_TYPE) fets[output]);
		howmanyon--;
	}
	if (howmanyon) ao_led_on(AO_LED_RED); else ao_led_off(AO_LED_RED);
}

static void
ao_fet_on(void)
{
	uint32_t output;

	output = ao_cmd_decimal();
        if (ao_cmd_status != ao_cmd_success)
                return;
	if (output > 5)		// can't be less than 0 since unsigned! 
		printf ("Invalid FET selection %lu, must be 0..5\n", output);
	else 
		ao_fet_control(output, 1);
}

static void
ao_fet_off(void)
{
	uint32_t output;

	output = ao_cmd_decimal();
        if (ao_cmd_status != ao_cmd_success)
                return;
	if (output > 5)		// can't be less than 0 since unsigned! 
		printf ("Invalid FET selection %lu, must be 0..5\n", output);
	else 
		ao_fet_control(output, 0);
}

static const struct ao_cmds ao_fet_cmds[] = {
	{ ao_fet_on,  "S <output>\0Set (turn on) FET" },
	{ ao_fet_off, "R <output>\0Reset (turn off) FET" },
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
