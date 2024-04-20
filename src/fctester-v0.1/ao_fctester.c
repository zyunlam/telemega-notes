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
#include <ao_led.h>

/*
 * define the FET outputs
 */

#define NUM_FETS	7
int on_count = 0;

typedef struct 
{
	int	port;
	int	pin;
} fet;

fet outputs[] = {
	{0, 11},		/* test_a */
	{0, 22},		/* test_b */
	{0, 12},		/* test_c */
	{0, 9},			/* test_d */
	{0, 13},		/* test_e */
	{0, 16},		/* test_f */
	{1, 19},		/* short  */
};

static void
ao_fet_control(uint32_t output, uint8_t value)
{
	/* map output to corresponding GPIO port and pin, set to value */
	ao_gpio_set(outputs[output].port, outputs[output].pin, value);
}

static void
ao_fet_init(void)
{
	int i;

	/* initialize GPIO outputs and turn them all off */
	for (i = 0; i < NUM_FETS; i++) 
	{
		ao_enable_output(outputs[i].port, outputs[i].pin, 0);
	}
}

static void
ao_fet_on(void)
{
	uint32_t output;

	output = ao_cmd_decimal();
        if (ao_cmd_status != ao_cmd_success)
                return;
	if (output > NUM_FETS-1)	/* can't be < 0 since unsigned! */
		printf ("Invalid FET %lu, must be 0..%u\n", output, NUM_FETS);
	else {
		ao_fet_control(output, 1);
		on_count++;
	}
	if (on_count > 0) ao_led_on(AO_LED_RED);
}

static void
ao_fet_off(void)
{
	uint32_t output;

	output = ao_cmd_decimal();
        if (ao_cmd_status != ao_cmd_success)
                return;
	if (output > NUM_FETS-1)	/* can't be < 0 since unsigned! */
		printf ("Invalid FET %lu, must be 0..%u\n", output, NUM_FETS);
	else {
		ao_fet_control(output, 0);
		on_count--;
	}
	if (on_count == 0) ao_led_off(AO_LED_RED);
}

static const struct ao_cmds ao_fet_cmds[] = {
	{ ao_fet_on,  "S <output>\0Set (turn on) FET" },
	{ ao_fet_off, "R <output>\0Reset (turn off) FET" },
	{ 0, NULL }
};

int
main(void)
{
	ao_fet_init();		/* turn all outputs off ASAP */
	ao_clock_init();

	ao_task_init();
	ao_led_init();

	/* both LEDs on briefly as system test */
	ao_led_on(LEDS_AVAILABLE);

	ao_timer_init();
	ao_adc_init();
	ao_cmd_init();
	ao_usb_init();

	ao_cmd_register(ao_fet_cmds);

	/* turn red off, leave green on as a "power indicator" */
	ao_led_off(AO_LED_RED);

	ao_start_scheduler();
}
