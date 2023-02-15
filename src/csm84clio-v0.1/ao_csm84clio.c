/*
 * Copyright © 2023 Bdale Garbee <bdale@gag.com>
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

/*
 * define the FET outputs
 */

typedef struct 
{
	struct stm_gpio *	port;
	int			pin;
} fet;

fet outputs[] = {
	{&stm_gpioe, 9},	/* step_0 */
	{&stm_gpioe, 13},	/* step_1 */
	{&stm_gpiob, 11},	/* step_2 */
	{&stm_gpioe, 8},	/* step_3 */
	{&stm_gpioe, 12},	/* step_4 */
	{&stm_gpioe, 10},	/* step_5 */
	{&stm_gpioe, 14},	/* step_6 */
	{&stm_gpiob, 10},	/* step_7 */
	{&stm_gpioe, 11},	/* step_8 */
	{&stm_gpioe, 15},	/* step_9 */
	{&stm_gpioa, 5},	/* step_10 */
	{&stm_gpioc, 5},	/* step_11 */
	{&stm_gpioe, 7},	/* step_12 */
	{&stm_gpioa, 6},	/* step_13 */
	{&stm_gpiob, 0},	/* step_14 */
	{&stm_gpioa, 3},	/* step_15 */
	{&stm_gpioa, 7},	/* step_16 */
	{&stm_gpiob, 1},	/* step_17 */
	{&stm_gpioa, 4},	/* step_18 */
	{&stm_gpioc, 4},	/* step_19 */
	{&stm_gpioe, 4},	/* step_20 */
	{&stm_gpioc, 0},	/* step_21 */
	{&stm_gpioc, 3},	/* step_22 */
	{&stm_gpioe, 5},	/* step_23 */
	{&stm_gpioc, 1},	/* step_24 */
	{&stm_gpioe, 2},	/* step_25 */
	{&stm_gpioc, 14},	/* step_26 */
	{&stm_gpioc, 2},	/* step_27 */
	{&stm_gpioe, 3},	/* step_28 */
	{&stm_gpioc, 15},	/* step_29 */
	{&stm_gpiob, 3},	/* step_30 */
	{&stm_gpiob, 5},	/* step_31 */
	{&stm_gpiob, 9},	/* step_32 */
	{&stm_gpiob, 4},	/* step_33 */
	{&stm_gpiob, 6},	/* step_34 */
	{&stm_gpiod, 7},	/* step_35 */
	{&stm_gpiob, 7},	/* step_36 */
	{&stm_gpioe, 0},	/* step_37 */
	{&stm_gpiod, 6},	/* step_38 */
	{&stm_gpiob, 8},	/* step_39 */
	{&stm_gpioc, 7},	/* step_40 */
	{&stm_gpiod, 1},	/* step_41 */
	{&stm_gpiod, 4},	/* step_42 */
	{&stm_gpioc, 6},	/* step_43 */
	{&stm_gpiod, 0},	/* step_44 */
	{&stm_gpioc, 8},	/* step_45 */
	{&stm_gpiod, 2},	/* step_46 */
	{&stm_gpiod, 5},	/* step_47 */
	{&stm_gpioc, 9},	/* step_48 */
	{&stm_gpiod, 3}};	/* step_49 */

static void
ao_fet_control(uint32_t output, uint8_t value)
{
	/* map output 0-49 to corresponding GPIO port and pin, set to value */
	ao_gpio_set(outputs[output].port, outputs[output].pin, value);
}

static void
ao_fet_init(void)
{
	int i;

	/* initialize GPIO outputs and turn them all off */
	for (i = 0; i < 50; i++) 
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
	if (output > 49)		/* can't be less than 0 since unsigned! */
		printf ("Invalid FET selection %lu, must be 0..49\n", output);
	else {
		ao_fet_control(output, 1);
		ao_led_on(AO_LED_RED);
	}
}

static void
ao_fet_off(void)
{
	uint32_t output;

	output = ao_cmd_decimal();
        if (ao_cmd_status != ao_cmd_success)
                return;
	if (output > 49)		/* can't be less than 0 since unsigned! */
		printf ("Invalid FET selection %lu, must be 0..49\n", output);
	else {
		ao_fet_control(output, 0);
		ao_led_off(AO_LED_RED);
	}
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

	ao_serial_init();
	ao_timer_init();
	ao_dma_init();
	ao_adc_init();
	ao_cmd_init();
	ao_usb_init();

	ao_cmd_register(ao_fet_cmds);

	/* turn red off, leave green on as a "power indicator" */
	ao_led_off(AO_LED_RED);

	ao_start_scheduler();
}
