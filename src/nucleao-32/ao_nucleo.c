/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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
#include <ao_lisp.h>

static uint16_t	blink_delay, blink_running;

static void blink(void) {
	blink_running = 1;
	while (blink_delay) {
		ao_led_on(AO_LED_GREEN);
		ao_delay(blink_delay);
		ao_led_off(AO_LED_GREEN);
		ao_delay(blink_delay);
	}
	blink_running = 0;
	ao_wakeup(&blink_running);
	ao_exit();
}

struct ao_task blink_task;

static void blink_cmd() {
	ao_cmd_decimal();
	blink_delay = ao_cmd_lex_i;
	if (blink_delay && !blink_running)
		ao_add_task(&blink_task, blink, "blink");
	if (!blink_delay)
		while (blink_running)
			ao_sleep(&blink_running);
}

static void lisp_cmd() {
	ao_lisp_read_eval_print();
}

static const struct ao_cmds blink_cmds[] = {
	{ blink_cmd,	"b <delay, 0 off>\0Blink the green LED" },
	{ lisp_cmd,	"l\0Run lisp interpreter" },
	{ 0, 0 }
};


void main(void)
{
	ao_led_init(LEDS_AVAILABLE);
	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_dma_init();
	ao_usb_init();
	ao_serial_init();
	ao_cmd_init();
	ao_cmd_register(blink_cmds);
	ao_start_scheduler();
}


