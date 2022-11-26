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
#include <ao_led.h>
#include <ao_dma_samd21.h>
#include <ao_exti.h>

static int	pressed;

static void
ao_button_callback(void)
{
	pressed = 1;
	ao_wakeup(&pressed);
}

static void
ao_beep_test(void)
{
	AO_BEEP_TCC->ctrlbset = (SAMD21_TCC_CTRLB_CMD_READSYNC << SAMD21_TCC_CTRLB_CMD);
	printf("pressed timer %ld\n", AO_BEEP_TCC->count);
	fflush(stdout);
	ao_beep_for(AO_BEEP_MID_DEFAULT, AO_MS_TO_TICKS(200));
}

static void
ao_button(void)
{
	ao_exti_setup(&samd21_port_a, 10, AO_EXTI_MODE_FALLING | AO_EXTI_MODE_PULL_UP, ao_button_callback);
	ao_exti_enable(&samd21_port_a, 10);
	for (;;) {
		ao_arch_block_interrupts();
		pressed = 0;
		while (!pressed)
			ao_sleep(&pressed);
		ao_arch_release_interrupts();
		ao_beep_test();
	}
}

static struct ao_task ao_button_task;

const struct ao_cmds ao_test_cmds[] = {
	{ ao_beep_test,	"b \0beep" },
	{ 0, NULL },
};

int main(void)
{
	ao_led_init();
	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_dma_init();
	ao_exti_init();
	ao_spi_init();
	ao_adc_init();
	ao_serial_init();
	ao_gps_init();
	ao_beep_init();
	ao_usb_init();
	ao_storage_init();
	ao_cmd_init();

	ao_cmd_register(ao_test_cmds);
	ao_add_task(&ao_button_task, ao_button, "button");
	ao_start_scheduler();

	return 0;
}
