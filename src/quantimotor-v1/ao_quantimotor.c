/*
 * Copyright © 2024 Bdale Garbee <bdale@gag.com>
 * GPLv3
 */

#include <ao.h>
#include <ao_exti.h>
#include <ao_serial.h>

int
main(void)
{
	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_exti_init();

	ao_adc_init();

	ao_usb_init();
	ao_serial_init();

	ao_cmd_init();
	ao_config_init();

	ao_start_scheduler();
}
