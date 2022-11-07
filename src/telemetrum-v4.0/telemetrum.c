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

int main(void)
{
	ao_led_init();
	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_dma_init();
	ao_exti_init();
	ao_spi_init();
	ao_serial_init();
	ao_usb_init();
	ao_cmd_init();
	ao_start_scheduler();
	return 0;
}
