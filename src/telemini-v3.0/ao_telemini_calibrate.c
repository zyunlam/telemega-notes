/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
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

static void
ao_cal(void)
{
	ao_config_get();
	ao_beep_for(AO_BEEP_MID, AO_MS_TO_TICKS(500));
	for (;;) {
		ao_led_on(AO_LED_GREEN);
		ao_radio_test_on();
		ao_delay(AO_SEC_TO_TICKS(30));
		ao_led_off(AO_LED_GREEN);
		ao_radio_test_off();
		ao_delay(AO_SEC_TO_TICKS(5));
	}
}

static struct ao_task ao_cal_task;

void
main(void)
{
	ao_clock_init();
	ao_task_init();
	ao_led_init(LEDS_AVAILABLE);
	ao_led_on(AO_LED_GREEN);
	ao_timer_init();

	ao_dma_init();
	ao_spi_init();
	ao_exti_init();

//	ao_adc_init();
	ao_beep_init();
//	ao_cmd_init();
//	ao_ms5607_init();

	ao_storage_init();
//	ao_flight_init();
//	ao_log_init();
//	ao_report_init();
//	ao_telemetry_init();

	ao_radio_init();
//	ao_packet_slave_init(TRUE);
//	ao_igniter_init();

	ao_config_init();

	ao_add_task(&ao_cal_task, ao_cal, "radio_cal");

	ao_start_scheduler();
}
