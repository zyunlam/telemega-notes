/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

#include "ao.h"
#include <ao_log.h>
#include <ao_data.h>
#include <ao_flight.h>

#if HAS_ADC
static uint8_t	ao_log_data_pos;

/* a hack to make sure that ao_log_metrums fill the eeprom block in even units */
typedef uint8_t check_log_size[1-(256 % sizeof(struct ao_log_metrum))] ;

#ifndef AO_SENSOR_INTERVAL_ASCENT
#define AO_SENSOR_INTERVAL_ASCENT	1
#define AO_SENSOR_INTERVAL_DESCENT	10
#define AO_OTHER_INTERVAL		32
#endif

void
ao_log(void)
{
	uint16_t	next_sensor, next_other;

	ao_storage_setup();

	ao_log_scan();

	while (!ao_log_running)
		ao_sleep(&ao_log_running);

#if HAS_FLIGHT
	ao_log_data.type = AO_LOG_FLIGHT;
	ao_log_data.tick = (uint16_t) ao_sample_tick;
#if HAS_ACCEL
	ao_log_data.u.flight.ground_accel = ao_ground_accel;
#endif
	ao_log_data.u.flight.ground_pres = ao_ground_pres;
	ao_log_data.u.flight.flight = ao_flight_number;
	ao_log_write(&ao_log_data);
#endif

	/* Write the whole contents of the ring to the log
	 * when starting up.
	 */
	ao_log_data_pos = ao_data_ring_next(ao_data_head);
	next_other = next_sensor = ao_data_ring[ao_log_data_pos].tick;
	ao_log_state = ao_flight_startup;
	for (;;) {
		/* Write samples to EEPROM */
		while (ao_log_data_pos != ao_data_head) {
			AO_TICK_TYPE tick = ao_data_ring[ao_log_data_pos].tick;
			ao_log_data.tick = (uint16_t) tick;
			if ((AO_TICK_SIGNED) (tick - next_sensor) >= 0) {
				ao_log_data.type = AO_LOG_SENSOR;
#if HAS_MS5607
				ao_log_data.u.sensor.pres = ao_data_ring[ao_log_data_pos].ms5607_raw.pres;
				ao_log_data.u.sensor.temp = ao_data_ring[ao_log_data_pos].ms5607_raw.temp;
#endif
#if HAS_ACCEL
				ao_log_data.u.sensor.accel = ao_data_accel(&ao_data_ring[ao_log_data_pos]);
#endif
				ao_log_write(&ao_log_data);
				if (ao_log_state <= ao_flight_coast)
					next_sensor = tick + AO_SENSOR_INTERVAL_ASCENT;
				else
					next_sensor = tick + AO_SENSOR_INTERVAL_DESCENT;
			}
			if ((AO_TICK_SIGNED) (tick - next_other) >= 0) {
				ao_log_data.type = AO_LOG_TEMP_VOLT;
				ao_log_data.u.volt.v_batt = ao_data_ring[ao_log_data_pos].adc.v_batt;
				ao_log_data.u.volt.sense_a = ao_data_ring[ao_log_data_pos].adc.sense_a;
				ao_log_data.u.volt.sense_m = ao_data_ring[ao_log_data_pos].adc.sense_m;
				ao_log_write(&ao_log_data);
				next_other = tick + AO_OTHER_INTERVAL;
			}
			ao_log_data_pos = ao_data_ring_next(ao_log_data_pos);
		}
#if HAS_FLIGHT
		/* Write state change to EEPROM */
		if (ao_flight_state != ao_log_state) {
			ao_log_state = ao_flight_state;
			ao_log_data.type = AO_LOG_STATE;
			ao_log_data.tick = (uint16_t) ao_time();
			ao_log_data.u.state.state = ao_log_state;
			ao_log_data.u.state.reason = 0;
			ao_log_write(&ao_log_data);

			if (ao_log_state == ao_flight_landed)
				ao_log_stop();
		}
#endif

		ao_log_flush();

		/* Wait for a while */
		ao_delay(AO_MS_TO_TICKS(100));

		/* Stop logging when told to */
		while (!ao_log_running)
			ao_sleep(&ao_log_running);
	}
}
#endif
