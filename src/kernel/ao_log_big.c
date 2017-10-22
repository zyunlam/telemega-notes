/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

static __data uint8_t	ao_log_data_pos;

/* a hack to make sure that ao_log_records fill the eeprom block in even units */
typedef uint8_t check_log_size[1-(256 % sizeof(struct ao_log_record))] ;

#ifndef AO_SENSOR_INTERVAL_ASCENT
#define AO_SENSOR_INTERVAL_ASCENT	1
#define AO_SENSOR_INTERVAL_DESCENT	10
#define AO_OTHER_INTERVAL		32
#endif

void
ao_log(void)
{
	__pdata uint16_t	next_sensor, next_other;

	ao_storage_setup();

	ao_log_scan();

	while (!ao_log_running)
		ao_sleep(&ao_log_running);

	log.type = AO_LOG_FLIGHT;
	log.tick = ao_sample_tick;
#if HAS_ACCEL
	log.u.flight.ground_accel = ao_ground_accel;
#endif
	log.u.flight.flight = ao_flight_number;
	ao_log_write(&log);

	/* Write the whole contents of the ring to the log
	 * when starting up.
	 */
	ao_log_data_pos = ao_data_ring_next(ao_sample_data);
	next_other = next_sensor = ao_data_ring[ao_log_data_pos].tick;
	ao_log_state = ao_flight_startup;
	for (;;) {
		/* Write samples to EEPROM */
		while (ao_log_data_pos != ao_sample_data) {
			log.tick = ao_data_ring[ao_log_data_pos].tick;
			if ((int16_t) (log.tick - next_sensor) >= 0) {
				log.type = AO_LOG_SENSOR;
				log.u.sensor.accel = ao_data_ring[ao_log_data_pos].adc.accel;
				log.u.sensor.pres = ao_data_ring[ao_log_data_pos].adc.pres;
				ao_log_write(&log);
				if (ao_log_state <= ao_flight_coast)
					next_sensor = log.tick + AO_SENSOR_INTERVAL_ASCENT;
				else
					next_sensor = log.tick + AO_SENSOR_INTERVAL_DESCENT;
			}
			if ((int16_t) (log.tick - next_other) >= 0) {
				log.type = AO_LOG_TEMP_VOLT;
				log.u.temp_volt.temp = ao_data_ring[ao_log_data_pos].adc.temp;
				log.u.temp_volt.v_batt = ao_data_ring[ao_log_data_pos].adc.v_batt;
				ao_log_write(&log);
				log.type = AO_LOG_DEPLOY;
				log.u.deploy.drogue = ao_data_ring[ao_log_data_pos].adc.sense_d;
				log.u.deploy.main = ao_data_ring[ao_log_data_pos].adc.sense_m;
				ao_log_write(&log);
				next_other = log.tick + AO_OTHER_INTERVAL;
			}
			ao_log_data_pos = ao_data_ring_next(ao_log_data_pos);
		}
		/* Write state change to EEPROM */
		if (ao_flight_state != ao_log_state) {
			ao_log_state = ao_flight_state;
			log.type = AO_LOG_STATE;
			log.tick = ao_sample_tick;
			log.u.state.state = ao_log_state;
			log.u.state.reason = 0;
			ao_log_write(&log);

			if (ao_log_state == ao_flight_landed)
				ao_log_stop();
		}

		/* Wait for a while */
		ao_delay(AO_MS_TO_TICKS(100));

		/* Stop logging when told to */
		while (!ao_log_running)
			ao_sleep(&ao_log_running);
	}
}
