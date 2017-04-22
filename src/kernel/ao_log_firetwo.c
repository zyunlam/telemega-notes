/*
 * Copyright © 2017 Bdale Garbee <bdale@gag.com>
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

static __xdata struct ao_log_firetwo log;

__code uint8_t ao_log_format = AO_LOG_FORMAT_TELEFIRETWO;

static uint8_t
ao_log_csum(__xdata uint8_t *b) __reentrant
{
	uint8_t	sum = 0x5a;
	uint8_t	i;

	for (i = 0; i < sizeof (struct ao_log_firetwo); i++)
		sum += *b++;
	return -sum;
}

uint8_t
ao_log_firetwo(__xdata struct ao_log_firetwo *log) __reentrant
{
	uint8_t wrote = 0;
	/* set checksum */
	log->csum = 0;
	log->csum = ao_log_csum((__xdata uint8_t *) log);
	ao_mutex_get(&ao_log_mutex); {
		if (ao_log_current_pos >= ao_log_end_pos && ao_log_running)
			ao_log_stop();
		if (ao_log_running) {
			wrote = 1;
			ao_storage_write(ao_log_current_pos,
					 log,
					 sizeof (struct ao_log_firetwo));
			ao_log_current_pos += sizeof (struct ao_log_firetwo);
		}
	} ao_mutex_put(&ao_log_mutex);
	return wrote;
}

static uint8_t
ao_log_dump_check_data(void)
{
	if (ao_log_csum((uint8_t *) &log) != 0)
		return 0;
	return 1;
}

#if HAS_ADC
static __data uint8_t	ao_log_data_pos;

/* a hack to make sure that ao_log_metrums fill the eeprom block in even units */
typedef uint8_t check_log_size[1-(256 % sizeof(struct ao_log_firetwo))] ;
#endif

void
ao_log(void)
{
	__pdata uint16_t	next_sensor, next_other;

	ao_storage_setup();

	ao_log_scan();

	while (!ao_log_running)
		ao_sleep(&ao_log_running);

#if HAS_FLIGHT
	log.type = AO_LOG_FLIGHT;
	log.tick = ao_sample_tick;
	log.u.flight.idle_pressure = ao_idle_pressure;
	log.u.flight.idle_thrust = ao_idle_thrust;
	log.u.flight.flight = ao_flight_number;
	ao_log_firetwo(&log);
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
			log.tick = ao_data_ring[ao_log_data_pos].tick;
			if ((int16_t) (log.tick - next_sensor) >= 0) {
				log.type = AO_LOG_SENSOR;
				log.u.sensor.pressure = ao_data_ring[ao_log_data_pos].sensor.pressure;
				log.u.sensor.thrust = ao_data_ring[ao_log_data_pos].sensor.thrust;
				for (i = 0; i < 4; i++) {
					log.u.sensor.thermistor[i] = ao_data_ring[ao_log_data_pos].sensor.thermistor[i];
				}
				ao_log_firetwo(&log);
			}
			ao_log_data_pos = ao_data_ring_next(ao_log_data_pos);
		}
#if HAS_FLIGHT
		/* Write state change to EEPROM */
		if (ao_flight_state != ao_log_state) {
			ao_log_state = ao_flight_state;
			log.type = AO_LOG_STATE;
			log.tick = ao_time();
			log.u.state.state = ao_log_state;
			log.u.state.reason = 0;
			ao_log_firetwo(&log);

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

uint16_t
ao_log_flight(uint8_t slot)
{
	if (!ao_storage_read(ao_log_pos(slot),
			     &log,
			     sizeof (struct ao_log_firetwo)))
		return 0;

	if (ao_log_dump_check_data() && log.type == AO_LOG_FLIGHT)
		return log.u.flight.flight;
	return 0;
}
