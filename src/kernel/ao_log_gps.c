/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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
#include <ao_log_gps.h>
#include <ao_data.h>
#include <ao_flight.h>
#include <ao_distance.h>
#include <ao_tracker.h>

void
ao_log_gps_flight(void)
{
	log.type = AO_LOG_FLIGHT;
	log.tick = ao_time();
	log.u.flight.flight = ao_flight_number;
	ao_log_write(&log);
}

void
ao_log_gps_data(uint16_t tick, struct ao_telemetry_location *gps_data)
{
	log.tick = tick;
	log.type = AO_LOG_GPS_TIME;
	log.u.gps.latitude = gps_data->latitude;
	log.u.gps.longitude = gps_data->longitude;
	log.u.gps.altitude_low = gps_data->altitude_low;
	log.u.gps.altitude_high = gps_data->altitude_high;

	log.u.gps.hour = gps_data->hour;
	log.u.gps.minute = gps_data->minute;
	log.u.gps.second = gps_data->second;
	log.u.gps.flags = gps_data->flags;
	log.u.gps.year = gps_data->year;
	log.u.gps.month = gps_data->month;
	log.u.gps.day = gps_data->day;
	log.u.gps.course = gps_data->course;
	log.u.gps.ground_speed = gps_data->ground_speed;
	log.u.gps.climb_rate = gps_data->climb_rate;
	log.u.gps.pdop = gps_data->pdop;
	log.u.gps.hdop = gps_data->hdop;
	log.u.gps.vdop = gps_data->vdop;
	log.u.gps.mode = gps_data->mode;
	ao_log_write(&log);
}

void
ao_log_gps_tracking(uint16_t tick, struct ao_telemetry_satellite *gps_tracking_data)
{
	uint8_t c, n, i;

	log.tick = tick;
	log.type = AO_LOG_GPS_SAT;
	i = 0;
	n = gps_tracking_data->channels;
	for (c = 0; c < n; c++)
		if ((log.u.gps_sat.sats[i].svid = gps_tracking_data->sats[c].svid))
		{
			log.u.gps_sat.sats[i].c_n = gps_tracking_data->sats[c].c_n_1;
			i++;
			if (i >= 12)
				break;
		}
	log.u.gps_sat.channels = i;
	ao_log_write(&log);
}

int8_t
ao_log_check(uint32_t pos)
{
	if (!ao_storage_read(pos,
			     &log,
			     sizeof (struct ao_log_gps)))
		return AO_LOG_INVALID;

	if (ao_log_check_clear())
		return AO_LOG_EMPTY;

	if (!ao_log_check_data())
		return AO_LOG_INVALID;
	return AO_LOG_VALID;
}
