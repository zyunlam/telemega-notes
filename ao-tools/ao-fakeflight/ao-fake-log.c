/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

#include "ao-fakeflight.h"

static uint8_t
ao_log_csum(uint8_t *b)
{
	uint8_t	sum = 0x5a;
	uint8_t	i;

	for (i = 0; i < sizeof (struct ao_log_metrum); i++)
		sum += *b++;
	return -sum;
}

void
ao_log_metrum(FILE *file, struct ao_log_metrum *log)
{
	uint8_t	*d;

	/* set checksum */
	log->csum = 0;
	log->csum = ao_log_csum((uint8_t *) log);
	int i;

	fprintf (file, "%c %04x", log->type, log->tick);
	d = (void *) &log->u;
	for (i = 0; i < sizeof (log->u); i++)
		fprintf(file, " %02x", d[i]);
	fprintf (file, "\n");
}

void
ao_log_flight(FILE *file, struct flight *f, struct rocket *r)
{
	static struct ao_log_metrum	log;

	log.type = AO_LOG_FLIGHT;
	log.tick = (f->time * 100);
	log.u.flight.ground_accel = ao_accel_unconvert(GRAVITY);
	log.u.flight.ground_pres = cc_altitude_to_pressure(f->altitude);
	log.u.flight.flight = 1;
	ao_log_metrum(file, &log);
}

void
ao_log_sensor(FILE *file, struct flight *f, struct rocket *r)
{
	static struct ao_log_metrum	log;
	struct ao_ms5607_sample		sample_baro;
	int16_t				sample_accel;

	ao_ms5607_unconvert(f->altitude, &sample_baro);
	sample_accel = ao_accel_unconvert(f->accel);

	log.type = AO_LOG_SENSOR;
	log.tick = (f->time * 100);
	log.u.sensor.pres = sample_baro.pres;
	log.u.sensor.temp = sample_baro.temp;
	log.u.sensor.accel = sample_accel;
	ao_log_metrum(file, &log);
}

void
ao_log_gps(FILE *file, struct flight *f, struct rocket *r)
{
	static struct ao_log_metrum	log;
	static double			gps_time;
	static int			been_here;
	int32_t				altitude;
	int32_t				seconds;

	if (been_here && (f->time - gps_time) < 1)
		return;
	been_here = 1;
	gps_time = f->time;

	altitude = f->altitude;
	seconds = f->time;

	log.type = AO_LOG_GPS_POS;
	log.tick = (f->time * 100);
	log.u.gps.latitude = 45 * 1e7;
	log.u.gps.longitude = -121 * 1e7;
	log.u.gps.altitude_low = altitude;
	log.u.gps.altitude_high = altitude >> 16;
	ao_log_metrum(file, &log);

	log.type = AO_LOG_GPS_TIME;
	log.tick = (f->time * 100);
	log.u.gps_time.hour = seconds / 3600;
	log.u.gps_time.minute = (seconds / 60) % 60;
	log.u.gps_time.second = seconds % 60;
	log.u.gps_time.flags = AO_GPS_VALID | AO_GPS_RUNNING | AO_GPS_DATE_VALID | AO_GPS_COURSE_VALID | (12 << AO_GPS_NUM_SAT_SHIFT);
	log.u.gps_time.year = 114;
	log.u.gps_time.month = 7;
	log.u.gps_time.day = 1;
	log.u.gps_time.pdop = 1;

	ao_log_metrum(file, &log);
}

void
ao_log_state(FILE *file, struct flight *f, struct rocket *r)
{
	static struct ao_log_metrum	log;
	static int			log_state = ao_flight_startup;
	int				flight_state;

	switch (f->state) {
	case state_ascent:
		if (f->speed == 0 && f->altitude == 0)
			flight_state = ao_flight_pad;
		else if (f->accel > 0)
			flight_state = ao_flight_boost;
		else if (f->speed > 300)
			flight_state = ao_flight_fast;
		else
			flight_state = ao_flight_coast;
		break;
	case state_drogue:
		flight_state = ao_flight_drogue;
		break;
	case state_main:
		flight_state = ao_flight_main;
		break;
	case state_landed:
		flight_state = ao_flight_landed;
		break;
	}
	if (flight_state != log_state) {
		log.type = AO_LOG_STATE;
		log.tick = (f->time * 100);
		log.u.state.state = flight_state;
		log.u.state.reason = 0;
		ao_log_metrum(file, &log);
		log_state = flight_state;
	}
}

void
ao_log(FILE *file, struct flight *f, struct rocket *r)
{
	static int	been_here = 0;

	if (!been_here) {
		been_here = 1;
		ao_log_flight(file, f, r);
	}
	ao_log_sensor(file, f, r);
	ao_log_state(file, f, r);
	ao_log_gps(file, f, r);
}
