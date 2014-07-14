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

#include "cc.h"
#include <ao-fakeflight.h>
#include <stdio.h>
#include <math.h>

/* From mega serial 1246 */

struct ao_ms5607_prom	ao_ms5607_prom = {
	.reserved =  156,
	.sens =  44229,
	.off =  38634,
	.tcs =  27789,
	.tco =  25015,
	.tref =  32003,
	.tempsens =  27647,
	.crc =  50256,
};

struct ao_config {
	int16_t	accel_plus_g;
	int16_t	accel_minus_g;
};

struct ao_config ao_config = {
	.accel_plus_g = 2028,
	.accel_minus_g = 2065
};

#define MIN_VALUE	0
#define MAX_VALUE	16777216
#define MID_VALUE	((MAX_VALUE - MIN_VALUE) / 2)

static void
ao_ms5607_search (struct ao_ms5607_value	*goal_value,
		  struct ao_ms5607_sample 	*goal_sample,
		  int	search_temp)
{
	struct ao_ms5607_sample	sample;
	struct ao_ms5607_value	value;
	uint32_t		hi, lo, mid;
	int32_t			goal;
	int32_t			result;

	if (search_temp) {
		sample.pres = MID_VALUE;
		goal = goal_value->temp;
	} else {
		sample.temp = goal_sample->temp;
		goal = goal_value->pres;
	}

	lo = MIN_VALUE;
	hi = MAX_VALUE;
	for (;;) {
		mid = (hi + lo) >> 1;

		if (mid == lo)
			break;

		if (search_temp)
			sample.temp = mid;
		else
			sample.pres = mid;

		ao_ms5607_convert(&sample, &value);

		if (search_temp)
			result = value.temp;
		else
			result = value.pres;

		if (result == goal)
			break;

		if (result < goal)
			lo = mid;
		else
			hi = mid;
	}
	*goal_sample = sample;
}

void
ao_ms5607_unconvert(double altitude,
		    struct ao_ms5607_sample *ret)
{
	double pressure = cc_altitude_to_pressure(altitude);
	double temperature = cc_altitude_to_temperature(altitude);

	struct ao_ms5607_value	value;

	value.pres = floor (pressure + 0.5);
	value.temp = floor (temperature * 100 + 0.5);

	/* First back-convert temperature; that's independent of pressure */

	ao_ms5607_search(&value, ret, 1);

	/* Now search for pressure */

	ao_ms5607_search(&value, ret, 0);
}

int16_t
ao_accel_unconvert(double accel)
{
	int16_t	ao_accel_2g = ao_config.accel_minus_g - ao_config.accel_plus_g;
	double	ao_accel_0g = (ao_config.accel_minus_g + ao_config.accel_plus_g) / 2.0;
	double	ao_accel_scale = GRAVITY * 2.0 / ao_accel_2g;

	int16_t	ao_sample_accel = ao_accel_0g - accel/ao_accel_scale;

	return ao_sample_accel;
}

static void
ao_ms5607_show(FILE *f, struct rocket *r)
{
	fprintf(f, "ms5607 reserved: %u\n", ao_ms5607_prom.reserved);
	fprintf(f, "ms5607 sens: %u\n", ao_ms5607_prom.sens);
	fprintf(f, "ms5607 off: %u\n", ao_ms5607_prom.off);
	fprintf(f, "ms5607 tcs: %u\n", ao_ms5607_prom.tcs);
	fprintf(f, "ms5607 tco: %u\n", ao_ms5607_prom.tco);
	fprintf(f, "ms5607 tref: %u\n", ao_ms5607_prom.tref);
	fprintf(f, "ms5607 tempsens: %u\n", ao_ms5607_prom.tempsens);
	fprintf(f, "ms5607 crc: %u\n", ao_ms5607_prom.crc);
}

static void
ao_config_show(FILE *f, struct rocket *r)
{
	fprintf(f, "Config version: 1.18\n");
	fprintf(f, "Accel cal +1g: %d -1g: %d\n",
		ao_config.accel_plus_g, ao_config.accel_minus_g);
	fprintf(f, "Main deploy: %d meters\n", (int) r->main_deploy);
}

#include "config.h"

void
ao_show_header(FILE *f, struct rocket *r)
{
	ao_config_show(f, r);
	ao_ms5607_show(f, r);
	fprintf(f, "serial-number 1\n");
	fprintf(f, "current-flight 1\n");
	fprintf(f, "log-format       %u\n", AO_LOG_FORMAT_TELEMETRUM);
	fprintf(f, "altitude-32      1\n");
	fprintf(f, "software-version %s\n", PACKAGE_VERSION);
}

#include "ao_ms5607_convert.c"
