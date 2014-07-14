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

#ifndef _AO_FAKEFLIGHT_H_
#define _AO_FAKEFLIGHT_H_

#define __xdata
#include <stdint.h>
#include <ao_ms5607.h>
#include <math.h>
#include <stdio.h>
#include "cc.h"

#define GAS_CONSTANT		287.05
#define STEP			0.01
#define GRAVITATIONAL_CONSTANT	6.67384e-11
#define GRAVITY			9.807
#define EARTH_MASS		5.9726e24
#define EARTH_RADIUS		6375313

enum flight_state {
	state_ascent,
	state_drogue,
	state_main,
	state_landed,
	state_num
};

extern char *state_name[];

struct model {
	double	cd;
	double	diameter;
};

struct motor {
	double	fuel_mass;
	double	average_thrust;
	double	burn_time;
	double	delay;
};

#define MAX_MOTORS	12

struct rocket {
	struct model	models[state_num];
	double		empty_mass;
	double		main_deploy;
	struct motor	motors[MAX_MOTORS];
};

/* Flight state */

struct flight {
	double	time;
	double	altitude;
	double	speed;
	double	accel;
	enum flight_state	state;
};

/* ao-physics.c */

/* Density of dry air in kg/m³ for a given pressure and temperature. */
/* Density of dry air in kg/m³ for a given pressure and temperature. */
double
density_air(double pressure,		/* Pa */
	    double temperature);	/* °C */

/* Area of a circle */
double
area_circle(double diameter);

/* Force due to drag (N) */
double
force_drag(double	speed,		/* m/s */
	   double	rho,		/* kg/m³ */
	   double	cd,		/* unitless */
	   double	area);		/* m² */

/* Force due to gravity (N) */
double
force_gravity(double	mass,		/* kg */
	      double   	altitude);	/* m */

/* ao-rocket.c */

/* Mass (kg) of the airframe */
double
rocket_mass(struct flight *f, struct rocket *r);

/* Force (N) due to thrust */
double
rocket_thrust(struct flight *f, struct rocket *r);

/* Drag (N) due to air resistance */
double
rocket_drag(struct flight *f, struct rocket *r);

/* Force (N) due to gravity */
double
rocket_gravity(struct flight *f, struct rocket *r);

/* ao-fake-convert.c */
void
ao_ms5607_unconvert(double altitude,
		    struct ao_ms5607_sample *ret);

int16_t
ao_accel_unconvert(double accel);

void
ao_show_header(FILE *f, struct rocket *r);

/* ao-fake-log.c */

void
ao_log_flight(FILE *file, struct flight *f, struct rocket *r);

void
ao_log_sensor(FILE *file, struct flight *f, struct rocket *r);

void
ao_log_state(FILE *file, struct flight *f, struct rocket *r);

#endif
