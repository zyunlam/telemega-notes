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

/* Mass (kg) of the airframe */
double
rocket_mass(struct flight *f, struct rocket *r)
{
	double	time = f->time;
	double	m = r->empty_mass;
	int	n;

	for (n = 0; n < MAX_MOTORS; n++) {
		struct motor *motor = &r->motors[n];
		if (motor->burn_time) {
			double fuel_time_remain = fmin (motor->burn_time, fmax(motor->burn_time - (time - motor->delay), 0.0));
			double fuel_fraction = fuel_time_remain / motor->burn_time;
			m += fuel_fraction * motor->fuel_mass;
		}
	}
	return m;
}

/* Force (N) due to thrust */
double
rocket_thrust(struct flight *f, struct rocket *r)
{
	double	time = f->time;
	double	thrust = 0.0;
	int	n;

	for (n = 0; n < MAX_MOTORS; n++) {
		struct motor *motor = &r->motors[n];
		if (motor->burn_time) {
			if (motor->delay <= time && time < motor->delay + motor->burn_time)
				thrust += motor->average_thrust;
		}
	}
	return thrust;
}

/* Drag (N) due to air resistance */
double
rocket_drag(struct flight *f, struct rocket *r)
{
	double	pressure = cc_altitude_to_pressure(f->altitude);
	double	temperature = cc_altitude_to_temperature(f->altitude);
	double	drag = force_drag(f->speed, density_air(pressure, temperature),
			  r->models[f->state].cd,
			  area_circle(r->models[f->state].diameter));

	/* drag operates in opposition to motion through air */
	if (f->speed >= 0)
		drag = -drag;

	return drag;
}

/* Force (N) due to gravity */
double
rocket_gravity(struct flight *f, struct rocket *r)
{
	double	gravity = force_gravity(rocket_mass (f, r), f->altitude);
}
