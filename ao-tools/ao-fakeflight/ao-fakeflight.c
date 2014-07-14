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

char *state_name[] = {
	[state_ascent] = "ascent",
	[state_drogue] = "drogue",
	[state_main] = "main",
	[state_landed] = "landed",
};

struct rocket o5778 = {
	.models = {
		[state_ascent] = {
			.cd = 0.5,
			.diameter = .100
		},
		[state_drogue] = {
			.cd = 1.3,
			.diameter = .300
		},
		[state_main] = {
			.cd = 1.3,
			.diameter = 1.0
		},
	},
	.empty_mass = 15.0,
	.main_deploy = 250,
	.motors = {
		[0] = {
			.fuel_mass = 18.021,
			.average_thrust = 5778.0,
			.burn_time = 8.52,
			.delay = 0.0,
		},
		[1] = {
			.fuel_mass = 18.021,
			.average_thrust = 5778.0,
			.burn_time = 0, /* 17.04 */
			.delay = 18.0,
		},
	},
};

struct rocket n5778 = {
	.models = {
		[state_ascent] = {
			.cd = 0.5,
			.diameter = .100
		},
		[state_drogue] = {
			.cd = 1.3,
			.diameter = .300
		},
		[state_main] = {
			.cd = 1.3,
			.diameter = 1.0
		},
	},
	.empty_mass = 15.0,
	.main_deploy = 250,
	.motors = {
		[0] = {
			.fuel_mass = 9.0105,
			.average_thrust = 5778.0,
			.burn_time = 4.26,
			.delay = 0.0,
		},
	},
};

/*
 * Move the flight one tick forward
 */
void
step(struct flight *f, struct rocket *r)
{
	double	drag = rocket_drag(f, r);
	double	thrust = rocket_thrust(f, r);
	double	gravity = rocket_gravity(f, r);
	double	mass = rocket_mass(f, r);
	double	accel;

	/* This is our acceleration relative to the earth */
	accel = (drag + thrust - gravity) / mass;

	/* Because the accelerometer is also affected by gravity, it
	 * only measures *other* forces applied to the rocket
	 */
	f->accel = (drag + thrust) / mass;

	f->altitude += f->speed * STEP + 0.5 * (accel * STEP) * (accel * STEP);
	f->speed += accel * STEP;
	f->time += STEP;
}

void
init(struct flight *f, struct rocket *r)
{
	double gravity = rocket_gravity(f, r);

	/* The pad is pushing up on the rocket to counter
	 * gravity, so the accelerometer, which only measures
	 * forces other than gravity, sees only this
	 */
	f->accel = gravity / rocket_mass(f, r);
	f->speed = 0;
	f->altitude = 0;
	f->time += STEP;
}

void
fly(FILE *file, struct rocket *r)
{
	struct flight f = {
		.time = -1,
		.altitude = 0,
		.speed = 0,
		.accel = 0,
		.state = state_ascent,
	};

	double	max_speed = 0;
	double	max_alt = 0;
	double	max_accel = 0;


	ao_show_header(file, r);
	while (f.time < 0) {
		init(&f, r);
		ao_log(file, &f, r);
	}
	while (f.altitude >= 0) {
		step(&f, r);
		switch (f.state) {
		case state_ascent:
			if (f.speed >= 0)
				break;
			f.state = state_drogue;
			/* fall through */
		case state_drogue:
			if (f.altitude > o5778.main_deploy)
				break;
			f.state = state_main;
			/* fall through */
		case state_main:
			break;
		}
		ao_log(file, &f, r);
		max_speed = fmax(max_speed, f.speed);
		max_accel = fmax(max_accel, f.accel);
		max_alt = fmax(max_alt, f.altitude);
	}
	fprintf(stderr, "max alt %9.1f speed %9.1f accel %9.1f\n",
		max_alt, max_speed, max_accel);
}

int
main(int argc, char **argv)
{
	fly(stdout, &o5778);
//	fly(stdout, &n5778);
	return 0;
}
