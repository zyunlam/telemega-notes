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

/* Density of dry air in kg/m³ for a given pressure and temperature. */
double
density_air(double pressure,		/* Pa */
	    double temperature)		/* °C */
{
	return pressure / (GAS_CONSTANT * (temperature + 273.15));
}

/* Area of a circle */
double
area_circle(double diameter)
{
	double radius = diameter / 2;
	return M_PI * radius * radius;
}

/* Force due to drag (N) */
double
force_drag(double	speed,		/* m/s */
	   double	rho,		/* kg/m³ */
	   double	cd,		/* unitless */
	   double	area)		/* m² */
{
	return 0.5 * rho * (speed * speed) * cd * area;
}

/* Force due to gravity (N) */
double
force_gravity(double	mass,		/* kg */
	      double   	altitude)	/* m */
{
	double	distance = altitude + EARTH_RADIUS;
	return GRAVITATIONAL_CONSTANT * EARTH_MASS * mass / (distance * distance);
}
