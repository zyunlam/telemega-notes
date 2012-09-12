/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.AltosLib;

public class AltosDistance extends AltosUnits {

	public double value(double v) {
		if (AltosConvert.imperial_units)
			return AltosConvert.meters_to_miles(v);
		return v;
	}

	public String show_units() {
		if (AltosConvert.imperial_units)
			return "miles";
		return "m";
	}

	public String say_units() {
		if (AltosConvert.imperial_units)
			return "miles";
		return "meters";
	}

	int show_fraction(int width) {
		if (AltosConvert.imperial_units)
			return width / 3;
		return width / 9;
	}

	int say_fraction() {
		if (AltosConvert.imperial_units)
			return 1;
		return 0;
	}
}