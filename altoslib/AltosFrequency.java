/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_11;

import java.io.*;
import java.util.*;
import java.text.*;

public class AltosFrequency implements AltosJsonable {
	public double	frequency;
	public String	description;

	public int hashCode() {
		return new Double(frequency).hashCode();
	}

	public boolean equals(Object o) {
		if (o == null)
			return false;
		if (!(o instanceof AltosFrequency))
			return false;
		AltosFrequency other = (AltosFrequency) o;
		return other.frequency == frequency;
	}

	public String toString() {
		return String.format("%7.3f MHz %-20s",
				     frequency, description);
	}

	public String toShortString() {
		return String.format("%7.3f MHz %s",
				     frequency, description);
	}

	public String frequency_string() {
		return String.format("%7.3f", frequency);
	}

	public boolean close(double f) {
		double	diff = Math.abs(frequency - f);

		return diff < 0.010;
	}

	public AltosJson json() {
		AltosJson	j = new AltosJson();

		j.put("frequency", frequency);
		j.put("description", description);
		return j;
	}

	public AltosFrequency(double f, String d) {
		frequency = f;
		description = d;
	}

	private AltosFrequency(AltosJson j) {
		frequency = j.get_double("frequency", 0.0);
		description = j.get_string("description", "");
	}

	public static AltosFrequency fromJson(AltosJson j, AltosFrequency def) {
		if (j == null)
			return def;
		return new AltosFrequency(j);
	}
}
