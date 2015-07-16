/*
 * Copyright © 2015 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_8;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.concurrent.*;

public class AltosMapPathPoint {
	public AltosLatLon	lat_lon;
	public int		state;

	public int hashCode() {
		return lat_lon.hashCode() ^ state;
	}

	public boolean equals(Object o) {
		if (o == null)
			return false;

		if (!(o instanceof AltosMapPathPoint))
			return false;

		AltosMapPathPoint other = (AltosMapPathPoint) o;

		return lat_lon.equals(other.lat_lon) && state == other.state;
	}

	public AltosMapPathPoint(AltosLatLon lat_lon, int state) {
		this.lat_lon = lat_lon;
		this.state = state;
	}
}

