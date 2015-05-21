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

package org.altusmetrum.altoslib_6;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.concurrent.*;

class PathPoint {
	AltosLatLon	lat_lon;
	int		state;

	public PathPoint(AltosLatLon lat_lon, int state) {
		this.lat_lon = lat_lon;
		this.state = state;
	}

	public boolean equals(PathPoint other) {
		if (other == null)
			return false;

		return lat_lon.equals(other.lat_lon) && state == other.state;
	}
}

public abstract class AltosMapPath {

	LinkedList<PathPoint>	points = new LinkedList<PathPoint>();
	PathPoint		last_point = null;

	static public int stroke_width = 6;

	public abstract void paint(AltosMapTransform t);

	public AltosMapRectangle add(double lat, double lon, int state) {
		PathPoint		point = new PathPoint(new AltosLatLon (lat, lon), state);
		AltosMapRectangle	rect = null;

		if (!point.equals(last_point)) {
			if (last_point != null)
				rect = new AltosMapRectangle(last_point.lat_lon, point.lat_lon);
			points.add (point);
			last_point = point;
		}
		return rect;
	}

	public void clear () {
		points = new LinkedList<PathPoint>();
	}
}
