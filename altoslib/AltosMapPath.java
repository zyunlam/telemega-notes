/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

package org.altusmetrum.altoslib_13;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.concurrent.*;

public abstract class AltosMapPath {

	public LinkedList<AltosMapPathPoint>	points = new LinkedList<AltosMapPathPoint>();
	public AltosMapPathPoint		last_point = null;

	static public int stroke_width = 6;

	public abstract void paint(AltosMapTransform t);

	public AltosMapRectangle add(double lat, double lon, double time, int state) {
		AltosMapPathPoint		point = new AltosMapPathPoint(new AltosLatLon (lat, lon), time, state);
		AltosMapRectangle	rect = null;

		if (!point.equals(last_point)) {
			if (last_point != null)
				rect = new AltosMapRectangle(last_point.lat_lon, point.lat_lon);
			points.add (point);
			last_point = point;
		}
		return rect;
	}

	private double dist(AltosLatLon lat_lon, AltosMapPathPoint point) {
		return (new AltosGreatCircle(lat_lon.lat,
					     lat_lon.lon,
					     point.lat_lon.lat,
					     point.lat_lon.lon)).distance;
	}

	public AltosMapPathPoint nearest(AltosLatLon lat_lon) {
		AltosMapPathPoint nearest = null;
		double nearest_dist = 0;
		for (AltosMapPathPoint point : points) {
			if (nearest == null) {
				nearest = point;
				nearest_dist = dist(lat_lon, point);
			} else {
				double d = dist(lat_lon, point);
				if (d < nearest_dist) {
					nearest = point;
					nearest_dist = d;
				}
			}
		}
		return nearest;
	}

	public void clear () {
		points = new LinkedList<AltosMapPathPoint>();
	}
}
