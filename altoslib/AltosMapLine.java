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

package org.altusmetrum.altoslib_10;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.concurrent.*;

public abstract class AltosMapLine {
	public AltosLatLon	start, end;

	static public int stroke_width = 6;

	public abstract void paint(AltosMapTransform t);

	private AltosLatLon lat_lon(AltosPointInt pt, AltosMapTransform t) {
		return t.screen_lat_lon(pt);
	}

	public void dragged(AltosPointInt pt, AltosMapTransform t) {
		end = lat_lon(pt, t);
	}

	public void pressed(AltosPointInt pt, AltosMapTransform t) {
		start = lat_lon(pt, t);
		end = null;
	}

	public String line_dist() {
		String	format;
		AltosGreatCircle	g = new AltosGreatCircle(start.lat, start.lon,
								 end.lat, end.lon);
		double	distance = g.distance;

		if (AltosConvert.imperial_units) {
			distance = AltosConvert.meters_to_feet(distance);
			if (distance < 10000) {
				format = "%4.0fft";
			} else {
				distance /= 5280;
				if (distance < 10)
					format = "%5.3fmi";
				else if (distance < 100)
					format = "%5.2fmi";
				else if (distance < 1000)
					format = "%5.1fmi";
				else
					format = "%5.0fmi";
			}
		} else {
			if (distance < 10000) {
				format = "%4.0fm";
			} else {
				distance /= 1000;
				if (distance < 100)
					format = "%5.2fkm";
				else if (distance < 1000)
					format = "%5.1fkm";
				else
					format = "%5.0fkm";
			}
		}
		return String.format(format, distance);
	}
}
