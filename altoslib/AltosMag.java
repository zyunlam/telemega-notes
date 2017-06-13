/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_12;

import java.util.concurrent.*;
import java.io.*;

public class AltosMag implements Cloneable {
	public int		x;
	public int		z;
	public int		y;

	public static final double counts_per_gauss = 1090;

	public static double convert_gauss(double counts) {
		return counts / counts_per_gauss;
	}

	public boolean parse_string(String line) {

		if (!line.startsWith("X:"))
			return false;

		String[] items = line.split("\\s+");

		if (items.length >= 6) {
			x = Integer.parseInt(items[1]);
			z = Integer.parseInt(items[3]);
			y = Integer.parseInt(items[5]);
		}
		return true;
	}

	public AltosMag clone() {
		AltosMag n = new AltosMag();

		n.x = x;
		n.z = z;
		n.y = y;
		return n;
	}

	public AltosMag() {
		x = AltosLib.MISSING;
		z = AltosLib.MISSING;
		y = AltosLib.MISSING;
	}

	static public void provide_data(AltosDataListener listener, AltosLink link, AltosCalData cal_data) throws InterruptedException {
		try {
			AltosMag	mag = new AltosMag(link);

			if (mag != null)
				listener.set_mag(cal_data.mag_along(mag.y),
						 cal_data.mag_across(mag.x),
						 cal_data.mag_through(mag.z));
		} catch (TimeoutException te) {
		}
	}

	public AltosMag(AltosLink link) throws InterruptedException, TimeoutException {
		this();
		link.printf("M\n");
		for (;;) {
			String line = link.get_reply_no_dialog(5000);
			if (line == null) {
				throw new TimeoutException();
			}
			if (parse_string(line))
				break;
		}
	}
}
