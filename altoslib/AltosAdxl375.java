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

package org.altusmetrum.altoslib_13;

import java.util.concurrent.*;

public class AltosAdxl375 implements Cloneable {

	private int	accel;
	private int	axis;

	public static final int X_AXIS = 0;
	public static final int Y_AXIS = 1;
	public static final int Z_AXIS = 2;

	public boolean parse_line(String line) throws NumberFormatException {
		if (line.startsWith("ADXL375 value")) {
			String[] items = line.split("\\s+");
			if (axis == AltosLib.MISSING)
				throw new NumberFormatException("No ADXL375 axis specified");
			if (items.length >= 3) {
				accel = Integer.parseInt(items[2 + axis]);
				return true;
			}
		}
		return false;
	}

	public AltosAdxl375 clone() {
		AltosAdxl375	n = new AltosAdxl375(axis);

		n.accel = accel;
		return n;
	}

	static public void provide_data(AltosDataListener listener, AltosLink link) throws InterruptedException, AltosUnknownProduct {
		try {
			AltosCalData	cal_data = listener.cal_data();
			AltosAdxl375	adxl375 = new AltosAdxl375(link, cal_data.adxl375_axis);

			if (adxl375 != null) {
				int accel = adxl375.accel;
				if (cal_data.adxl375_inverted)
					accel = -accel;
				if (cal_data.pad_orientation == 1)
					accel = -accel;
				listener.set_acceleration(cal_data.acceleration(accel));
			}
		} catch (TimeoutException te) {
		} catch (NumberFormatException ne) {
		}
	}

	public AltosAdxl375() {
		accel = AltosLib.MISSING;
		axis = AltosLib.MISSING;
	}

	public AltosAdxl375(int axis) {
		this();
		this.axis = axis;
	}

	public AltosAdxl375(AltosLink link, int axis) throws InterruptedException, TimeoutException, NumberFormatException {
		this(axis);
		link.printf("A\n");
		for (;;) {
			String line = link.get_reply_no_dialog(5000);
			if (line == null)
				throw new TimeoutException();
			if (parse_line(line))
				break;
		}
	}
}
