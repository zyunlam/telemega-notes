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
import java.io.*;

public class AltosIMU implements Cloneable {
	public int		accel_x = AltosLib.MISSING;
	public int		accel_y = AltosLib.MISSING;
	public int		accel_z = AltosLib.MISSING;

	public int		gyro_x = AltosLib.MISSING;
	public int		gyro_y = AltosLib.MISSING;
	public int		gyro_z = AltosLib.MISSING;

	public int		mag_x = AltosLib.MISSING;
	public int		mag_y = AltosLib.MISSING;
	public int		mag_z = AltosLib.MISSING;

	public static final double	counts_per_g = 2048.0;

	public static double convert_accel(double counts) {
		return counts / counts_per_g * AltosConvert.gravity;
	}

	/* In radians */
	public static final double 	GYRO_FULLSCALE_DEGREES = 2000.0;
	public static final double	GYRO_COUNTS = 32767.0;

	public static double gyro_degrees_per_second(double counts, double cal) {
		return (counts - cal) * GYRO_FULLSCALE_DEGREES / GYRO_COUNTS;
	}

	public boolean parse_string(String line) {
		if (!line.startsWith("Accel:"))
			return false;

		String[] items = line.split("\\s+");

		if (items.length >= 8) {
			accel_x = Integer.parseInt(items[1]);
			accel_y = Integer.parseInt(items[2]);
			accel_z = Integer.parseInt(items[3]);
			gyro_x = Integer.parseInt(items[5]);
			gyro_y = Integer.parseInt(items[6]);
			gyro_z = Integer.parseInt(items[7]);
		}
		if (items.length >= 12) {
			mag_x = Integer.parseInt(items[9]);
			mag_y = Integer.parseInt(items[10]);
			mag_z = Integer.parseInt(items[11]);
		}
		return true;
	}

	public AltosIMU clone() {
		AltosIMU	n = new AltosIMU();

		n.accel_x = accel_x;
		n.accel_y = accel_y;
		n.accel_z = accel_z;

		n.gyro_x = gyro_x;
		n.gyro_y = gyro_y;
		n.gyro_z = gyro_z;

		n.mag_x = mag_x;
		n.mag_y = mag_y;
		n.mag_z = mag_z;

		return n;
	}

	public static final int orient_telemega = 0;
	public static final int orient_easymega_v2 = 1;

	private int accel_across(int orient) {
		switch (orient) {
		case orient_telemega:
			return accel_x;
		case orient_easymega_v2:
			return -accel_y;
		default:
			return AltosLib.MISSING;
		}
	}

	private int accel_along(int orient) {
		switch (orient) {
		case orient_telemega:
			return accel_y;
		case orient_easymega_v2:
			return accel_x;
		default:
			return AltosLib.MISSING;
		}
	}

	private int accel_through(int orient) {
		return accel_z;
	}

	private int gyro_roll(int orient) {
		switch (orient) {
		case orient_telemega:
			return gyro_y;
		case orient_easymega_v2:
			return gyro_x;
		default:
			return AltosLib.MISSING;
		}
	}

	private int gyro_pitch(int orient) {
		switch (orient) {
		case orient_telemega:
			return gyro_x;
		case orient_easymega_v2:
			return -gyro_y;
		default:
			return AltosLib.MISSING;
		}
	}

	private int gyro_yaw(int orient) {
		return gyro_z;
	}

	private int mag_across(int orient) {
		switch (orient) {
		case orient_telemega:
			return mag_x;
		case orient_easymega_v2:
			return -mag_y;
		default:
			return AltosLib.MISSING;
		}
	}

	private int mag_along(int orient) {
		switch (orient) {
		case orient_telemega:
			return mag_y;
		case orient_easymega_v2:
			return mag_x;
		default:
			return AltosLib.MISSING;
		}
	}

	private int mag_through(int orient) {
		return mag_z;
	}

	static public void provide_data(AltosDataListener listener, AltosLink link, int orient) throws InterruptedException {
		try {
			AltosIMU	imu = new AltosIMU(link);
			AltosCalData	cal_data = listener.cal_data();

			if (imu != null) {
				listener.set_gyro(cal_data.gyro_roll(imu.gyro_roll(orient)),
						  cal_data.gyro_pitch(imu.gyro_pitch(orient)),
						  cal_data.gyro_yaw(imu.gyro_yaw(orient)));
				listener.set_accel_ground(imu.accel_along(orient),
							  imu.accel_across(orient),
							  imu.accel_through(orient));
				if (imu.mag_x != AltosLib.MISSING) {
					listener.set_mag(cal_data.mag_along(imu.mag_along(orient)),
							 cal_data.mag_across(imu.mag_across(orient)),
							 cal_data.mag_through(imu.mag_through(orient)));
				}
			}
		} catch (TimeoutException te) {
		}
	}

	public AltosIMU() {
		accel_x = AltosLib.MISSING;
		accel_y = AltosLib.MISSING;
		accel_z = AltosLib.MISSING;

		gyro_x = AltosLib.MISSING;
		gyro_y = AltosLib.MISSING;
		gyro_z = AltosLib.MISSING;

		mag_x = AltosLib.MISSING;
		mag_y = AltosLib.MISSING;
		mag_z = AltosLib.MISSING;
	}

	public AltosIMU(AltosLink link) throws InterruptedException, TimeoutException {
		this();
		link.printf("I\n");
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
