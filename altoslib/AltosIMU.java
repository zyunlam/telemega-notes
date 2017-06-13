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

public class AltosIMU implements Cloneable {
	public int		accel_x;
	public int		accel_y;
	public int		accel_z;

	public int		gyro_x;
	public int		gyro_y;
	public int		gyro_z;

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
		return n;
	}

	static public void provide_data(AltosDataListener listener, AltosLink link, AltosCalData cal_data) throws InterruptedException {
		try {
			AltosIMU	imu = new AltosIMU(link);

			if (imu != null) {
				listener.set_gyro(cal_data.gyro_roll(imu.gyro_y),
						  cal_data.gyro_pitch(imu.gyro_x),
						  cal_data.gyro_yaw(imu.gyro_z));
				listener.set_accel_ground(cal_data.accel_along(imu.accel_y),
							  cal_data.accel_across(imu.accel_x),
							  cal_data.accel_through(imu.accel_z));
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
