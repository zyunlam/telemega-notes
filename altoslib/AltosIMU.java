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

package org.altusmetrum.altoslib_7;

import java.util.concurrent.*;
import java.io.*;

public class AltosIMU implements Cloneable, Serializable {
	public int		accel_along;
	public int		accel_across;
	public int		accel_through;

	public int		gyro_roll;
	public int		gyro_pitch;
	public int		gyro_yaw;

	public static double	counts_per_g = 2048.0;

	public static double convert_accel(double counts) {
		return counts / counts_per_g * (-AltosConvert.GRAVITATIONAL_ACCELERATION);
	}

	public static double	counts_per_degsec = 16.4;

	public static double convert_gyro(double counts) {
		return counts / counts_per_degsec;
	}

	public boolean parse_string(String line) {
		if (!line.startsWith("Accel:"))
			return false;

		String[] items = line.split("\\s+");

		if (items.length >= 8) {
			accel_along = Integer.parseInt(items[1]);
			accel_across = Integer.parseInt(items[2]);
			accel_through = Integer.parseInt(items[3]);
			gyro_roll = Integer.parseInt(items[5]);
			gyro_pitch = Integer.parseInt(items[6]);
			gyro_yaw = Integer.parseInt(items[7]);
		}
		return true;
	}

	public AltosIMU clone() {
		AltosIMU	n = new AltosIMU();

		n.accel_along = accel_along;
		n.accel_across = accel_across;
		n.accel_through = accel_through;

		n.gyro_roll = gyro_roll;
		n.gyro_pitch = gyro_pitch;
		n.gyro_yaw = gyro_yaw;
		return n;
	}

	static public void update_state(AltosState state, AltosLink link, AltosConfigData config_data) throws InterruptedException {
		try {
			AltosIMU	imu = new AltosIMU(link);

			if (imu != null)
				state.set_imu(imu);
		} catch (TimeoutException te) {
		}
	}

	public AltosIMU() {
		accel_along = AltosLib.MISSING;
		accel_across = AltosLib.MISSING;
		accel_through = AltosLib.MISSING;

		gyro_roll = AltosLib.MISSING;
		gyro_pitch = AltosLib.MISSING;
		gyro_yaw = AltosLib.MISSING;
	}

	public AltosIMU(int accel_along, int accel_across, int accel_through,
			int gyro_roll, int gyro_pitch, int gyro_yaw) {

		this.accel_along = accel_along;
		this.accel_across = accel_across;
		this.accel_through = accel_through;

		this.gyro_roll = gyro_roll;
		this.gyro_pitch = gyro_pitch;
		this.gyro_yaw = gyro_yaw;
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
