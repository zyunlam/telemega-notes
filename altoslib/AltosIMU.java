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

package org.altusmetrum.altoslib_14;

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

	public static final double	counts_per_g_mpu = 2048.0;
	public static final double	counts_per_g_bmx = 2048.0;

	int			pad_orientation;

	private static double counts_per_g(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
		case imu_type_easymega_v2:
			return counts_per_g_mpu;
		case  imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return counts_per_g_bmx;
		default:
			return AltosLib.MISSING;
		}
	}

	public static double convert_accel(double counts, int imu_type) {
		return counts / counts_per_g(imu_type) * AltosConvert.gravity;
	}

	public static final double 	GYRO_FULLSCALE_DEGREES_MPU = 2000.0;
	public static final double	GYRO_COUNTS_MPU = 32767.0;
	public static final double	counts_per_degree_mpu = GYRO_COUNTS_MPU / GYRO_FULLSCALE_DEGREES_MPU;
	public static final double 	GYRO_FULLSCALE_DEGREES_BMX = 2000.0;
	public static final double	GYRO_COUNTS_BMX = 32767.0;
	public static final double	counts_per_degree_bmx = GYRO_COUNTS_BMX / GYRO_FULLSCALE_DEGREES_BMX;

	private static double counts_per_degree(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
		case imu_type_easymega_v2:
			return counts_per_degree_mpu;
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return counts_per_degree_bmx;
		default:
			return AltosLib.MISSING;
		}
	}

	public static double gyro_degrees_per_second(double counts, int imu_type) {
		return counts / counts_per_degree(imu_type);
	}

	public static final int imu_axis_x = 0;
	public static final int imu_axis_y = 1;
	public static final int imu_axis_z = 2;

	public static final double MAG_FULLSCALE_GAUSS_MPU = 48.00;	/* 4800µT */
	public static final double MAG_COUNTS_MPU = 32767.0;
	public static final double counts_per_gauss_mpu = MAG_COUNTS_MPU / MAG_FULLSCALE_GAUSS_MPU;

	public static final double counts_per_gauss_bmx = 100.0;	/* BMX driver converts to µT */

	public static double counts_per_gauss(int imu_type, int axis) {
		switch(imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_easymega_v1:
			return AltosMag.counts_per_gauss;
		case imu_type_telemega_v3:
		case imu_type_easymega_v2:
			return counts_per_gauss_mpu;
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return 100.0;
		default:
			return AltosLib.MISSING;
		}
	}

	public static double convert_gauss(double counts, int imu_type, int imu_axis) {
		return counts / counts_per_gauss(imu_type, imu_axis);
	}

	public boolean parse_string(String line, AltosLink link) {
		if (!line.startsWith("Accel:"))
			return false;

		try {
			pad_orientation = link.config_data().pad_orientation;
		} catch (Exception e) {
			return false;
		}

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

	public static final int imu_type_telemega_v1_v2 = 0;	/* MPU6000 */
	public static final int imu_type_telemega_v3 = 1;	/* MPU9250 */
	public static final int imu_type_telemega_v4 = 2;	/* BMX160 */

	public static final int imu_type_easymega_v1 = 3;	/* MPU6000 */
	public static final int imu_type_easymega_v2 = 4;	/* MPU9250 */

	public static final int imu_type_easytimer_v1 = 5;	/* BMX160 */

	private int accel_across(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return accel_x;
		case imu_type_easymega_v2:
			return -accel_y;
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return -accel_y;
		default:
			return AltosLib.MISSING;
		}
	}

	private int accel_along(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return accel_y;
		case imu_type_easymega_v2:
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return accel_x;
		default:
			return AltosLib.MISSING;
		}
	}

	private int accel_through(int imu_type) {
		return accel_z;
	}

	private int gyro_roll(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return gyro_y;
		case imu_type_easymega_v2:
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return gyro_x;
		default:
			return AltosLib.MISSING;
		}
	}

	private int gyro_pitch(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return gyro_x;
		case imu_type_easymega_v2:
			return -gyro_y;
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return -gyro_y;
		default:
			return AltosLib.MISSING;
		}
	}

	private int gyro_yaw(int imu_type) {
		return gyro_z;
	}

	public static int mag_across_axis(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return imu_axis_x;
		case imu_type_easymega_v2:
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return imu_axis_y;
		default:
			return AltosLib.MISSING;
		}
	}

	private int mag_across(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return mag_x;
		case imu_type_easymega_v2:
			return -mag_y;
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return mag_y;
		default:
			return AltosLib.MISSING;
		}
	}

	public static int mag_along_axis(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return imu_axis_y;
		case imu_type_easymega_v2:
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return imu_axis_x;
		default:
			return AltosLib.MISSING;
		}
	}

	private int mag_along(int imu_type) {
		switch (imu_type) {
		case imu_type_telemega_v1_v2:
		case imu_type_telemega_v3:
		case imu_type_easymega_v1:
			return mag_y;
		case imu_type_easymega_v2:
		case imu_type_telemega_v4:
		case imu_type_easytimer_v1:
			return mag_x;
		default:
			return AltosLib.MISSING;
		}
	}

	public static int mag_through_axis(int imu_type) {
		return imu_axis_z;
	}

	private int mag_through(int imu_type) {
		return mag_z;
	}

	static public void provide_data(AltosDataListener listener, AltosLink link, int imu_type) throws InterruptedException {
		try {
			AltosIMU	imu = new AltosIMU(link);
			AltosCalData	cal_data = listener.cal_data();

			cal_data.set_imu_type(imu_type);

			if (imu != null) {
				listener.set_gyro(cal_data.gyro_roll(imu.gyro_roll(imu_type)),
						  cal_data.gyro_pitch(imu.gyro_pitch(imu_type)),
						  cal_data.gyro_yaw(imu.gyro_yaw(imu_type)));
				listener.set_accel_ground(cal_data.accel_along(imu.accel_along(imu_type)),
							  cal_data.accel_across(imu.accel_across(imu_type)),
							  cal_data.accel_through(imu.accel_through(imu_type)));
				listener.set_accel(cal_data.accel_along(imu.accel_along(imu_type)),
						   cal_data.accel_across(imu.accel_across(imu_type)),
						   cal_data.accel_through(imu.accel_through(imu_type)));
				if (imu.mag_x != AltosLib.MISSING) {
					listener.set_mag(cal_data.mag_along(imu.mag_along(imu_type)),
							 cal_data.mag_across(imu.mag_across(imu_type)),
							 cal_data.mag_through(imu.mag_through(imu_type)));
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
			if (parse_string(line, link))
				break;
		}
	}
}
