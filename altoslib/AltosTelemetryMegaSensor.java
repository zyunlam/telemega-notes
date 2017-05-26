/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_11;

public class AltosTelemetryMegaSensor extends AltosTelemetryStandard {
	int	orient() { return int8(5); }

	int	accel() { return int16(6); }
	int	pres() { return int32(8); }
	int	temp() { return int16(12); }

	int	accel_x() { return int16(14); }
	int	accel_y() { return int16(16); }
	int	accel_z() { return int16(18); }

	int	gyro_x() { return int16(20); }
	int	gyro_y() { return int16(22); }
	int	gyro_z() { return int16(24); }

	int	mag_x() { return int16(26); }
	int	mag_y() { return int16(28); }
	int	mag_z() { return int16(30); }

	public AltosTelemetryMegaSensor(int[] bytes) throws AltosCRCException {
		super(bytes);
	}

	public void provide_data(AltosDataListener listener, AltosCalData cal_data) {
		super.provide_data(listener, cal_data);

		listener.set_acceleration(cal_data.acceleration(accel()));
		listener.set_pressure(pres());
		listener.set_temperature(temp() / 100.0);

		listener.set_orient(orient());

		/* XXX we have no calibration data for these values */

		if (cal_data.accel_zero_along == AltosLib.MISSING)
			cal_data.set_accel_zero(0, 0, 0);
		if (cal_data.gyro_zero_roll == AltosLib.MISSING)
			cal_data.set_gyro_zero(0, 0, 0);

		int	accel_along = accel_y();
		int	accel_across = accel_x();
		int	accel_through = accel_z();
		int	gyro_roll = gyro_y();
		int	gyro_pitch = gyro_x();
		int	gyro_yaw = gyro_z();

		int	mag_along = mag_x();
		int	mag_across = mag_y();
		int	mag_through = mag_z();

		listener.set_accel(cal_data.accel_along(accel_along),
				   cal_data.accel_across(accel_across),
				   cal_data.accel_through(accel_through));
		listener.set_gyro(cal_data.gyro_roll(gyro_roll),
				  cal_data.gyro_pitch(gyro_pitch),
				  cal_data.gyro_yaw(gyro_yaw));
		listener.set_mag(cal_data.mag_along(mag_along),
				 cal_data.mag_across(mag_across),
				 cal_data.mag_through(mag_through));
	}
}
