/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

public class AltosTelemetryMegaSensor extends AltosTelemetryStandard {
	int	accel;
	int	pres;
	int	temp;

	int	accel_x;
	int	accel_y;
	int	accel_z;

	int	gyro_x;
	int	gyro_y;
	int	gyro_z;

	int	mag_x;
	int	mag_y;
	int	mag_z;

	int	orient;

	public AltosTelemetryMegaSensor(int[] bytes) {
		super(bytes);

		orient	      = int8(5);
		accel         = int16(6);
		pres          = int32(8);
		temp          = int16(12);

		accel_x	      = int16(14);
		accel_y	      = int16(16);
		accel_z	      = int16(18);

		gyro_x	      = int16(20);
		gyro_y	      = int16(22);
		gyro_z	      = int16(24);

		mag_x	      = int16(26);
		mag_y	      = int16(28);
		mag_z	      = int16(30);
	}

	public void update_state(AltosState state) {
		super.update_state(state);

		state.set_accel(accel);
		state.set_pressure(pres);
		state.set_temperature(temp / 100.0);

		state.set_orient(orient);

		state.set_imu(new AltosIMU(accel_y,	/* along */
					   accel_x,	/* across */
					   accel_z,	/* through */
					   gyro_y,	/* along */
					   gyro_x,	/* across */
					   gyro_z));	/* through */

		state.set_mag(new AltosMag(mag_x, mag_y, mag_z));
	}
}
