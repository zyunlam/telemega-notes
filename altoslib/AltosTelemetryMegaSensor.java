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

	public void update_state(AltosState state) {
		super.update_state(state);

		state.set_accel(accel());
		state.set_pressure(pres());
		state.set_temperature(temp() / 100.0);

		state.set_orient(orient());

		state.set_imu(new AltosIMU(accel_y(),	/* along */
					   accel_x(),	/* across */
					   accel_z(),	/* through */
					   gyro_y(),	/* along */
					   gyro_x(),	/* across */
					   gyro_z()));	/* through */

		state.set_mag(new AltosMag(mag_x(), mag_y(), mag_z()));
	}
}
