/*
 * Copyright © 2012 Mike Beattie <mike@ethernal.org>
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

package org.altusmetrum.AltosDroid;

import org.altusmetrum.altoslib_6.*;
import android.location.Location;

public class TelemetryState {
	public static final int CONNECT_NONE         = 0;
	public static final int CONNECT_DISCONNECTED = 1;
	public static final int CONNECT_CONNECTING   = 2;
	public static final int CONNECT_CONNECTED    = 3;

	int		connect;
	DeviceAddress	address;
	AltosConfigData	config;
	AltosState	state;
	Location	location;
	int		crc_errors;
	double		frequency;
	int		telemetry_rate;

	public TelemetryState() {
		connect = CONNECT_NONE;
		config = null;
		state = null;
		location = null;
		crc_errors = 0;
		frequency = AltosPreferences.frequency(0);
		telemetry_rate = AltosPreferences.telemetry_rate(0);
	}
}
