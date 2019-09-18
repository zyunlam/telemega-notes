/*
 * Copyright © 2012 Mike Beattie <mike@ethernal.org>
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

package org.altusmetrum.AltosDroid;

import java.util.*;
import org.altusmetrum.altoslib_13.*;

public class TelemetryState {
	public static final int CONNECT_NONE         = 0;
	public static final int CONNECT_DISCONNECTED = 1;
	public static final int CONNECT_CONNECTING   = 2;
	public static final int CONNECT_CONNECTED    = 3;

	int		connect;
	DeviceAddress	address;
	AltosConfigData	config;
	int		crc_errors;
	double		receiver_battery;
	double		frequency;
	int		telemetry_rate;

	boolean		quiet;

	HashMap<Integer,AltosState>	states;

	int		latest_serial;

	public TelemetryState() {
		connect = CONNECT_NONE;
		config = null;
		states = new HashMap<Integer,AltosState>();
		crc_errors = 0;
		receiver_battery = AltosLib.MISSING;
		frequency = AltosPreferences.frequency(0);
		telemetry_rate = AltosPreferences.telemetry_rate(0);
	}
}
