/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

import android.content.Context;
import org.altusmetrum.altoslib_5.*;

public class AltosDroidPreferences extends AltosPreferences {

	/* Active device preference name */
	final static String activeDevicePreference = "ACTIVE-DEVICE";

	static String active_device_address;

	public static void init(Context context) {
		AltosPreferences.init(new AltosDroidPreferencesBackend(context));

		active_device_address = backend.getString(activeDevicePreference, null);
	}

	public static void set_active_device(String address) {
		synchronized(backend) {
			active_device_address = address;
			backend.putString(activeDevicePreference, active_device_address);
			flush_preferences();
		}
	}

	public static String active_device() {
		synchronized(backend) {
			return active_device_address;
		}
	}
}
