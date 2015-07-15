/*
 * Copyright © 2015 Keith Packard <keithp@keithp.com>
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

import java.util.*;
import java.io.*;
import android.location.Location;
import org.altusmetrum.altoslib_7.*;

public interface AltosDroidMapInterface {
	public void onCreateView(AltosDroid altos_droid);

	public void set_visible(boolean visible);

	public void center(double lat, double lon, double accuracy);

	public void show(TelemetryState telem_state, AltosState state, AltosGreatCircle from_receiver, Location receiver);
}
