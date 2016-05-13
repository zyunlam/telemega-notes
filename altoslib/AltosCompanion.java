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

package org.altusmetrum.altoslib_11;

import java.io.*;

public class AltosCompanion implements AltosHashable {
	public final static int	board_id_telescience = 0x0a;
	public final static int	MAX_CHANNELS = 12;

	public int	tick;
	public int	board_id;
	public int	update_period;
	public int	channels;
	public int[]	companion_data;

	public AltosCompanion(int in_channels) {
		channels = in_channels;
		if (channels < 0)
			channels = 0;
		if (channels > MAX_CHANNELS)
			channels = MAX_CHANNELS;
		companion_data = new int[channels];
	}

	public AltosHashSet hashSet() {
		AltosHashSet h = new AltosHashSet();

		h.putInt("tick", tick);
		h.putInt("board_id", board_id);
		h.putInt("update_period", update_period);
		h.putInt("channels", channels);
		h.putIntArray("companion_data", companion_data);
		return h;
	}

	public AltosCompanion(AltosHashSet h) {
		tick = h.getInt("tick", tick);
		board_id = h.getInt("board_id", board_id);
		update_period = h.getInt("update_period", update_period);
		channels = h.getInt("channels", channels);
		companion_data = h.getIntArray("companion_data", new int[channels]);
	}

	public static AltosCompanion fromHashSet(AltosHashSet h, AltosCompanion def) {
		if (h == null)
			return def;

		return new AltosCompanion(h);
	}
}
