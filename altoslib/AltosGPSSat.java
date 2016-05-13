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
import java.text.*;
import java.util.*;
import java.util.concurrent.*;

public class AltosGPSSat {
	public int	svid;
	public int	c_n0;

	public AltosGPSSat(int s, int c) {
		svid = s;
		c_n0= c;
	}

	public AltosGPSSat() {
	}

	public AltosHashSet hashSet() {
		AltosHashSet h = new AltosHashSet();
		h.putInt("svid", svid);
		h.putInt("c_n0", c_n0);
		return h;
	}

	private AltosGPSSat(AltosHashSet h) {
		svid = h.getInt("svid", 0);
		c_n0 = h.getInt("c_n0", 0);
	}

	static public AltosGPSSat fromHashSet(AltosHashSet h, AltosGPSSat def) {
		if (h == null)
			return def;
		return new AltosGPSSat(h);
	}

	static public AltosGPSSat[] array(String string) {

		if (string == null)
			return null;

		try {
			StringReader 		reader = new StringReader(string);
			ArrayList<AltosGPSSat>	array = new ArrayList<AltosGPSSat>();
			String			element;

			while ((element = AltosHashSet.get_token(reader)) != null) {
				AltosGPSSat sat = AltosGPSSat.fromHashSet(AltosHashSet.fromString(element), null);
				if (sat != null)
					array.add(sat);
			}
			return array.toArray(new AltosGPSSat[0]);
		} catch (IOException ie) {
			return null;
		}
	}

	public static String toString(AltosGPSSat[] sats) {
		if (sats == null)
			return null;

		try {
			StringWriter		writer = new StringWriter();

			for (AltosGPSSat g : sats) {
				String		element = g.hashSet().toString();
				AltosHashSet.put_token(writer, element);
			}
			return writer.toString();
		} catch (IOException ie) {
			return null;
		}
	}
}

