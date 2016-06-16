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

public class AltosGPSSat implements AltosJsonable {
	public int	svid;
	public int	c_n0;

	public AltosGPSSat(int s, int c) {
		svid = s;
		c_n0= c;
	}

	public AltosGPSSat() {
	}

	public AltosJson json() {
		AltosJson j = new AltosJson();
		j.put("svid", svid);
		j.put("c_n0", c_n0);
		return j;
	}

	private AltosGPSSat(AltosJson j) {
		svid = j.get_int("svid", 0);
		c_n0 = j.get_int("c_n0", 0);
	}

	static public AltosGPSSat[] json_array(AltosJson j) {
		if (j == null)
			return null;

		int size = j.size();
		AltosGPSSat[] sats = new AltosGPSSat[size];
		for (int i = 0; i < size; i++)
			sats[i] = new AltosGPSSat(j.get(i));
		return sats;
	}
}

