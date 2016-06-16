/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

import java.util.concurrent.*;
import java.io.*;

public class AltosMs5607 implements AltosHashable, AltosJsonable {
	public int	reserved;
	public int	sens;
	public int	off;
	public int	tcs;
	public int	tco;
	public int	tref;
	public int	tempsens;
	public int	crc;

	public int	raw_pres;
	public int	raw_temp;
	public int	pa;
	public int	cc;

	static final boolean	ms5611 = false;

	void convert() {
		int	dT;
		int TEMP;
		long OFF;
		long SENS;
		//int P;

		dT = raw_temp - ((int) tref << 8);

		TEMP = (int) (2000 + (((long) dT * (long) tempsens) >> 23));

		if (ms5611) {
			OFF = ((long) off << 16) + (((long) tco * (long) dT) >> 7);

			SENS = ((long) sens << 15) + (((long) tcs * (long) dT) >> 8);
		} else {
			OFF = ((long) off << 17) + (((long) tco * (long) dT) >> 6);

			SENS = ((long) sens << 16) + (((long) tcs * (long) dT) >> 7);
		}

		if (TEMP < 2000) {
			int	T2 = (int) (((long) dT * (long) dT) >> 31);
			int TEMPM = TEMP - 2000;
			long OFF2 = ((long) 61 * (long) TEMPM * (long) TEMPM) >> 4;
			long SENS2 = (long) 2 * (long) TEMPM * (long) TEMPM;
			if (TEMP < 1500) {
				int TEMPP = TEMP + 1500;
				long TEMPP2 = (long) TEMPP * (long) TEMPP;
				OFF2 = OFF2 + 15 * TEMPP2;
				SENS2 = SENS2 + 8 * TEMPP2;
			}
			TEMP -= T2;
			OFF -= OFF2;
			SENS -= SENS2;
		}

		pa = (int) (((((long) raw_pres * SENS) >> 21) - OFF) >> 15);
		cc = TEMP;
	}

	public int set(int in_pres, int in_temp) {
		raw_pres = in_pres;
		raw_temp = in_temp;
		convert();
		return pa;
	}

	public boolean parse_line(String line) {
		String[] items = line.split("\\s+");
		if (line.startsWith("Pressure:")) {
			if (items.length >= 2) {
				raw_pres = Integer.parseInt(items[1]);
			}
		} else if (line.startsWith("Temperature:")) {
			if (items.length >= 2)
				raw_temp = Integer.parseInt(items[1]);
		} else if (line.startsWith("ms5607 reserved:")) {
			if (items.length >= 3)
				reserved = Integer.parseInt(items[2]);
		} else if (line.startsWith("ms5607 sens:")) {
			if (items.length >= 3) {
				sens = Integer.parseInt(items[2]);
			}
		} else if (line.startsWith("ms5607 off:")) {
			if (items.length >= 3)
				off = Integer.parseInt(items[2]);
		} else if (line.startsWith("ms5607 tcs:")) {
			if (items.length >= 3)
				tcs = Integer.parseInt(items[2]);
		} else if (line.startsWith("ms5607 tco:")) {
			if (items.length >= 3)
				tco = Integer.parseInt(items[2]);
		} else if (line.startsWith("ms5607 tref:")) {
			if (items.length >= 3)
				tref = Integer.parseInt(items[2]);
		} else if (line.startsWith("ms5607 tempsens:")) {
			if (items.length >= 3)
				tempsens = Integer.parseInt(items[2]);
		} else if (line.startsWith("ms5607 crc:")) {
			if (items.length >= 3)
				crc = Integer.parseInt(items[2]);
		} else if (line.startsWith("Altitude:")) {
			return false;
		}
		return true;
	}

	static public void update_state(AltosState state, AltosLink link, AltosConfigData config_data) throws InterruptedException {
		try {
			AltosMs5607	ms5607 = new AltosMs5607(link, config_data);

			if (ms5607 != null) {
				state.set_ms5607(ms5607);
				return;
			}
		} catch (TimeoutException te) {
		}
	}

	public AltosMs5607() {
		raw_pres = AltosLib.MISSING;
		raw_temp = AltosLib.MISSING;
		pa = AltosLib.MISSING;
		cc = AltosLib.MISSING;
	}

	public AltosMs5607 (AltosLink link, AltosConfigData config_data) throws InterruptedException, TimeoutException {
		this();
		reserved = config_data.ms5607_reserved;
		sens = config_data.ms5607_sens;
		off = config_data.ms5607_off;
		tcs = config_data.ms5607_tcs;
		tco = config_data.ms5607_tco;
		tref = config_data.ms5607_tref;
		tempsens = config_data.ms5607_tempsens;
		crc = config_data.ms5607_crc;
		link.printf("B\n");
		for (;;) {
			String line = link.get_reply_no_dialog(5000);
			if (line == null) {
				throw new TimeoutException();
			}
			if (!parse_line(line)) {
				break;
			}
		}
		convert();
	}

	public AltosHashSet hashSet() {
		AltosHashSet h = new AltosHashSet();

		h.putInt("reserved", reserved);
		h.putInt("sens", sens);
		h.putInt("off", off);
		h.putInt("tcs", tcs);
		h.putInt("tco", tco);
		h.putInt("tref", tref);
		h.putInt("tempsens", tempsens);
		h.putInt("crc", crc);
		h.putInt("raw_pres", raw_pres);
		h.putInt("raw_temp", raw_temp);
		h.putInt("pa", pa);
		h.putInt("cc", cc);
		return h;
	}

	public AltosJson json() {
		AltosJson j = new AltosJson();

		j.put("reserved", reserved);
		j.put("sens", sens);
		j.put("off", off);
		j.put("tcs", tcs);
		j.put("tco", tco);
		j.put("tref", tref);
		j.put("tempsens", tempsens);
		j.put("crc", crc);
		j.put("raw_pres", raw_pres);
		j.put("raw_temp", raw_temp);
		j.put("pa", pa);
		j.put("cc", cc);
		return j;
	}

	public AltosMs5607(AltosHashSet h) {
		this();

		reserved = h.getInt("reserved", reserved);
		sens = h.getInt("sens", sens);
		off = h.getInt("off", off);
		tcs = h.getInt("tcs", tcs);
		tco = h.getInt("tco", tco);
		tref = h.getInt("tref", tref);
		tempsens = h.getInt("tempsens", tempsens);
		crc = h.getInt("crc", crc);
		raw_pres = h.getInt("raw_pres", raw_pres);
		raw_temp = h.getInt("raw_temp", raw_temp);
		pa = h.getInt("pa", pa);
		cc = h.getInt("cc", cc);
	}

	public static AltosMs5607 fromHashSet(AltosHashSet h, AltosMs5607 def) {
		if (h == null)
			return def;

		return new AltosMs5607(h);
	}

	public AltosMs5607(AltosJson j) {
		this();

		reserved = j.get_int("reserved", reserved);
		sens = j.get_int("sens", sens);
		off = j.get_int("off", off);
		tcs = j.get_int("tcs", tcs);
		tco = j.get_int("tco", tco);
		tref = j.get_int("tref", tref);
		tempsens = j.get_int("tempsens", tempsens);
		crc = j.get_int("crc", crc);
		raw_pres = j.get_int("raw_pres", raw_pres);
		raw_temp = j.get_int("raw_temp", raw_temp);
		pa = j.get_int("pa", pa);
		cc = j.get_int("cc", cc);
	}

	public static AltosMs5607 fromJson(AltosJson j, AltosMs5607 def) {
		if (j == null)
			return def;

		return new AltosMs5607(j);
	}
}
