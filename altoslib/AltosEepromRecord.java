/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

package org.altusmetrum.altoslib_11;

public abstract class AltosEepromRecord implements Comparable<AltosEepromRecord> {

	AltosEepromNew		eeprom;

	int			wide_tick;

	final int		start;
	final int		length;

	public final static int header_length = 4;

	public int cmd() {
		return eeprom.data8(start);
	}

	public int tick() {
		return eeprom.data16(start+2);
	}

	public int data8(int i) {
		i += start + header_length;
		return eeprom.data8(i);
	}

	public int data16(int i) {
		return ((data8(i) | (data8(i+1) << 8)) << 16) >> 16;
	}

	public int data24(int i) {
		return data8(i) | (data8(i+1) << 8) | (data8(i+2) << 16);
	}

	public int data32(int i) {
		return data8(i) | (data8(i+1) << 8) | (data8(i+2) << 16) | (data8(i+3) << 24);
	}

	public boolean valid(int s) {
		return AltosConvert.checksum(eeprom.data, s, length) == 0;
	}

	public boolean valid() {
		return valid(start);
	}

	private int cmdi() {
		if (cmd() == AltosLib.AO_LOG_FLIGHT)
			return 0;
		return 1;
	}

	public AltosConfigData config_data() {
		return eeprom.config_data();
	}

	public int compareTo(AltosEepromRecord o) {
		int	cmd_diff = cmdi() - o.cmdi();

		if (cmd_diff != 0)
			return cmd_diff;

		int	tick_diff = wide_tick - o.wide_tick;

		if (tick_diff != 0)
			return tick_diff;
		return start - o.start;
	}

	/* AltosDataProvider */
	public void provide_data(AltosDataListener listener, AltosCalData cal_data) {
		cal_data.set_tick(tick());
		if (cmd() == AltosLib.AO_LOG_FLIGHT)
			cal_data.set_boost_tick();
		listener.set_time(cal_data.time());

		/* Flush any pending GPS changes */
		if (!AltosLib.is_gps_cmd(cmd())) {
			AltosGPS gps = cal_data.temp_gps();
			if (gps != null) {
				listener.set_gps(gps);
				cal_data.reset_temp_gps();
			}
		}
	}

	public int next_start() {
		int	s = start + length;

		while (s + length <= eeprom.data.size()) {
			if (valid(s))
				return s;
			s += length;
		}
		return -1;
	}

	public boolean hasNext() {
		return next_start() >= 0;
	}

	public abstract AltosEepromRecord next();

	public AltosEepromRecord(AltosEepromNew eeprom, int start, int length) {
		this.eeprom = eeprom;
		this.start = start;
		this.length = length;

		while (start + length < eeprom.data.size() && !valid())
			start += length;
	}
}
