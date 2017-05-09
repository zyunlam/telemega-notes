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

	public boolean valid() {
		return AltosConvert.checksum(eeprom.data, start, length) == 0;
	}

	private int cmdi() {
		if (cmd() == AltosLib.AO_LOG_FLIGHT)
			return 0;
		return 1;
	}

	public int compareTo(AltosEepromRecord o) {
		int	cmd_diff = cmdi() - o.cmdi();

		if (cmd_diff != 0)
			return cmd_diff;

		int	tick_diff = tick() - o.tick();

		if (tick_diff != 0)
			return tick_diff;
		return start - o.start;
	}

	public void update_state(AltosState state) {
		if (cmd() == AltosLib.AO_LOG_FLIGHT)
			state.set_boost_tick(tick());
		else
			state.set_tick(tick());
	}

	public boolean hasNext() {
		return start + length * 2 < eeprom.data.size();
	}

	public abstract AltosEepromRecord next();

	public AltosEepromRecord(AltosEepromNew eeprom, int start, int length) {
		this.eeprom = eeprom;
		this.start = start;
		this.length = length;
	}
}
