/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_11;

public class AltosEepromRecordMini extends AltosEepromRecord {
	public static final int	record_length = 16;

	/* AO_LOG_FLIGHT elements */
	public int flight() { return data16(0); }
	public int ground_pres() { return data32(4); }

	/* AO_LOG_STATE elements */
	public int state() { return data16(0); }
	public int reason() { return data16(2); }

	/* AO_LOG_SENSOR elements */
	public int pres() { return data24(0); }
	public int temp() { return data24(3); }
	public int sense_a() { return data16(6); }
	public int sense_m() { return data16(8); }
	public int v_batt() { return data16(10); }

	private int log_format() {
		return eeprom.config_data().log_format;
	}

	private double battery_voltage(int sensor) {
		int log_format = log_format();
		if (log_format == AltosLib.AO_LOG_FORMAT_EASYMINI)
			return AltosConvert.easy_mini_voltage(sensor, eeprom.config_data().serial);
		if (log_format == AltosLib.AO_LOG_FORMAT_TELEMINI2)
			return AltosConvert.tele_mini_2_voltage(sensor);
		if (log_format == AltosLib.AO_LOG_FORMAT_TELEMINI3)
			return AltosConvert.tele_mini_3_battery_voltage(sensor);
		return -1;
	}

	private double pyro_voltage(int sensor) {
		int log_format = log_format();
		if (log_format == AltosLib.AO_LOG_FORMAT_EASYMINI)
			return AltosConvert.easy_mini_voltage(sensor, eeprom.config_data().serial);
		if (log_format == AltosLib.AO_LOG_FORMAT_TELEMINI2)
			return AltosConvert.tele_mini_2_voltage(sensor);
		if (log_format == AltosLib.AO_LOG_FORMAT_TELEMINI3)
			return AltosConvert.tele_mini_3_pyro_voltage(sensor);
		return -1;
	}

	public void update_state(AltosState state) {
		super.update_state(state);

		switch (cmd()) {
		case AltosLib.AO_LOG_FLIGHT:
			state.set_flight(flight());
			state.set_ground_pressure(ground_pres());
			break;
		case AltosLib.AO_LOG_STATE:
			state.set_state(state());
			break;
		case AltosLib.AO_LOG_SENSOR:
			state.set_ms5607(pres(), temp());
			state.set_apogee_voltage(pyro_voltage(sense_a()));
			state.set_main_voltage(pyro_voltage(sense_m()));
			state.set_battery_voltage(battery_voltage(v_batt()));
			break;
		}
	}

	public AltosEepromRecord next() {
		int	s = next_start();
		if (s < 0)
			return null;
		return new AltosEepromRecordMini(eeprom, s);
	}

	public AltosEepromRecordMini(AltosEepromNew eeprom, int start) {
		super(eeprom, start, record_length);
	}

	public AltosEepromRecordMini(AltosEepromNew eeprom) {
		this(eeprom, 0);
	}
}
