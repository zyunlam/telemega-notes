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

public class AltosEepromRecordFull extends AltosEepromRecord {
	public static final int	record_length = 8;

	public static final int max_sat = 12;

	public void update_state(AltosFlightListener state) {
		super.update_state(state);
		AltosGPS	gps;

		/* Flush any pending GPS changes */
		if (state.gps_pending()) {
			switch (cmd()) {
			case AltosLib.AO_LOG_GPS_LAT:
			case AltosLib.AO_LOG_GPS_LON:
			case AltosLib.AO_LOG_GPS_ALT:
			case AltosLib.AO_LOG_GPS_SAT:
			case AltosLib.AO_LOG_GPS_DATE:
				break;
			default:
				state.set_temp_gps();
				break;
			}
		}

		switch (cmd()) {
		case AltosLib.AO_LOG_FLIGHT:
			state.set_state(AltosLib.ao_flight_pad);
			state.set_ground_accel(data16(0));
			state.set_flight(data16(2));
			break;
		case AltosLib.AO_LOG_SENSOR:
			state.set_accel(data16(0));
			state.set_pressure(AltosConvert.barometer_to_pressure(data16(2)));
			break;
		case AltosLib.AO_LOG_PRESSURE:
			state.set_pressure(AltosConvert.barometer_to_pressure(data16(2)));
			break;
		case AltosLib.AO_LOG_TEMP_VOLT:
			state.set_temperature(AltosConvert.thermometer_to_temperature(data16(0)));
			state.set_battery_voltage(AltosConvert.cc_battery_to_voltage(data16(2)));
			break;
		case AltosLib.AO_LOG_DEPLOY:
			state.set_apogee_voltage(AltosConvert.cc_ignitor_to_voltage(data16(0)));
			state.set_main_voltage(AltosConvert.cc_ignitor_to_voltage(data16(2)));
			break;
		case AltosLib.AO_LOG_STATE:
			state.set_state(data16(0));
			break;
		case AltosLib.AO_LOG_GPS_TIME:
			gps = state.make_temp_gps(false);

			gps.hour = data8(0);
			gps.minute = data8(1);
			gps.second = data8(2);

			int flags = data8(3);

			gps.connected = (flags & AltosLib.AO_GPS_RUNNING) != 0;
			gps.locked = (flags & AltosLib.AO_GPS_VALID) != 0;
			gps.nsat = (flags & AltosLib.AO_GPS_NUM_SAT_MASK) >>
				AltosLib.AO_GPS_NUM_SAT_SHIFT;
			break;
		case AltosLib.AO_LOG_GPS_LAT:
			gps = state.make_temp_gps(false);

			int lat32 = data32(0);
			gps.lat = (double) lat32 / 1e7;
			break;
		case AltosLib.AO_LOG_GPS_LON:
			gps = state.make_temp_gps(false);

			int lon32 = data32(0);
			gps.lon = (double) lon32 / 1e7;
			break;
		case AltosLib.AO_LOG_GPS_ALT:
			gps = state.make_temp_gps(false);
			gps.alt = data16(0);
			break;
		case AltosLib.AO_LOG_GPS_SAT:
			gps = state.make_temp_gps(true);
			int svid = data16(0);
			int c_n0 = data16(3);
			gps.add_sat(svid, c_n0);
			break;
		case AltosLib.AO_LOG_GPS_DATE:
			gps = state.make_temp_gps(false);
			gps.year = data8(0) + 2000;
			gps.month = data8(1);
			gps.day = data8(2);
			break;
		}
	}

	public AltosEepromRecord next() {
		int	s = next_start();
		if (s < 0)
			return null;
		return new AltosEepromRecordFull(eeprom, s);
	}

	public AltosEepromRecordFull(AltosEepromNew eeprom, int start) {
		super(eeprom, start, record_length);
	}

	public AltosEepromRecordFull(AltosEepromNew eeprom) {
		this(eeprom, 0);
	}
}
