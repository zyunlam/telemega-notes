/*
 * Copyright © 2010 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_8;

import java.io.*;
import java.util.*;

public class AltosCSV implements AltosWriter {
	File			name;
	PrintStream		out;
	boolean			header_written;
	boolean			seen_boost;
	int			boost_tick;
	LinkedList<AltosState>	pad_states;
	AltosState		state;

	static boolean		has_basic;
	static boolean		has_battery;
	static boolean		has_flight_state;
	static boolean		has_advanced;
	static boolean		has_gps;
	static boolean		has_gps_sat;
	static boolean		has_companion;

	static final int ALTOS_CSV_VERSION = 5;

	/* Version 4 format:
	 *
	 * General info
	 *	version number
	 *	serial number
	 *	flight number
	 *	callsign
	 *	time (seconds since boost)
	 *	clock (tick count / 100)
	 *	rssi
	 *	link quality
	 *
	 * Flight status
	 *	state
	 *	state name
	 *
	 * Basic sensors
	 *	acceleration (m/s²)
	 *	pressure (mBar)
	 *	altitude (m)
	 *	height (m)
	 *	accelerometer speed (m/s)
	 *	barometer speed (m/s)
	 *	temp (°C)
	 *	drogue (V)
	 *	main (V)
	 *
	 * Battery
	 *	battery (V)
	 *
	 * Advanced sensors (if available)
	 *	accel_x (m/s²)
	 *	accel_y (m/s²)
	 *	accel_z (m/s²)
	 *	gyro_x (d/s)
	 *	gyro_y (d/s)
	 *	gyro_z (d/s)
	 *	mag_x (g)
	 *	mag_y (g)
	 *	mag_z (g)
	 *
	 * GPS data (if available)
	 *	connected (1/0)
	 *	locked (1/0)
	 *	nsat (used for solution)
	 *	latitude (°)
	 *	longitude (°)
	 *	altitude (m)
	 *	year (e.g. 2010)
	 *	month (1-12)
	 *	day (1-31)
	 *	hour (0-23)
	 *	minute (0-59)
	 *	second (0-59)
	 *	from_pad_dist (m)
	 *	from_pad_azimuth (deg true)
	 *	from_pad_range (m)
	 *	from_pad_elevation (deg from horizon)
	 *	pdop
	 *	hdop
	 *	vdop
	 *
	 * GPS Sat data
	 *	C/N0 data for all 32 valid SDIDs
	 *
	 * Companion data
	 *	companion_id (1-255. 10 is TeleScience)
	 *	time of last companion data (seconds since boost)
	 *	update_period (0.1-2.55 minimum telemetry interval)
	 *	channels (0-12)
	 *	channel data for all 12 possible channels
	 */

	void write_general_header() {
		out.printf("version,serial,flight,call,time,clock,rssi,lqi");
	}

	void write_general(AltosState state) {
		out.printf("%s, %d, %d, %s, %8.2f, %8.2f, %4d, %3d",
			   ALTOS_CSV_VERSION, state.serial, state.flight, state.callsign,
			   (double) state.time_since_boost(), (double) state.tick / 100.0,
			   state.rssi,
			   state.status & 0x7f);
	}

	void write_flight_header() {
		out.printf("state,state_name");
	}

	void write_flight(AltosState state) {
		out.printf("%d,%8s", state.state, state.state_name());
	}

	void write_basic_header() {
		out.printf("acceleration,pressure,altitude,height,accel_speed,baro_speed,temperature,drogue_voltage,main_voltage");
	}

	void write_basic(AltosState state) {
		out.printf("%8.2f,%10.2f,%8.2f,%8.2f,%8.2f,%8.2f,%5.1f,%5.2f,%5.2f",
			   state.acceleration(),
			   state.pressure(),
			   state.altitude(),
			   state.height(),
			   state.speed(),
			   state.speed(),
			   state.temperature,
			   state.apogee_voltage,
			   state.main_voltage);
	}

	void write_battery_header() {
		out.printf("battery_voltage");
	}

	void write_battery(AltosState state) {
		out.printf("%5.2f", state.battery_voltage);
	}

	void write_advanced_header() {
		out.printf("accel_x,accel_y,accel_z,gyro_x,gyro_y,gyro_z,mag_x,mag_y,mag_z");
	}

	void write_advanced(AltosState state) {
		out.printf("%7.2f,%7.2f,%7.2f,%7.2f,%7.2f,%7.2f,%7.2f,%7.2f,%7.2f",
			   state.accel_along(), state.accel_across(), state.accel_through(),
			   state.gyro_roll(), state.gyro_pitch(), state.gyro_yaw(),
			   state.mag_along(), state.mag_across(), state.mag_through());
	}

	void write_gps_header() {
		out.printf("connected,locked,nsat,latitude,longitude,altitude,year,month,day,hour,minute,second,pad_dist,pad_range,pad_az,pad_el,pdop,hdop,vdop");
	}

	void write_gps(AltosState state) {
		AltosGPS	gps = state.gps;
		if (gps == null)
			gps = new AltosGPS();

		AltosGreatCircle from_pad = state.from_pad;
		if (from_pad == null)
			from_pad = new AltosGreatCircle();

		out.printf("%2d,%2d,%3d,%12.7f,%12.7f,%8.1f,%5d,%3d,%3d,%3d,%3d,%3d,%9.0f,%9.0f,%4.0f,%4.0f,%6.1f,%6.1f,%6.1f",
			   gps.connected?1:0,
			   gps.locked?1:0,
			   gps.nsat,
			   gps.lat,
			   gps.lon,
			   gps.alt,
			   gps.year,
			   gps.month,
			   gps.day,
			   gps.hour,
			   gps.minute,
			   gps.second,
			   from_pad.distance,
			   state.range,
			   from_pad.bearing,
			   state.elevation,
			   gps.pdop,
			   gps.hdop,
			   gps.vdop);
	}

	void write_gps_sat_header() {
		for(int i = 1; i <= 32; i++) {
			out.printf("sat%02d", i);
			if (i != 32)
				out.printf(",");
		}
	}

	void write_gps_sat(AltosState state) {
		AltosGPS	gps = state.gps;
		for(int i = 1; i <= 32; i++) {
			int	c_n0 = 0;
			if (gps != null && gps.cc_gps_sat != null) {
				for(int j = 0; j < gps.cc_gps_sat.length; j++)
					if (gps.cc_gps_sat[j].svid == i) {
						c_n0 = gps.cc_gps_sat[j].c_n0;
						break;
					}
			}
			out.printf ("%3d", c_n0);
			if (i != 32)
				out.printf(",");
		}
	}

	void write_companion_header() {
		out.printf("companion_id,companion_time,companion_update,companion_channels");
		for (int i = 0; i < 12; i++)
			out.printf(",companion_%02d", i);
	}

	void write_companion(AltosState state) {
		AltosCompanion companion = state.companion;

		int	channels_written = 0;
		if (companion == null) {
			out.printf("0,0,0,0");
		} else {
			out.printf("%3d,%5.2f,%5.2f,%2d",
				   companion.board_id,
				   (companion.tick - boost_tick) / 100.0,
				   companion.update_period / 100.0,
				   companion.channels);
			for (; channels_written < companion.channels; channels_written++)
				out.printf(",%5d", companion.companion_data[channels_written]);
		}
		for (; channels_written < 12; channels_written++)
			out.printf(",0");
	}

	void write_header() {
		out.printf("#"); write_general_header();
		if (has_flight_state) {
			out.printf(",");
			write_flight_header();
		}
		if (has_basic) {
			out.printf(",");
			write_basic_header();
		}
		if (has_battery) {
			out.printf(",");
			write_battery_header();
		}
		if (has_advanced) {
			out.printf(",");
			write_advanced_header();
		}
		if (has_gps) {
			out.printf(",");
			write_gps_header();
		}
		if (has_gps_sat) {
			out.printf(",");
			write_gps_sat_header();
		}
		if (has_companion) {
			out.printf(",");
			write_companion_header();
		}
		out.printf ("\n");
	}

	void write_one(AltosState state) {
		write_general(state);
		if (has_flight_state) {
			out.printf(",");
			write_flight(state);
		}
		if (has_basic) {
			out.printf(",");
			write_basic(state);
		}
		if (has_battery) {
			out.printf(",");
			write_battery(state);
		}
		if (has_advanced) {
			out.printf(",");
			write_advanced(state);
		}
		if (has_gps) {
			out.printf(",");
			write_gps(state);
		}
		if (has_gps_sat) {
			out.printf(",");
			write_gps_sat(state);
		}
		if (has_companion) {
			out.printf(",");
			write_companion(state);
		}
		out.printf ("\n");
	}

	private void flush_pad() {
		while (!pad_states.isEmpty()) {
			write_one (pad_states.remove());
		}
	}

	private void write(AltosState state) {
		if (state.state == AltosLib.ao_flight_startup)
			return;
		if (!header_written) {
			write_header();
			header_written = true;
		}
		if (!seen_boost) {
			if (state.state >= AltosLib.ao_flight_boost) {
				seen_boost = true;
				boost_tick = state.tick;
				flush_pad();
			}
		}
		if (seen_boost)
			write_one(state);
		else
			pad_states.add(state);
	}

	private PrintStream out() {
		return out;
	}

	public void close() {
		if (!pad_states.isEmpty()) {
			boost_tick = pad_states.element().tick;
			flush_pad();
		}
		out.close();
	}

	public void write(AltosStateIterable states) {
		states.write_comments(out());

		has_flight_state = false;
		has_basic = false;
		has_battery = false;
		has_advanced = false;
		has_gps = false;
		has_gps_sat = false;
		has_companion = false;
		for (AltosState state : states) {
			if (state.state != AltosLib.ao_flight_stateless && state.state != AltosLib.ao_flight_invalid && state.state != AltosLib.ao_flight_startup)
				has_flight_state = true;
			if (state.acceleration() != AltosLib.MISSING || state.pressure() != AltosLib.MISSING)
				has_basic = true;
			if (state.battery_voltage != AltosLib.MISSING)
				has_battery = true;
			if (state.accel_across() != AltosLib.MISSING)
				has_advanced = true;
			if (state.gps != null) {
				has_gps = true;
				if (state.gps.cc_gps_sat != null)
					has_gps_sat = true;
			}
			if (state.companion != null)
				has_companion = true;
		}
		for (AltosState state : states)
			write(state);
	}

	public AltosCSV(PrintStream in_out, File in_name) {
		name = in_name;
		out = in_out;
		pad_states = new LinkedList<AltosState>();
	}

	public AltosCSV(File in_name) throws FileNotFoundException {
		this(new PrintStream(in_name), in_name);
	}

	public AltosCSV(String in_string) throws FileNotFoundException {
		this(new File(in_string));
	}
}
