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

public abstract class AltosFlightListener {

	int flight;

	public int tick;
	int boost_tick;

	AltosGPS temp_gps;
	int temp_gps_sat_tick;

	/* AltosEepromRecord */
	public void set_boost_tick(int boost_tick) {
		if (boost_tick != AltosLib.MISSING)
			this.boost_tick = boost_tick;
	}

	public void set_tick(int tick) {
		if (tick != AltosLib.MISSING)
			this.tick = tick;
	}

	public double time() {
		if (tick == AltosLib.MISSING)
			return AltosLib.MISSING;
		return tick / 100.0;
	}

	public double boost_time() {
		if (boost_tick == AltosLib.MISSING)
			return AltosLib.MISSING;
		return boost_tick / 100.0;
	}

	/* AltosEepromRecordFull */

	public abstract void set_state(int state);
	public abstract void set_ground_accel(double ground_accel);
	public void set_flight(int flight) {
		if (flight != AltosLib.MISSING)
			this.flight = flight;
	}
	public int flight() {
		return flight;
	}

	public abstract void set_accel(double accel);
	public abstract void set_accel_g(double accel_plus_g, double accel_minus_g);
	public abstract void set_pressure(double pa);

	public abstract void set_temperature(double deg_c);
	public abstract void set_battery_voltage(double volts);

	public abstract void set_apogee_voltage(double volts);
	public abstract void set_main_voltage(double volts);

	public void set_temp_gps() {
		temp_gps = null;
	}

	public boolean gps_pending() {
		return temp_gps != null;
	}

	public AltosGPS make_temp_gps(boolean sats) {
		if (temp_gps == null) {
			temp_gps = new AltosGPS();
		}
		if (sats) {
			if (tick != temp_gps_sat_tick)
				temp_gps.cc_gps_sat = null;
			temp_gps_sat_tick = tick;
		}
		return temp_gps;
	}

	public abstract void set_ground_pressure(double ground_pressure);
	public abstract void set_accel_ground(double along, double across, double through);
	public abstract void set_gyro_zero(double roll, double pitch, double yaw);
	public abstract void set_ms5607(int pres_val, int temp_val);
	public abstract void check_imu_wrap(AltosIMU imu);
	public abstract void set_imu(AltosIMU imu);
	public abstract void set_mag(AltosMag mag);
	public abstract void set_pyro_voltage(double volts);
	public abstract void set_ignitor_voltage(double[] voltage);
	public abstract void set_pyro_fired(int pyro_mask);

	public void copy(AltosFlightListener old) {
		flight = old.flight;
		tick = old.tick;
		boost_tick = old.boost_tick;
		temp_gps = old.temp_gps;
		temp_gps_sat_tick = old.temp_gps_sat_tick;
	}

	public void init() {
		flight = AltosLib.MISSING;
		tick = AltosLib.MISSING;
		boost_tick = AltosLib.MISSING;
		temp_gps_sat_tick = AltosLib.MISSING;
	}
}
