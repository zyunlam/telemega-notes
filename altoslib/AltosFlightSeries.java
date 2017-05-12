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

import java.util.*;

public class AltosFlightSeries extends AltosFlightListener {

	int flight;

	int tick;
	int boost_tick;

	AltosGPS temp_gps;
	int temp_gps_sat_tick;

	AltosMs5607 ms5607;

	public ArrayList<AltosTimeSeries> series;

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

	public AltosTimeSeries make_series(String label, AltosUnits units) {
		return new AltosTimeSeries(label, units);
	}

	public AltosTimeSeries add_series(String label, AltosUnits units) {
		System.out.printf("add series %s\n", label);
		AltosTimeSeries s = make_series(label, units);
		series.add(s);
		return s;
	}

	/* AltosEepromRecordFull */

	AltosTimeSeries state_series;

	public static final String state_name = "State";

	public void set_state(int state) {
		if (state_series == null)
			state_series = add_series(state_name, null);
		state_series.add(time(), state);
	}

	public void set_flight(int flight) {
		if (flight != AltosLib.MISSING)
			this.flight = flight;
	}
	public int flight() {
		return flight;
	}

	AltosTimeSeries	accel_series;

	double accel_plus_g, accel_minus_g;

	public static final String accel_name = "Accel";

	public  void set_accel(double accel) {
		if (accel_series == null)
			accel_series = add_series(accel_name, AltosConvert.accel);
		double counts_per_g = (accel_minus_g - accel_plus_g) / 2.0;
		double counts_per_mss = counts_per_g / 9.80665;
		double mss = (accel_plus_g - accel) / counts_per_mss;

		accel_series.add(time(), mss);
	}


	public  void set_accel_g(double accel_plus_g, double accel_minus_g) {
		this.accel_plus_g = accel_plus_g;
		this.accel_minus_g = accel_minus_g;
	}

	public void set_config_data(AltosConfigData config_data) {
//		if (config_data.callsign != null)
//			set_callsign(config_data.callsign);
		if (config_data.accel_cal_plus != AltosLib.MISSING &&
		    config_data.accel_cal_minus != AltosLib.MISSING)
			set_accel_g(config_data.accel_cal_plus, config_data.accel_cal_minus);
//		if (config_data.product != null)
//			set_product(config_data.product);
//		if (config_data.log_format != AltosLib.MISSING)
//			set_log_format(config_data.log_format);
//		if (config_data.serial != AltosLib.MISSING)
//			set_serial(config_data.serial);
		AltosMs5607 ms5607 = new AltosMs5607(config_data);
		if (ms5607.valid_config())
			this.ms5607 = ms5607;
	}

	public  void set_ground_accel(double ground_accel) {
	}

	AltosTimeSeries pressure_series;

	public static final String pressure_name = "Pressure";

	public  void set_pressure(double pa) {
		if (pressure_series == null)
			pressure_series = add_series(pressure_name, AltosConvert.pressure);
		pressure_series.add(time(), pa);
	}

	public  void set_temperature(double deg_c) {
	}

	public  void set_battery_voltage(double volts) {
	}

	public  void set_apogee_voltage(double volts) {
	}

	public  void set_main_voltage(double volts) {
	}

	AltosTimeSeries	sats_in_view;
	AltosTimeSeries sats_in_soln;
	AltosTimeSeries gps_altitude;
	AltosTimeSeries gps_ground_speed;
	AltosTimeSeries gps_ascent_rate;
	AltosTimeSeries gps_course;
	AltosTimeSeries gps_speed;

	public static final String sats_in_view_name = "Satellites in view";
	public static final String sats_in_soln_name = "Satellites in solution";
	public static final String gps_altitude_name = "GPS Altitude";

	public void set_temp_gps() {
		if (sats_in_view == null) {
			sats_in_view = add_series("Satellites in view", null);
			sats_in_soln = add_series("Satellites in solution", null);
			gps_altitude = add_series("GPS Altitude", AltosConvert.height);
			gps_ground_speed = add_series("GPS Ground Speed", AltosConvert.speed);
			gps_ascent_rate = add_series("GPS Ascent Rate", AltosConvert.speed);
			gps_course = add_series("GPS Course", null);
			gps_speed = add_series("GPS Speed", null);
		}

		/* XXX capture GPS data */
		super.set_temp_gps();
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

	public  void set_ground_pressure(double ground_pressure) {
	}

	public  void set_accel_ground(double along, double across, double through) {
	}

	public  void set_gyro_zero(double roll, double pitch, double yaw) {
	}

	public  void set_ms5607(int pres_val, int temp_val) {
		if (ms5607 != null) {
			ms5607.set(pres_val, temp_val);

			set_pressure(ms5607.pa);
			set_temperature(ms5607.cc / 100.0);
		}
	}

	public  void check_imu_wrap(AltosIMU imu) {
	}

	public  void set_imu(AltosIMU imu) {
	}

	public  void set_mag(AltosMag mag) {
	}

	public  void set_pyro_voltage(double volts) {
	}

	public  void set_ignitor_voltage(double[] voltage) {
	}

	public  void set_pyro_fired(int pyro_mask) {
	}

	public void init() {
		flight = AltosLib.MISSING;
		tick = AltosLib.MISSING;
		boost_tick = AltosLib.MISSING;
		temp_gps_sat_tick = AltosLib.MISSING;
		series = new ArrayList<AltosTimeSeries>();
	}

	public void copy(AltosFlightSeries old) {
		super.copy(old);
	}

	public AltosTimeSeries[] series() {
		return series.toArray(new AltosTimeSeries[0]);
	}

	public AltosFlightSeries() {
		init();
	}
}
