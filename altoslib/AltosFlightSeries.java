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

public class AltosFlightSeries extends AltosDataListener {

	public ArrayList<AltosTimeSeries> series;

	public int[] indices() {
		int[] indices = new int[series.size()];
		for (int i = 0; i < indices.length; i++)
			indices[i] = 0;
		return indices;
	}

	private double time(int id, int index) {
		AltosTimeSeries		s = series.get(id);
		if (index < s.values.size())
			return s.values.get(index).time;
		return Double.POSITIVE_INFINITY;
	}

	public boolean step_indices(int[] indices) {
		double	min_next = time(0, indices[0]+1);

		for (int i = 1; i < indices.length; i++) {
			double next = time(i, indices[i]+1);
			if (next < min_next)
				min_next = next;
		}

		if (min_next == Double.POSITIVE_INFINITY)
			return false;

		for (int i = 0; i < indices.length; i++) {
			double	t = time(i, indices[i] + 1);

			if (t <= min_next)
				indices[i]++;
		}
		return true;
	}

	public double time(int[] indices) {
		double max = time(0, indices[0]);

		for (int i = 1; i < indices.length; i++) {
			double t = time(i, indices[i]);
			if (t >= max)
				max = t;
		}
		return max;
	}

	public double value(String name, int[] indices) {
		for (int i = 0; i < indices.length; i++) {
			AltosTimeSeries	s = series.get(i);
			if (s.label.equals(name))
				return s.values.get(indices[i]).value;
		}
		return AltosLib.MISSING;
	}

	public AltosTimeSeries make_series(String label, AltosUnits units) {
		return new AltosTimeSeries(label, units);
	}

	public void add_series(AltosTimeSeries s) {
		series.add(s);
	}

	public AltosTimeSeries add_series(String label, AltosUnits units) {
		AltosTimeSeries s = make_series(label, units);
		add_series(s);
		return s;
	}

	public void remove_series(AltosTimeSeries s) {
		series.remove(s);
	}

	public boolean has_series(String label) {
		for (AltosTimeSeries s : series)
			if (s.label.equals(label))
				return true;
		return false;
	}

	AltosTimeSeries state_series;

	public static final String state_name = "State";

	public void set_state(int state) {
		this.state = state;
		if (state_series == null)
			state_series = add_series(state_name, AltosConvert.state_name);
		else if ((int) state_series.get(state_series.size()-1).value == state)
			return;
		state_series.add(time(), state);
	}

	AltosTimeSeries	accel_series;

	public static final String accel_name = "Accel";

	public void set_acceleration(double acceleration) {
		if (accel_series == null) {
			System.out.printf("set acceleration %g\n", acceleration);
			accel_series = add_series(accel_name, AltosConvert.accel);
		}
		accel_series.add(time(), acceleration);
	}

	private void compute_accel() {
		if (accel_series != null)
			return;

		if (speed_series != null) {
			AltosTimeSeries temp_series = make_series(accel_name, AltosConvert.accel);
			speed_series.differentiate(temp_series);
			accel_series = add_series(accel_name, AltosConvert.accel);
			temp_series.filter(accel_series, 0.25);
		}
	}

	public void set_received_time(long received_time) {
	}

	AltosTimeSeries rssi_series;

	public static final String rssi_name = "RSSI";

	AltosTimeSeries status_series;

	public static final String status_name = "Status";

	public void set_rssi(int rssi, int status) {
		if (rssi_series == null)
			rssi_series = add_series(rssi_name, null);
		rssi_series.add(time(), rssi);
		if (status_series == null)
			status_series = add_series(status_name, null);
		status_series.add(time(), status);
	}

	AltosTimeSeries pressure_series;

	public static final String pressure_name = "Pressure";

	AltosTimeSeries altitude_series;

	public static final String altitude_name = "Altitude";

	AltosTimeSeries height_series;

	public static final String height_name = "Height";

	public  void set_pressure(double pa) {
		if (pressure_series == null)
			pressure_series = add_series(pressure_name, AltosConvert.pressure);
		pressure_series.add(time(), pa);
		if (altitude_series == null)
			altitude_series = add_series(altitude_name, AltosConvert.height);

		double altitude = AltosConvert.pressure_to_altitude(pa);
		altitude_series.add(time(), altitude);
	}

	private void compute_height(double ground_altitude) {
		if (height_series == null) {
			height_series = add_series(height_name, AltosConvert.height);
			for (AltosTimeValue alt : altitude_series)
				height_series.add(alt.time, alt.value - ground_altitude);
		}
	}

	AltosTimeSeries speed_series;

	public static final String speed_name = "Speed";

	private void compute_speed() {
		if (speed_series != null) {
			System.out.printf("speed series already made\n");
			return;
		}

		AltosTimeSeries	alt_speed_series = null;
		AltosTimeSeries accel_speed_series = null;

		if (altitude_series != null) {
			AltosTimeSeries temp_series = make_series(speed_name, AltosConvert.speed);
			altitude_series.differentiate(temp_series);

			alt_speed_series = make_series(speed_name, AltosConvert.speed);
			temp_series.filter(alt_speed_series, 10.0);
		} else {
			System.out.printf("no altitude series\n");
		}
		if (accel_series != null) {
			AltosTimeSeries temp_series = make_series(speed_name, AltosConvert.speed);
			accel_series.integrate(temp_series);

			accel_speed_series = make_series(speed_name, AltosConvert.speed);
			temp_series.filter(accel_speed_series, 0.1);
		} else {
			System.out.printf("no accel series\n");
		}

		if (alt_speed_series != null && accel_speed_series != null) {
			double	apogee_time = AltosLib.MISSING;
			if (state_series != null) {
				for (AltosTimeValue d : state_series) {
					if (d.value >= AltosLib.ao_flight_drogue){
						apogee_time = d.time;
						break;
					}
				}
			}
			if (apogee_time == AltosLib.MISSING) {
				speed_series = alt_speed_series;
			} else {
				speed_series = make_series(speed_name, AltosConvert.speed);
				for (AltosTimeValue d : accel_speed_series) {
					if (d.time <= apogee_time)
						speed_series.add(d);
				}
				for (AltosTimeValue d : alt_speed_series) {
					if (d.time > apogee_time)
						speed_series.add(d);
				}

			}
		} else if (alt_speed_series != null) {
			speed_series = alt_speed_series;
		} else if (accel_speed_series != null) {
			speed_series = accel_speed_series;
		}
		if (speed_series != null) {
			add_series(speed_series);
			System.out.printf("speed series for %s set to %s\n", this.toString(), speed_series.toString());
		} else
			System.out.printf("didn't manage to make speed series\n");
	}

	AltosTimeSeries	kalman_height_series, kalman_speed_series, kalman_accel_series;

	public static final String kalman_height_name = "Kalman Height";
	public static final String kalman_speed_name = "Kalman Speed";
	public static final String kalman_accel_name = "Kalman Accel";

	public void set_kalman(double height, double speed, double acceleration) {
		if (kalman_height_series == null) {
			kalman_height_series = add_series(kalman_height_name, AltosConvert.height);
			kalman_speed_series = add_series(kalman_speed_name, AltosConvert.speed);
			kalman_accel_series = add_series(kalman_accel_name, AltosConvert.accel);
		}
		kalman_height_series.add(time(), height);
		kalman_speed_series.add(time(), speed);
		kalman_accel_series.add(time(), acceleration);
	}

	AltosTimeSeries thrust_series;

	public static final String thrust_name = "Thrust";

	public	void set_thrust(double N) {
		if (thrust_series == null)
			thrust_series = add_series(thrust_name, AltosConvert.force);
		thrust_series.add(time(), N);
	}

	AltosTimeSeries temperature_series;

	public static final String temperature_name = "Temperature";

	public  void set_temperature(double deg_c) {
		if (temperature_series == null)
			temperature_series = add_series(temperature_name, AltosConvert.temperature);
		temperature_series.add(time(), deg_c);
	}

	AltosTimeSeries battery_voltage_series;

	public static final String battery_voltage_name = "Battery Voltage";

	public void set_battery_voltage(double volts) {
		if (volts == AltosLib.MISSING)
			return;
		if (battery_voltage_series == null)
			battery_voltage_series = add_series(battery_voltage_name, AltosConvert.voltage);
		battery_voltage_series.add(time(), volts);
	}

	AltosTimeSeries apogee_voltage_series;

	public static final String apogee_voltage_name = "Apogee Voltage";

	public void set_apogee_voltage(double volts) {
		if (volts == AltosLib.MISSING)
			return;
		if (apogee_voltage_series == null)
			apogee_voltage_series = add_series(apogee_voltage_name, AltosConvert.voltage);
		apogee_voltage_series.add(time(), volts);
	}

	AltosTimeSeries main_voltage_series;

	public static final String main_voltage_name = "Main Voltage";

	public void set_main_voltage(double volts) {
		if (volts == AltosLib.MISSING)
			return;
		if (main_voltage_series == null)
			main_voltage_series = add_series(main_voltage_name, AltosConvert.voltage);
		main_voltage_series.add(time(), volts);
	}

	AltosTimeSeries	sats_in_view;
	AltosTimeSeries sats_in_soln;
	AltosTimeSeries gps_altitude;
	AltosTimeSeries gps_height;
	AltosTimeSeries gps_ground_speed;
	AltosTimeSeries gps_ascent_rate;
	AltosTimeSeries gps_course;
	AltosTimeSeries gps_speed;

	public ArrayList<AltosGPSTimeValue> gps_series;

	public static final String sats_in_view_name = "Satellites in view";
	public static final String sats_in_soln_name = "Satellites in solution";
	public static final String gps_altitude_name = "GPS Altitude";
	public static final String gps_height_name = "GPS Height";
	public static final String gps_ground_speed_name = "GPS Ground Speed";
	public static final String gps_ascent_rate_name = "GPS Ascent Rate";
	public static final String gps_course_name = "GPS Course";
	public static final String gps_speed_name = "GPS Speed";

	public void set_gps(AltosGPS gps) {
		if (gps_series == null)
			gps_series = new ArrayList<AltosGPSTimeValue>();
		gps_series.add(new AltosGPSTimeValue(time(), gps));

		if (sats_in_view == null) {
			sats_in_view = add_series(sats_in_view_name, null);
			sats_in_soln = add_series(sats_in_soln_name, null);
			gps_altitude = add_series(gps_altitude_name, AltosConvert.height);
			gps_height = add_series(gps_height_name, AltosConvert.height);
			gps_ground_speed = add_series(gps_ground_speed_name, AltosConvert.speed);
			gps_ascent_rate = add_series(gps_ascent_rate_name, AltosConvert.speed);
			gps_course = add_series(gps_course_name, null);
			gps_speed = add_series(gps_speed_name, null);
		}
		if (gps.cc_gps_sat != null)
			sats_in_view.add(time(), gps.cc_gps_sat.length);
		if (gps.locked) {
			sats_in_soln.add(time(), gps.nsat);
			if (gps.alt != AltosLib.MISSING) {
				gps_altitude.add(time(), gps.alt);
				if (cal_data.gps_ground_altitude != AltosLib.MISSING)
					gps_height.add(time(), gps.alt - cal_data.gps_ground_altitude);
			}
			if (gps.ground_speed != AltosLib.MISSING)
				gps_ground_speed.add(time(), gps.ground_speed);
			if (gps.climb_rate != AltosLib.MISSING)
				gps_ascent_rate.add(time(), gps.climb_rate);
			if (gps.course != AltosLib.MISSING)
				gps_course.add(time(), gps.course);
			if (gps.ground_speed != AltosLib.MISSING && gps.climb_rate != AltosLib.MISSING)
				gps_speed.add(time(), Math.sqrt(gps.ground_speed * gps.ground_speed +
								gps.climb_rate * gps.climb_rate));
		}
	}

	public static final String accel_across_name = "Accel Across";
	public static final String accel_along_name = "Accel Along";
	public static final String accel_through_name = "Accel Through";

	public  void set_accel(double along, double across, double through) {
	}

	public  void set_accel_ground(double along, double across, double through) {
	}

	public  void set_gyro(double roll, double pitch, double yaw) {
	}

	public  void set_mag(double along, double across, double through) {
	}

	public void set_orient(double new_orient) { }

	public  void set_pyro_voltage(double volts) {
	}

	public  void set_ignitor_voltage(double[] voltage) {
	}

	public  void set_pyro_fired(int pyro_mask) {
	}

	public void set_companion(AltosCompanion companion) {
	}

	public void fill_in() {
		System.out.printf("fill in %s\n", this.toString());
		compute_speed();
		compute_accel();
		if (cal_data.ground_altitude != AltosLib.MISSING)
			compute_height(cal_data.ground_altitude);
	}

	public void init() {
		time = AltosLib.MISSING;
		series = new ArrayList<AltosTimeSeries>();
	}

	public AltosTimeSeries[] series() {
		fill_in();
		return series.toArray(new AltosTimeSeries[0]);
	}

	public AltosFlightSeries(AltosCalData cal_data) {
		super(cal_data);
		System.out.printf("new flight series %s\n", this.toString());
		init();
	}
}
