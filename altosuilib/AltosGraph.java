/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altosuilib_12;

import java.io.*;
import java.util.ArrayList;

import java.awt.*;
import javax.swing.*;
import org.altusmetrum.altoslib_12.*;

import org.jfree.ui.*;
import org.jfree.chart.*;
import org.jfree.chart.plot.*;
import org.jfree.chart.axis.*;
import org.jfree.chart.renderer.*;
import org.jfree.chart.renderer.xy.*;
import org.jfree.chart.labels.*;
import org.jfree.data.xy.*;
import org.jfree.data.*;

public class AltosGraph extends AltosUIGraph {

	static final private Color height_color = new Color(194,31,31);
	static final private Color kalman_height_color = new Color(255,0,0);
	static final private Color gps_height_color = new Color(150,31,31);
	static final private Color pressure_color = new Color (225,31,31);
	static final private Color range_color = new Color(100, 31, 31);
	static final private Color distance_color = new Color(100, 31, 194);
	static final private Color speed_color = new Color(31,194,31);
	static final private Color kalman_speed_color = new Color(0,255,0);
	static final private Color thrust_color = new Color(31,194,31);
	static final private Color accel_color = new Color(31,31,194);
	static final private Color vert_accel_color = new Color(64,164,164);
	static final private Color kalman_accel_color = new Color(0,0,255);
	static final private Color voltage_color = new Color(194, 194, 31);
	static final private Color battery_voltage_color = new Color(194, 194, 31);
	static final private Color drogue_voltage_color = new Color(150, 150, 31);
	static final private Color main_voltage_color = new Color(100, 100, 31);
	static final private Color igniter_voltage_color = new Color(80, 80, 31);
	static final private Color igniter_marker_color = new Color(255, 0, 0);
	static final private Color gps_nsat_color = new Color (194, 31, 194);
	static final private Color gps_nsat_solution_color = new Color (194, 31, 194);
	static final private Color gps_nsat_view_color = new Color (150, 31, 150);
	static final private Color gps_course_color = new Color (100, 31, 112);
	static final private Color gps_ground_speed_color = new Color (31, 112, 100);
	static final private Color gps_speed_color = new Color (31, 112, 100);
	static final private Color gps_climb_rate_color = new Color (31, 31, 112);
	static final private Color gps_pdop_color = new Color(50, 194, 0);
	static final private Color gps_hdop_color = new Color(50, 0, 194);
	static final private Color gps_vdop_color = new Color(194, 0, 50);
	static final private Color temperature_color = new Color (31, 194, 194);
	static final private Color dbm_color = new Color(31, 100, 100);
	static final private Color state_color = new Color(0,0,0);
	static final private Color accel_along_color = new Color(255, 0, 0);
	static final private Color accel_across_color = new Color(0, 255, 0);
	static final private Color accel_through_color = new Color(0, 0, 255);
	static final private Color gyro_roll_color = new Color(192, 0, 0);
	static final private Color gyro_pitch_color = new Color(0, 192, 0);
	static final private Color gyro_yaw_color = new Color(0, 0, 192);
	static final private Color mag_along_color = new Color(128, 0, 0);
	static final private Color mag_across_color = new Color(0, 128, 0);
	static final private Color mag_through_color = new Color(0, 0, 128);
	static final private Color orient_color = new Color(31, 31, 31);

	static AltosUnits dop_units = null;
	static AltosUnits tick_units = null;

	AltosUIFlightSeries flight_series;

	AltosUITimeSeries[] setup(AltosFlightStats stats, AltosUIFlightSeries flight_series) {
		AltosCalData	cal_data = flight_series.cal_data();

		AltosUIAxis	height_axis, speed_axis, accel_axis, voltage_axis, temperature_axis, nsat_axis, dbm_axis;
		AltosUIAxis	distance_axis, pressure_axis, thrust_axis;
		AltosUIAxis	gyro_axis, orient_axis, mag_axis;
		AltosUIAxis	course_axis, dop_axis, tick_axis;

		if (stats.serial != AltosLib.MISSING && stats.product != null && stats.flight != AltosLib.MISSING)
			setName(String.format("%s %d flight %d\n", stats.product, stats.serial, stats.flight));

		height_axis = newAxis("Height", AltosConvert.height, height_color);
		pressure_axis = newAxis("Pressure", AltosConvert.pressure, pressure_color, 0);
		speed_axis = newAxis("Speed", AltosConvert.speed, speed_color);
		thrust_axis = newAxis("Thrust", AltosConvert.force, thrust_color);
		tick_axis = newAxis("Tick", tick_units, accel_color, 0);
		accel_axis = newAxis("Acceleration", AltosConvert.accel, accel_color);
		voltage_axis = newAxis("Voltage", AltosConvert.voltage, voltage_color);
		temperature_axis = newAxis("Temperature", AltosConvert.temperature, temperature_color, 0);
		nsat_axis = newAxis("Satellites", null, gps_nsat_color,
				    AltosUIAxis.axis_include_zero | AltosUIAxis.axis_integer);
		dbm_axis = newAxis("Signal Strength", null, dbm_color, 0);
		distance_axis = newAxis("Distance", AltosConvert.distance, range_color);

		gyro_axis = newAxis("Rotation Rate", AltosConvert.rotation_rate, gyro_roll_color, 0);
		orient_axis = newAxis("Tilt Angle", AltosConvert.orient, orient_color, 0);
		mag_axis = newAxis("Magnetic Field", AltosConvert.magnetic_field, mag_along_color, 0);
		course_axis = newAxis("Course", AltosConvert.orient, gps_course_color, 0);
		dop_axis = newAxis("Dilution of Precision", dop_units, gps_pdop_color, 0);

		flight_series.register_axis("default",
					    speed_color,
					    false,
					    speed_axis);

		flight_series.register_marker(AltosUIFlightSeries.state_name,
					      state_color,
					      true,
					      plot,
					      true);

		flight_series.register_marker(AltosUIFlightSeries.pyro_fired_name,
					      igniter_marker_color,
					      true,
					      plot,
					      false);

		flight_series.register_axis(AltosUIFlightSeries.tick_name,
					    accel_color,
					    false,
					    tick_axis);

		flight_series.register_axis(AltosUIFlightSeries.accel_name,
					    accel_color,
					    true,
					    accel_axis);

		flight_series.register_axis(AltosUIFlightSeries.vert_accel_name,
					    vert_accel_color,
					    true,
					    accel_axis);

		flight_series.register_axis(AltosUIFlightSeries.kalman_accel_name,
					    kalman_accel_color,
					    false,
					    accel_axis);

		flight_series.register_axis(AltosUIFlightSeries.rssi_name,
					    dbm_color,
					    false,
					    dbm_axis);

		flight_series.register_axis(AltosUIFlightSeries.speed_name,
					    speed_color,
					    true,
					    speed_axis);

		flight_series.register_axis(AltosUIFlightSeries.kalman_speed_name,
					    kalman_speed_color,
					    true,
					    speed_axis);

		flight_series.register_axis(AltosUIFlightSeries.pressure_name,
					    pressure_color,
					    false,
					    pressure_axis);

		flight_series.register_axis(AltosUIFlightSeries.height_name,
					    height_color,
					    true,
					    height_axis);

		flight_series.register_axis(AltosUIFlightSeries.altitude_name,
					    height_color,
					    false,
					    height_axis);

		flight_series.register_axis(AltosUIFlightSeries.kalman_height_name,
					    kalman_height_color,
					    false,
					    height_axis);


		flight_series.register_axis(AltosUIFlightSeries.temperature_name,
					    temperature_color,
					    false,
					    temperature_axis);

		flight_series.register_axis(AltosUIFlightSeries.battery_voltage_name,
					    battery_voltage_color,
					    false,
					    voltage_axis);

		flight_series.register_axis(AltosUIFlightSeries.apogee_voltage_name,
					    drogue_voltage_color,
					    false,
					    voltage_axis);

		flight_series.register_axis(AltosUIFlightSeries.main_voltage_name,
					    main_voltage_color,
					    false,
					    voltage_axis);

		flight_series.register_axis(AltosUIFlightSeries.sats_in_view_name,
					    gps_nsat_view_color,
					    false,
					    nsat_axis);

		flight_series.register_axis(AltosUIFlightSeries.sats_in_soln_name,
					    gps_nsat_solution_color,
					    false,
					    nsat_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_pdop_name,
					    gps_pdop_color,
					    false,
					    dop_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_hdop_name,
					    gps_hdop_color,
					    false,
					    dop_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_vdop_name,
					    gps_vdop_color,
					    false,
					    dop_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_altitude_name,
					    gps_height_color,
					    false,
					    height_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_height_name,
					    gps_height_color,
					    false,
					    height_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_ground_speed_name,
					    gps_ground_speed_color,
					    false,
					    speed_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_ascent_rate_name,
					    gps_climb_rate_color,
					    false,
					    speed_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_course_name,
					    gps_course_color,
					    false,
					    course_axis);

		flight_series.register_axis(AltosUIFlightSeries.gps_speed_name,
					    gps_speed_color,
					    false,
					    speed_axis);

		flight_series.register_axis(AltosUIFlightSeries.accel_along_name,
					    accel_along_color,
					    false,
					    accel_axis);

		flight_series.register_axis(AltosUIFlightSeries.accel_across_name,
					    accel_across_color,
					    false,
					    accel_axis);

		flight_series.register_axis(AltosUIFlightSeries.accel_through_name,
					    accel_through_color,
					    false,
					    accel_axis);

		flight_series.register_axis(AltosUIFlightSeries.gyro_roll_name,
					    gyro_roll_color,
					    false,
					    gyro_axis);

		flight_series.register_axis(AltosUIFlightSeries.gyro_pitch_name,
					    gyro_pitch_color,
					    false,
					    gyro_axis);

		flight_series.register_axis(AltosUIFlightSeries.gyro_yaw_name,
					    gyro_yaw_color,
					    false,
					    gyro_axis);

		flight_series.register_axis(AltosUIFlightSeries.mag_along_name,
					    mag_along_color,
					    false,
					    mag_axis);

		flight_series.register_axis(AltosUIFlightSeries.mag_across_name,
					    mag_across_color,
					    false,
					    mag_axis);

		flight_series.register_axis(AltosUIFlightSeries.mag_through_name,
					    mag_through_color,
					    false,
					    mag_axis);

		flight_series.register_axis(AltosUIFlightSeries.orient_name,
					    orient_color,
					    false,
					    orient_axis);

		for (int channel = 0; channel < 26; channel++) {
			flight_series.register_axis(flight_series.igniter_voltage_name(channel),
						    igniter_voltage_color,
						    false,
						    voltage_axis);
		}

		flight_series.register_axis(AltosUIFlightSeries.thrust_name,
					    thrust_color,
					    true,
					    thrust_axis);

		return flight_series.series(cal_data);
	}

	public void set_filter(double filter) {
		System.out.printf("filter set to %f\n", filter);
		flight_series.set_filter(filter, filter);
		units_changed(false);
	}

	public void set_data(AltosFlightStats stats, AltosUIFlightSeries flight_series) {
		set_series(setup(stats, flight_series));
	}

	public AltosGraph(AltosUIEnable enable) {
		super(enable, "Flight");
	}

	public AltosGraph(AltosUIEnable enable, AltosFlightStats stats, AltosUIFlightSeries flight_series) {
		this(enable);
		this.flight_series = flight_series;
		set_series(setup(stats, flight_series));
	}
}
