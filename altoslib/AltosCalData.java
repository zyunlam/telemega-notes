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

/*
 * Calibration and other data needed to construct 'real' values from various data
 * sources.
 */

public class AltosCalData {
	public int		flight = AltosLib.MISSING;

	public void set_flight(int flight) {
		if (flight != AltosLib.MISSING)
			this.flight = flight;
	}

	public String		callsign = null;

	public void set_callsign(String callsign) {
		if (callsign != null)
			this.callsign = callsign;
	}

	public String		firmware_version = null;

	public void set_firmware_version(String firmware_version) {
		if (firmware_version != null)
			this.firmware_version = firmware_version;
	}

	public String		product = null;

	public void set_product(String product) {
		if (product != null)
			this.product = product;
	}

	public int		serial = AltosLib.MISSING;

	public void set_serial(int serial) {
		if (serial != AltosLib.MISSING)
			this.serial = serial;
	}

	public int		receiver_serial = AltosLib.MISSING;

	public void set_receiver_serial(int receiver_serial) {
		if (receiver_serial != AltosLib.MISSING)
			this.receiver_serial = receiver_serial;
	}

	public int		device_type = AltosLib.MISSING;

	public void set_device_type(int device_type) {
		if (device_type != AltosLib.MISSING)
			this.device_type = device_type;
	}

	public int		config_major = AltosLib.MISSING;
	public int		config_minor = AltosLib.MISSING;
	public int		flight_log_max = AltosLib.MISSING;

	public void set_config(int major, int minor, int log_max) {
		if (major != AltosLib.MISSING)
			config_major = major;
		if (minor != AltosLib.MISSING)
			config_minor = minor;
		if (log_max != AltosLib.MISSING)
			flight_log_max = log_max;
	}

	public double		apogee_delay = AltosLib.MISSING;
	public double		main_deploy = AltosLib.MISSING;

	public void set_flight_params(double apogee_delay, double main_deploy) {
		if (apogee_delay != AltosLib.MISSING)
			this.apogee_delay = apogee_delay;
		if (main_deploy != AltosLib.MISSING)
			this.main_deploy = main_deploy;
	}

	public double		accel_plus_g = AltosLib.MISSING;
	public double		accel_minus_g = AltosLib.MISSING;
	public double		ground_accel = AltosLib.MISSING;

	public void set_accel_plus_minus(double plus, double minus) {
		if (plus != AltosLib.MISSING && minus != AltosLib.MISSING) {
			accel_plus_g = plus;
			accel_minus_g = minus;
		}
	}

	public void set_ground_accel(double ground_accel) {
		if (ground_accel != AltosLib.MISSING)
			this.ground_accel = ground_accel;
	}

	/* Raw acceleration value */
	public double		accel = AltosLib.MISSING;

	public void set_accel(double accel) {
		this.accel = accel;
	}

	public boolean mma655x_inverted = false;

	public void set_mma655x_inverted(boolean inverted) {
		mma655x_inverted = inverted;
	}

	public int pad_orientation = AltosLib.MISSING;

	public void set_pad_orientation(int orientation) {
		if (orientation != AltosLib.MISSING)
			pad_orientation = orientation;
	}

	/* Compute acceleration */
	public double acceleration(double sensor) {
		return AltosConvert.acceleration_from_sensor(sensor, accel_plus_g, accel_minus_g, ground_accel);
	}

	public AltosMs5607	ms5607 = null;

	public void set_ms5607(AltosMs5607 ms5607) {
		this.ms5607 = ms5607;
	}

	public double		ground_pressure = AltosLib.MISSING;
	public double		ground_altitude = AltosLib.MISSING;

	public void set_ground_pressure(double ground_pressure) {
		if (ground_pressure != AltosLib.MISSING) {
			this.ground_pressure = ground_pressure;
			this.ground_altitude = AltosConvert.pressure_to_altitude(ground_pressure);
		}
	}

	public void set_ground_altitude(double ground_altitude) {
		if (ground_altitude != AltosLib.MISSING)
			this.ground_altitude = ground_altitude;
	}

	/* Compute pressure */

	public AltosPresTemp pressure_ms5607(int raw_pres, int raw_temp) {
		if (ms5607 == null)
			return new AltosPresTemp(AltosLib.MISSING, AltosLib.MISSING);
		return ms5607.pres_temp(raw_pres, raw_temp);
	}

	public int		tick = AltosLib.MISSING;
	private int		prev_tick = AltosLib.MISSING;

	public void set_tick(int tick) {
		if (tick != AltosLib.MISSING) {
			if (prev_tick != AltosLib.MISSING) {
				while (tick < prev_tick - 1000) {
					tick += 65536;
				}
			}
			prev_tick = tick;
			this.tick = tick;
		}
	}

	public int		boost_tick = AltosLib.MISSING;

	public void set_boost_tick() {
		boost_tick = tick;
	}

	public double		ticks_per_sec = 100.0;

	public void set_ticks_per_sec(double ticks_per_sec) {
		this.ticks_per_sec = ticks_per_sec;
	}

	public double time() {
		if (tick == AltosLib.MISSING)
			return AltosLib.MISSING;
		if (boost_tick == AltosLib.MISSING)
			return AltosLib.MISSING;
		return (tick - boost_tick) / ticks_per_sec;
	}

	public double boost_time() {
		if (boost_tick == AltosLib.MISSING)
			return AltosLib.MISSING;
		return boost_tick / ticks_per_sec;
	}

	public int		state = AltosLib.MISSING;

	public void set_state(int state) {
		if (state >= AltosLib.ao_flight_boost && boost_tick == AltosLib.MISSING)
			set_boost_tick();
		this.state = state;
	}

	public AltosGPS		gps_pad = null;

	public double		gps_pad_altitude = AltosLib.MISSING;

	public void set_gps(AltosGPS gps) {
		if ((state != AltosLib.MISSING && state < AltosLib.ao_flight_boost) || gps_pad == null)
			gps_pad = gps;
		if (gps_pad_altitude == AltosLib.MISSING && gps.alt != AltosLib.MISSING)
			gps_pad_altitude = gps.alt;
	}

	/*
	 * While receiving GPS data, we construct a temporary GPS state
	 * object and then deliver the result atomically to the listener
	 */
	AltosGPS		temp_gps = null;
	AltosGPS		prev_gps = null;
	int			temp_gps_sat_tick = AltosLib.MISSING;

	public AltosGPS temp_gps() {
		return temp_gps;
	}

	public void reset_temp_gps() {
		if (temp_gps != null) {
			if (temp_gps.locked && temp_gps.nsat >= 4)
				set_gps(temp_gps);
			prev_gps = temp_gps;
			temp_gps = null;
		}
	}

	public boolean gps_pending() {
		return temp_gps != null;
	}

	public AltosGPS make_temp_gps(int tick, boolean sats) {
		if (temp_gps == null) {
			if (prev_gps != null)
				temp_gps = prev_gps.clone();
			else
				temp_gps = new AltosGPS();
		}
		if (sats) {
			if (tick != temp_gps_sat_tick)
				temp_gps.cc_gps_sat = null;
			temp_gps_sat_tick = tick;
		}
		return temp_gps;
	}

	public double	accel_zero_along, accel_zero_across, accel_zero_through;

	public void set_accel_zero(double zero_along, double zero_across, double zero_through) {
		if (zero_along != AltosLib.MISSING) {
			accel_zero_along = zero_along;
			accel_zero_across = zero_across;
			accel_zero_through = zero_through;
		}
	}

	public double accel_along(double counts) {
		return AltosIMU.convert_accel(counts - accel_zero_along);
	}

	public double accel_across(double counts) {
		return AltosIMU.convert_accel(counts - accel_zero_across);
	}

	public double accel_through(double counts) {
		return AltosIMU.convert_accel(counts - accel_zero_through);
	}

	public double	gyro_zero_roll,	gyro_zero_pitch, gyro_zero_yaw;

	public void set_gyro_zero(double roll, double pitch, double yaw) {
		if (roll != AltosLib.MISSING) {
			gyro_zero_roll = roll;
			gyro_zero_pitch = pitch;
			gyro_zero_yaw = yaw;
		}
	}

	public double gyro_roll(double counts) {
		if (gyro_zero_roll == AltosLib.MISSING || counts == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosIMU.convert_gyro(counts - gyro_zero_roll);
	}

	public double gyro_pitch(double counts) {
		if (gyro_zero_pitch == AltosLib.MISSING || counts == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosIMU.convert_gyro(counts - gyro_zero_pitch);
	}

	public double gyro_yaw(double counts) {
		if (gyro_zero_yaw == AltosLib.MISSING || counts == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosIMU.convert_gyro(counts - gyro_zero_yaw);
	}

	private double gyro_zero_overflow(double first) {
		double v = first / 128.0;
		if (v < 0)
			v = Math.ceil(v);
		else
			v = Math.floor(v);
		return v * 128.0;
	}

	public void check_imu_wrap(double roll, double pitch, double yaw) {
		gyro_zero_roll += gyro_zero_overflow(roll);
		gyro_zero_pitch += gyro_zero_overflow(pitch);
		gyro_zero_yaw += gyro_zero_overflow(yaw);
	}

	public double mag_along(double along) {
		if (along == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosMag.convert_gauss(along);
	}

	public double mag_across(double across) {
		if (across == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosMag.convert_gauss(across);
	}

	public double mag_through(double through) {
		if (through == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosMag.convert_gauss(through);
	}

	public AltosCalData() {
	}

	public AltosCalData(AltosConfigData config_data) {
		set_serial(config_data.serial);
		set_ticks_per_sec(100.0);
		set_flight(config_data.flight);
		set_callsign(config_data.callsign);
		set_config(config_data.config_major, config_data.config_minor, config_data.flight_log_max);
		set_firmware_version(config_data.version);
		set_flight_params(config_data.apogee_delay / ticks_per_sec, config_data.apogee_lockout / ticks_per_sec);
		set_pad_orientation(config_data.pad_orientation);
		set_product(config_data.product);
		set_accel_plus_minus(config_data.accel_cal_plus, config_data.accel_cal_minus);
		set_accel_zero(config_data.accel_zero_along, config_data.accel_zero_across, config_data.accel_zero_through);
		set_ms5607(config_data.ms5607);
		try {
			set_mma655x_inverted(config_data.mma655x_inverted());
		} catch (AltosUnknownProduct up) {
		}
		set_pad_orientation(config_data.pad_orientation);
	}
}
