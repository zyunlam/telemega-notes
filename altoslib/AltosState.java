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

/*
 * Track flight state from telemetry or eeprom data stream
 */

package org.altusmetrum.altoslib_11;

import java.io.*;

public class AltosState implements Cloneable, AltosJsonable {

	public static final int set_position = 1;
	public static final int set_gps = 2;
	public static final int set_data = 4;

	public int set;

	static final double filter_len = 2.0;
	static final double ascent_filter_len = 0.5;
	static final double descent_filter_len = 5.0;

	/* derived data */

	public long	received_time;

	public double	time;
	public double	prev_time;
	public double	time_change;
	public int	tick;
	private int	prev_tick;
	public int	boost_tick;

	class AltosValue implements AltosJsonable {
		double	value;
		double	prev_value;
		private double	max_value;
		private double	set_time;
		private double	prev_set_time;

		boolean can_max() { return true; }

		void set(double new_value, double time) {
			if (new_value != AltosLib.MISSING) {
				value = new_value;
				if (can_max() && (max_value == AltosLib.MISSING || value > max_value))
					max_value = value;
				set_time = time;
			}
		}

		void set_filtered(double new_value, double time) {
			if (prev_value != AltosLib.MISSING) {
				double f = 1/Math.exp((time - prev_set_time) / filter_len);
				new_value = f * new_value + (1-f) * prev_value;
			}
			set(new_value, time);
		}

		double value() {
			return value;
		}

		double max() {
			return max_value;
		}

		double prev() {
			return prev_value;
		}

		double change() {
			if (value != AltosLib.MISSING && prev_value != AltosLib.MISSING)
				return value - prev_value;
			return AltosLib.MISSING;
		}

		double rate() {
			double c = change();
			double t = set_time - prev_set_time;

			if (c != AltosLib.MISSING && t != 0)
				return c / t;
			return AltosLib.MISSING;
		}

		double integrate() {
			if (value == AltosLib.MISSING)
				return AltosLib.MISSING;
			if (prev_value == AltosLib.MISSING)
				return AltosLib.MISSING;

			return (value + prev_value) / 2 * (set_time - prev_set_time);
		}

		double time() {
			return set_time;
		}

		void set_derivative(AltosValue in) {
			double	n = in.rate();

			if (n == AltosLib.MISSING)
				return;

			double	p = prev_value;
			double	pt = prev_set_time;

			if (p == AltosLib.MISSING) {
				p = 0;
				pt = in.time() - 0.01;
			}

			/* Clip changes to reduce noise */
			double	ddt = in.time() - pt;
			double	ddv = (n - p) / ddt;

			final double max = 100000;

			/* 100gs */
			if (Math.abs(ddv) > max) {
				if (n > p)
					n = p + ddt * max;
				else
					n = p - ddt * max;
			}

			double filter_len;

			if (ascent)
				filter_len = ascent_filter_len;
			else
				filter_len = descent_filter_len;

			double f = 1/Math.exp(ddt/ filter_len);
			n = p * f + n * (1-f);

			set(n, in.time());
		}

		void set_integral(AltosValue in) {
			double	change = in.integrate();

			if (change != AltosLib.MISSING) {
				double	prev = prev_value;
				if (prev == AltosLib.MISSING)
					prev = 0;
				set(prev + change, in.time());
			}
		}

		void copy(AltosValue old) {
			value = old.value;
			set_time = old.set_time;
			prev_value = old.value;
			prev_set_time = old.set_time;
			max_value = old.max_value;
		}

		void finish_update() {
			prev_value = value;
			prev_set_time = set_time;
		}

		public AltosJson json() {
			AltosJson j = new AltosJson();

			j.put("value", value);
			j.put("prev_value", prev_value);
			j.put("max_value", max_value);
			j.put("set_time", set_time);
			j.put("prev_set_time", prev_set_time);
			return j;
		}

		AltosValue(AltosJson j) {
			this();
			if (j != null) {
				value = j.get_double("value", value);
				prev_value = j.get_double("prev_value", prev_value);
				max_value = j.get_double("max_value", max_value);
				set_time = j.get_double("set_time", 0);
				prev_set_time = j.get_double("prev_set_time", 0);
			}
		}

		AltosValue() {
			value = AltosLib.MISSING;
			prev_value = AltosLib.MISSING;
			max_value = AltosLib.MISSING;
		}

	}

	AltosValue AltosValue_fromJson(AltosJson j, AltosValue def) {
		if (j == null)
			return def;
		return new AltosValue(j);
	}

	class AltosCValue implements AltosJsonable {

		class AltosIValue extends AltosValue implements AltosJsonable {
			boolean can_max() {
				return c_can_max();
			}

			AltosIValue() {
				super();
			}

			AltosIValue(AltosJson j) {
				super(j);
			}
		};

		public AltosIValue	measured;
		public AltosIValue	computed;

		boolean can_max() { return true; }

		boolean c_can_max() { return can_max(); }

		double value() {
			double v = measured.value();
			if (v != AltosLib.MISSING)
				return v;
			return computed.value();
		}

		boolean is_measured() {
			return measured.value() != AltosLib.MISSING;
		}

		double max() {
			double m = measured.max();

			if (m != AltosLib.MISSING)
				return m;
			return computed.max();
		}

		double prev_value() {
			if (measured.value != AltosLib.MISSING && measured.prev_value != AltosLib.MISSING)
				return measured.prev_value;
			return computed.prev_value;
		}

		AltosValue altos_value() {
			if (measured.value() != AltosLib.MISSING)
				return measured;
			return computed;
		}

		double change() {
			double c = measured.change();
			if (c == AltosLib.MISSING)
				c = computed.change();
			return c;
		}

		double rate() {
			double r = measured.rate();
			if (r == AltosLib.MISSING)
				r = computed.rate();
			return r;
		}

		void set_measured(double new_value, double time) {
			measured.set(new_value, time);
		}

		void set_computed(double new_value, double time) {
			computed.set(new_value, time);
		}

		void set_derivative(AltosValue in) {
			computed.set_derivative(in);
		}

		void set_derivative(AltosCValue in) {
			set_derivative(in.altos_value());
		}

		void set_integral(AltosValue in) {
			computed.set_integral(in);
		}

		void set_integral(AltosCValue in) {
			set_integral(in.altos_value());
		}

		void copy(AltosCValue old) {
			measured.copy(old.measured);
			computed.copy(old.computed);
		}

		void finish_update() {
			measured.finish_update();
			computed.finish_update();
		}

		AltosCValue() {
			measured = new AltosIValue();
			computed = new AltosIValue();
		}

		public AltosJson json() {
			AltosJson	j = new AltosJson();

			j.put("measured", measured.json());
			j.put("computed", computed.json());
			return j;
		}

		AltosCValue(AltosJson j) {
			measured = new AltosIValue(j.get("measured"));
			computed = new AltosIValue(j.get("computed"));
		}
	}

	AltosCValue AltosCValue_fromJson(AltosJson j, AltosCValue def) {
		if (j == null)
			return def;
		return new AltosCValue(j);
	}

	private int	state;
	public int	flight;
	public int	serial;
	public int	altitude_32;
	public int	receiver_serial;
	public boolean	landed;
	public boolean	ascent;	/* going up? */
	public boolean	boost;	/* under power */
	public int	rssi;
	public int	status;
	public int	device_type;
	public int	config_major;
	public int	config_minor;
	public int	apogee_delay;
	public int	main_deploy;
	public int	flight_log_max;

	private double pressure_to_altitude(double p) {
		if (p == AltosLib.MISSING)
			return AltosLib.MISSING;
		return AltosConvert.pressure_to_altitude(p);
	}

	private AltosCValue ground_altitude;

	public double ground_altitude() {
		return ground_altitude.value();
	}

	public void set_ground_altitude(double a) {
		ground_altitude.set_measured(a, time);
	}

	class AltosGpsGroundAltitude extends AltosValue {
		void set(double a, double t) {
			super.set(a, t);
			pad_alt = value();
			gps_altitude.set_gps_height();
		}

		void set_filtered(double a, double t) {
			super.set_filtered(a, t);
			pad_alt = value();
			gps_altitude.set_gps_height();
		}

		AltosGpsGroundAltitude() {
			super();
		}

		AltosGpsGroundAltitude (AltosJson j) {
			super(j);
		}
	}

	AltosGpsGroundAltitude AltosGpsGroundAltitude_fromJson(AltosJson j, AltosGpsGroundAltitude def) {
		if (j == null) return def;
		return new AltosGpsGroundAltitude(j);
	}

	private AltosGpsGroundAltitude gps_ground_altitude;

	public double gps_ground_altitude() {
		return gps_ground_altitude.value();
	}

	public void set_gps_ground_altitude(double a) {
		gps_ground_altitude.set(a, time);
	}

	class AltosGroundPressure extends AltosCValue {
		void set_filtered(double p, double time) {
			computed.set_filtered(p, time);
			if (!is_measured())
				ground_altitude.set_computed(pressure_to_altitude(computed.value()), time);
		}

		void set_measured(double p, double time) {
			super.set_measured(p, time);
			ground_altitude.set_computed(pressure_to_altitude(p), time);
		}

		AltosGroundPressure () {
			super();
		}

		AltosGroundPressure (AltosJson j) {
			super(j);
		}
	}

	AltosGroundPressure AltosGroundPressure_fromJson(AltosJson j, AltosGroundPressure def) {
		if (j == null) return def;
		return new AltosGroundPressure(j);
	}

	private AltosGroundPressure ground_pressure;

	public double ground_pressure() {
		return ground_pressure.value();
	}

	public void set_ground_pressure (double pressure) {
		ground_pressure.set_measured(pressure, time);
	}

	class AltosAltitude extends AltosCValue {

		private void set_speed(AltosValue v) {
			if (!acceleration.is_measured() || !ascent)
				speed.set_derivative(this);
		}

		void set_computed(double a, double time) {
			super.set_computed(a,time);
			set_speed(computed);
			set |= set_position;
		}

		void set_measured(double a, double time) {
			super.set_measured(a,time);
			set_speed(measured);
			set |= set_position;
		}

		AltosAltitude() {
			super();
		}

		AltosAltitude (AltosJson j) {
			super(j);
		}
	}

	AltosAltitude AltosAltitude_fromJson(AltosJson j, AltosAltitude def) {
		if (j == null) return def;
		return new AltosAltitude(j);
	}

	private AltosAltitude	altitude;

	class AltosGpsAltitude extends AltosValue {

		private void set_gps_height() {
			double	a = value();
			double	g = gps_ground_altitude.value();

			if (a != AltosLib.MISSING && g != AltosLib.MISSING)
				gps_height = a - g;
			else
				gps_height = AltosLib.MISSING;
		}

		void set(double a, double t) {
			super.set(a, t);
			set_gps_height();
		}

		AltosGpsAltitude() {
			super();
		}

		AltosGpsAltitude (AltosJson j) {
			super(j);
		}
	}

	AltosGpsAltitude AltosGpsAltitude_fromJson(AltosJson j, AltosGpsAltitude def) {
		if (j == null) return def;
		return new AltosGpsAltitude(j);
	}

	private AltosGpsAltitude	gps_altitude;

	private AltosValue		gps_ground_speed;
	private AltosValue		gps_ascent_rate;
	private AltosValue		gps_course;
	private AltosValue		gps_speed;

	public double altitude() {
		double a = altitude.value();
		if (a != AltosLib.MISSING)
			return a;
		return gps_altitude.value();
	}

	public double max_altitude() {
		double a = altitude.max();
		if (a != AltosLib.MISSING)
			return a;
		return gps_altitude.max();
	}

	public void set_altitude(double new_altitude) {
		altitude.set_measured(new_altitude, time);
	}

	public double gps_altitude() {
		return gps_altitude.value();
	}

	public double max_gps_altitude() {
		return gps_altitude.max();
	}

	public void set_gps_altitude(double new_gps_altitude) {
		gps_altitude.set(new_gps_altitude, time);
	}

	public double gps_ground_speed() {
		return gps_ground_speed.value();
	}

	public double max_gps_ground_speed() {
		return gps_ground_speed.max();
	}

	public double gps_ascent_rate() {
		return gps_ascent_rate.value();
	}

	public double max_gps_ascent_rate() {
		return gps_ascent_rate.max();
	}

	public double gps_course() {
		return gps_course.value();
	}

	public double gps_speed() {
		return gps_speed.value();
	}

	public double max_gps_speed() {
		return gps_speed.max();
	}

	class AltosPressure extends AltosValue {
		void set(double p, double time) {
			super.set(p, time);
			if (state == AltosLib.ao_flight_pad)
				ground_pressure.set_filtered(p, time);
			double a = pressure_to_altitude(p);
			altitude.set_computed(a, time);
		}

		AltosPressure() {
			super();
		}

		AltosPressure (AltosJson j) {
			super(j);
		}
	}

	AltosPressure AltosPressure_fromJson(AltosJson j, AltosPressure def) {
		if (j == null) return def;
		return new AltosPressure(j);
	}

	private AltosPressure	pressure;

	public double pressure() {
		return pressure.value();
	}

	public void set_pressure(double p) {
		pressure.set(p, time);
	}

	public double baro_height() {
		double a = altitude();
		double g = ground_altitude();
		if (a != AltosLib.MISSING && g != AltosLib.MISSING)
			return a - g;
		return AltosLib.MISSING;
	}

	public double height() {
		double k = kalman_height.value();
		if (k != AltosLib.MISSING)
			return k;

		double b = baro_height();
		if (b != AltosLib.MISSING)
			return b;

		return gps_height();
	}

	public double max_height() {
		double	k = kalman_height.max();
		if (k != AltosLib.MISSING)
			return k;

		double a = altitude.max();
		double g = ground_altitude();
		if (a != AltosLib.MISSING && g != AltosLib.MISSING)
			return a - g;
		return max_gps_height();
	}

	public double gps_height() {
		double a = gps_altitude();
		double g = gps_ground_altitude();

		if (a != AltosLib.MISSING && g != AltosLib.MISSING)
			return a - g;
		return AltosLib.MISSING;
	}

	public double max_gps_height() {
		double a = gps_altitude.max();
		double g = gps_ground_altitude();

		if (a != AltosLib.MISSING && g != AltosLib.MISSING)
			return a - g;
		return AltosLib.MISSING;
	}

	class AltosSpeed extends AltosCValue {

		boolean can_max() {
			return state < AltosLib.ao_flight_fast || state == AltosLib.ao_flight_stateless;
		}

		void set_accel() {
			acceleration.set_derivative(this);
		}

		void set_derivative(AltosCValue in) {
			super.set_derivative(in);
			set_accel();
		}

		void set_computed(double new_value, double time) {
			super.set_computed(new_value, time);
			set_accel();
		}

		void set_measured(double new_value, double time) {
			super.set_measured(new_value, time);
			set_accel();
		}

		AltosSpeed() {
			super();
		}

		AltosSpeed (AltosJson j) {
			super(j);
		}
	}

	AltosSpeed AltosSpeed_fromJson(AltosJson j, AltosSpeed def) {
		if (j == null) return def;
		return new AltosSpeed(j);
	}

	private AltosSpeed speed;

	public double speed() {
		double v = kalman_speed.value();
		if (v != AltosLib.MISSING)
			return v;
		v = speed.value();
		if (v != AltosLib.MISSING)
			return v;
		v = gps_speed();
		if (v != AltosLib.MISSING)
			return v;
		return AltosLib.MISSING;
	}

	public double max_speed() {
		double v = kalman_speed.max();
		if (v != AltosLib.MISSING)
			return v;
		v = speed.max();
		if (v != AltosLib.MISSING)
			return v;
		v = max_gps_speed();
		if (v != AltosLib.MISSING)
			return v;
		return AltosLib.MISSING;
	}

	class AltosAccel extends AltosCValue {

		boolean can_max() {
			return state < AltosLib.ao_flight_fast || state == AltosLib.ao_flight_stateless;
		}

		void set_measured(double a, double time) {
			super.set_measured(a, time);
			if (ascent)
				speed.set_integral(this.measured);
		}

		AltosAccel() {
			super();
		}

		AltosAccel (AltosJson j) {
			super(j);
		}
	}

	AltosAccel AltosAccel_fromJson(AltosJson j, AltosAccel def) {
		if (j == null) return def;
		return new AltosAccel(j);
	}

	AltosAccel acceleration;

	public double acceleration() {
		return acceleration.value();
	}

	public double max_acceleration() {
		return acceleration.max();
	}

	public AltosCValue	orient;

	public void set_orient(double new_orient) {
		orient.set_measured(new_orient, time);
	}

	public double orient() {
		return orient.value();
	}

	public double max_orient() {
		return orient.max();
	}

	public AltosValue	kalman_height, kalman_speed, kalman_acceleration;

	public void set_kalman(double height, double speed, double acceleration) {
		kalman_height.set(height, time);
		kalman_speed.set(speed, time);
		kalman_acceleration.set(acceleration, time);
	}

	public double	battery_voltage;
	public double	pyro_voltage;
	public double	temperature;
	public double	apogee_voltage;
	public double	main_voltage;

	public double	ignitor_voltage[];

	public AltosGPS	gps;
	public AltosGPS	temp_gps;
	public int temp_gps_sat_tick;
	public boolean	gps_pending;
	public int gps_sequence;

	public AltosIMU	imu;
	public AltosMag	mag;

	public static final int MIN_PAD_SAMPLES = 10;

	public int	npad;
	public int	gps_waiting;
	public boolean	gps_ready;

	public int	ngps;

	public AltosGreatCircle from_pad;
	public double	elevation;	/* from pad */
	public double	range;		/* total distance */

	public double	gps_height;

	public double pad_lat, pad_lon, pad_alt;

	public int	speak_tick;
	public double	speak_altitude;

	public String	callsign;
	public String	firmware_version;

	public double	accel_plus_g;
	public double	accel_minus_g;
	public double	accel;
	public double	ground_accel;
	public double	ground_accel_avg;

	public int	log_format;
	public int	log_space;
	public String	product;

	public AltosMs5607	baro;

	public AltosCompanion	companion;

	public int	pyro_fired;

	public void set_npad(int npad) {
		this.npad = npad;
		gps_waiting = MIN_PAD_SAMPLES - npad;
		if (this.gps_waiting < 0)
			gps_waiting = 0;
		gps_ready = gps_waiting == 0;
	}

	public void init() {
		set = 0;

		received_time = System.currentTimeMillis();
		time = AltosLib.MISSING;
		time_change = AltosLib.MISSING;
		prev_time = AltosLib.MISSING;
		tick = AltosLib.MISSING;
		prev_tick = AltosLib.MISSING;
		boost_tick = AltosLib.MISSING;
		state = AltosLib.ao_flight_invalid;
		flight = AltosLib.MISSING;
		landed = false;
		boost = false;
		rssi = AltosLib.MISSING;
		status = 0;
		device_type = AltosLib.MISSING;
		config_major = AltosLib.MISSING;
		config_minor = AltosLib.MISSING;
		apogee_delay = AltosLib.MISSING;
		main_deploy = AltosLib.MISSING;
		flight_log_max = AltosLib.MISSING;

		ground_altitude = new AltosCValue();
		ground_pressure = new AltosGroundPressure();
		altitude = new AltosAltitude();
		pressure = new AltosPressure();
		speed = new AltosSpeed();
		acceleration = new AltosAccel();
		orient = new AltosCValue();

		temperature = AltosLib.MISSING;
		battery_voltage = AltosLib.MISSING;
		pyro_voltage = AltosLib.MISSING;
		apogee_voltage = AltosLib.MISSING;
		main_voltage = AltosLib.MISSING;
		ignitor_voltage = null;

		kalman_height = new AltosValue();
		kalman_speed = new AltosValue();
		kalman_acceleration = new AltosValue();

		gps = null;
		temp_gps = null;
		temp_gps_sat_tick = 0;
		gps_sequence = 0;
		gps_pending = false;

		imu = null;
		last_imu_time = AltosLib.MISSING;
		rotation = null;
		ground_rotation = null;

		mag = null;
		accel_zero_along = AltosLib.MISSING;
		accel_zero_across = AltosLib.MISSING;
		accel_zero_through = AltosLib.MISSING;

		accel_ground_along = AltosLib.MISSING;
		accel_ground_across = AltosLib.MISSING;
		accel_ground_through = AltosLib.MISSING;

		pad_orientation = AltosLib.MISSING;

		gyro_zero_roll = AltosLib.MISSING;
		gyro_zero_pitch = AltosLib.MISSING;
		gyro_zero_yaw = AltosLib.MISSING;

		set_npad(0);
		ngps = 0;

		from_pad = null;
		elevation = AltosLib.MISSING;
		range = AltosLib.MISSING;
		gps_height = AltosLib.MISSING;

		pad_lat = AltosLib.MISSING;
		pad_lon = AltosLib.MISSING;
		pad_alt = AltosLib.MISSING;

		gps_altitude = new AltosGpsAltitude();
		gps_ground_altitude = new AltosGpsGroundAltitude();
		gps_ground_speed = new AltosValue();
		gps_speed = new AltosValue();
		gps_ascent_rate = new AltosValue();
		gps_course = new AltosValue();

		speak_tick = AltosLib.MISSING;
		speak_altitude = AltosLib.MISSING;

		callsign = null;
		firmware_version = null;

		accel_plus_g = AltosLib.MISSING;
		accel_minus_g = AltosLib.MISSING;
		accel = AltosLib.MISSING;

		ground_accel = AltosLib.MISSING;
		ground_accel_avg = AltosLib.MISSING;

		log_format = AltosLib.MISSING;
		log_space = AltosLib.MISSING;
		product = null;
		serial = AltosLib.MISSING;
		receiver_serial = AltosLib.MISSING;
		altitude_32 = AltosLib.MISSING;

		baro = null;
		companion = null;

		pyro_fired = 0;
	}

	void finish_update() {
		prev_tick = tick;

		ground_altitude.finish_update();
		altitude.finish_update();
		pressure.finish_update();
		speed.finish_update();
		acceleration.finish_update();
		orient.finish_update();

		kalman_height.finish_update();
		kalman_speed.finish_update();
		kalman_acceleration.finish_update();
	}

	void copy(AltosState old) {

		if (old == null) {
			init();
			return;
		}

		received_time = old.received_time;
		time = old.time;
		time_change = old.time_change;
		prev_time = old.time;

		tick = old.tick;
		prev_tick = old.tick;
		boost_tick = old.boost_tick;

		state = old.state;
		flight = old.flight;
		landed = old.landed;
		ascent = old.ascent;
		boost = old.boost;
		rssi = old.rssi;
		status = old.status;
		device_type = old.device_type;
		config_major = old.config_major;
		config_minor = old.config_minor;
		apogee_delay = old.apogee_delay;
		main_deploy = old.main_deploy;
		flight_log_max = old.flight_log_max;

		set = 0;

		ground_pressure.copy(old.ground_pressure);
		ground_altitude.copy(old.ground_altitude);
		altitude.copy(old.altitude);
		pressure.copy(old.pressure);
		speed.copy(old.speed);
		acceleration.copy(old.acceleration);
		orient.copy(old.orient);

		battery_voltage = old.battery_voltage;
		pyro_voltage = old.pyro_voltage;
		temperature = old.temperature;
		apogee_voltage = old.apogee_voltage;
		main_voltage = old.main_voltage;
		ignitor_voltage = old.ignitor_voltage;

		kalman_height.copy(old.kalman_height);
		kalman_speed.copy(old.kalman_speed);
		kalman_acceleration.copy(old.kalman_acceleration);

		if (old.gps != null)
			gps = old.gps.clone();
		else
			gps = null;
		if (old.temp_gps != null)
			temp_gps = old.temp_gps.clone();
		else
			temp_gps = null;
		temp_gps_sat_tick = old.temp_gps_sat_tick;
		gps_sequence = old.gps_sequence;
		gps_pending = old.gps_pending;

		if (old.imu != null)
			imu = old.imu.clone();
		else
			imu = null;
		last_imu_time = old.last_imu_time;

		if (old.rotation != null)
			rotation = new AltosRotation (old.rotation);

		if (old.ground_rotation != null) {
			ground_rotation = new AltosRotation(old.ground_rotation);
		}

		accel_zero_along = old.accel_zero_along;
		accel_zero_across = old.accel_zero_across;
		accel_zero_through = old.accel_zero_through;

		accel_ground_along = old.accel_ground_along;
		accel_ground_across = old.accel_ground_across;
		accel_ground_through = old.accel_ground_through;
		pad_orientation = old.pad_orientation;

		gyro_zero_roll = old.gyro_zero_roll;
		gyro_zero_pitch = old.gyro_zero_pitch;
		gyro_zero_yaw = old.gyro_zero_yaw;

		if (old.mag != null)
			mag = old.mag.clone();
		else
			mag = null;

		npad = old.npad;
		gps_waiting = old.gps_waiting;
		gps_ready = old.gps_ready;
		ngps = old.ngps;

		if (old.from_pad != null)
			from_pad = old.from_pad.clone();
		else
			from_pad = null;

		elevation = old.elevation;
		range = old.range;

		gps_height = old.gps_height;

		gps_altitude.copy(old.gps_altitude);
		gps_ground_altitude.copy(old.gps_ground_altitude);
		gps_ground_speed.copy(old.gps_ground_speed);
		gps_ascent_rate.copy(old.gps_ascent_rate);
		gps_course.copy(old.gps_course);
		gps_speed.copy(old.gps_speed);

		pad_lat = old.pad_lat;
		pad_lon = old.pad_lon;
		pad_alt = old.pad_alt;

		speak_tick = old.speak_tick;
		speak_altitude = old.speak_altitude;

		callsign = old.callsign;
		firmware_version = old.firmware_version;

		accel_plus_g = old.accel_plus_g;
		accel_minus_g = old.accel_minus_g;
		accel = old.accel;
		ground_accel = old.ground_accel;
		ground_accel_avg = old.ground_accel_avg;

		log_format = old.log_format;
		log_space = old.log_space;
		product = old.product;
		serial = old.serial;
		receiver_serial = old.receiver_serial;
		altitude_32 = old.altitude_32;

		baro = old.baro;
		companion = old.companion;

		pyro_fired = old.pyro_fired;
	}

	void update_time() {
	}

	void update_gps() {
		elevation = AltosLib.MISSING;
		range = AltosLib.MISSING;

		if (gps == null)
			return;

		if (gps.locked && gps.nsat >= 4) {
			/* Track consecutive 'good' gps reports, waiting for 10 of them */
			if (state == AltosLib.ao_flight_pad || state == AltosLib.ao_flight_stateless) {
				set_npad(npad+1);
				if (pad_lat != AltosLib.MISSING && (npad < 10 || state == AltosLib.ao_flight_pad)) {
					pad_lat = (pad_lat * 31 + gps.lat) / 32;
					pad_lon = (pad_lon * 31 + gps.lon) / 32;
					gps_ground_altitude.set_filtered(gps.alt, time);
				}
			}
			if (pad_lat == AltosLib.MISSING) {
				pad_lat = gps.lat;
				pad_lon = gps.lon;
				gps_ground_altitude.set(gps.alt, time);
			}
			gps_altitude.set(gps.alt, time);
			if (gps.climb_rate != AltosLib.MISSING)
				gps_ascent_rate.set(gps.climb_rate, time);
			if (gps.ground_speed != AltosLib.MISSING)
				gps_ground_speed.set(gps.ground_speed, time);
			if (gps.climb_rate != AltosLib.MISSING && gps.ground_speed != AltosLib.MISSING)
				gps_speed.set(Math.sqrt(gps.ground_speed * gps.ground_speed +
							gps.climb_rate * gps.climb_rate), time);
			if (gps.course != AltosLib.MISSING)
				gps_course.set(gps.course, time);
		}
		if (gps.lat != 0 && gps.lon != 0 &&
		    pad_lat != AltosLib.MISSING &&
		    pad_lon != AltosLib.MISSING)
		{
			double h = height();

			if (h == AltosLib.MISSING)
				h = 0;
			from_pad = new AltosGreatCircle(pad_lat, pad_lon, 0, gps.lat, gps.lon, h);
			elevation = from_pad.elevation;
			range = from_pad.range;
		}
	}

	public void set_tick(int new_tick) {
		if (new_tick != AltosLib.MISSING) {
			if (prev_tick != AltosLib.MISSING) {
				while (new_tick < prev_tick - 1000) {
					new_tick += 65536;
				}
			}
			tick = new_tick;
			time = tick / 100.0;
			time_change = time - prev_time;
		}
	}

	public void set_boost_tick(int boost_tick) {
		if (boost_tick != AltosLib.MISSING)
			this.boost_tick = boost_tick;
	}

	public String state_name() {
		return AltosLib.state_name(state);
	}

	public void set_product(String product) {
		this.product = product;
	}

	public void set_state(int state) {
		if (state != AltosLib.ao_flight_invalid) {
			this.state = state;
			ascent = (AltosLib.ao_flight_boost <= state &&
				  state <= AltosLib.ao_flight_coast);
			boost = (AltosLib.ao_flight_boost == state);
		}
	}

	public int state() {
		return state;
	}

	public void set_device_type(int device_type) {
		this.device_type = device_type;
		switch (device_type) {
		case AltosLib.product_telegps:
			this.state = AltosLib.ao_flight_stateless;
			break;
		}
	}

	public void set_log_format(int log_format) {
		this.log_format = log_format;
		switch (log_format) {
		case AltosLib.AO_LOG_FORMAT_TELEGPS:
			this.state = AltosLib.ao_flight_stateless;
			break;
		}
	}

	public void set_log_space(int log_space) {
		this.log_space = log_space;
	}

	public void set_flight_params(int apogee_delay, int main_deploy) {
		this.apogee_delay = apogee_delay;
		this.main_deploy = main_deploy;
	}

	public void set_config(int major, int minor, int flight_log_max) {
		config_major = major;
		config_minor = minor;
		this.flight_log_max = flight_log_max;
	}

	public void set_callsign(String callsign) {
		this.callsign = callsign;
	}

	public void set_firmware_version(String version) {
		firmware_version = version;
	}

	public int compare_version(String other_version) {
		if (firmware_version == null)
			return AltosLib.MISSING;
		return AltosLib.compare_version(firmware_version, other_version);
	}

	private void re_init() {
		int bt = boost_tick;
		int rs = receiver_serial;
		init();
		boost_tick = bt;
		receiver_serial = rs;
	}

	public void set_flight(int flight) {

		/* When the flight changes, reset the state */
		if (flight != AltosLib.MISSING) {
			if (this.flight != AltosLib.MISSING &&
			    this.flight != flight) {
				re_init();
			}
			this.flight = flight;
		}
	}

	public void set_serial(int serial) {
		/* When the serial changes, reset the state */
		if (serial != AltosLib.MISSING) {
			if (this.serial != AltosLib.MISSING &&
			    this.serial != serial) {
				re_init();
			}
			this.serial = serial;
		}
	}

	public void set_receiver_serial(int serial) {
		if (serial != AltosLib.MISSING)
			receiver_serial = serial;
	}

	public boolean altitude_32() {
		return altitude_32 == 1;
	}

	public void set_altitude_32(int altitude_32) {
		if (altitude_32 != AltosLib.MISSING)
			this.altitude_32 = altitude_32;
	}

	public int rssi() {
		if (rssi == AltosLib.MISSING)
			return 0;
		return rssi;
	}

	public void set_rssi(int rssi, int status) {
		if (rssi != AltosLib.MISSING) {
			this.rssi = rssi;
			this.status = status;
		}
	}

	public void set_received_time(long ms) {
		received_time = ms;
	}

	public void set_gps(AltosGPS gps, int sequence) {
		if (gps != null) {
			this.gps = gps.clone();
			gps_sequence = sequence;
			update_gps();
			set |= set_gps;
		}
	}


	public double	accel_zero_along;
	public double	accel_zero_across;
	public double	accel_zero_through;

	public AltosRotation	rotation;
	public AltosRotation	ground_rotation;

	public void set_accel_zero(double zero_along, double zero_across, double zero_through) {
		if (zero_along != AltosLib.MISSING) {
			accel_zero_along = zero_along;
			accel_zero_across = zero_across;
			accel_zero_through = zero_through;
		}
	}

	public int pad_orientation;

	public double	accel_ground_along, accel_ground_across, accel_ground_through;

	void update_pad_rotation() {
		if (pad_orientation != AltosLib.MISSING && accel_ground_along != AltosLib.MISSING) {
			rotation = new AltosRotation(AltosIMU.convert_accel(accel_ground_across - accel_zero_across),
						     AltosIMU.convert_accel(accel_ground_through - accel_zero_through),
						     AltosIMU.convert_accel(accel_ground_along - accel_zero_along),
						     pad_orientation);
			ground_rotation = rotation;
			orient.set_computed(rotation.tilt(), time);
		}
	}

	public void set_accel_ground(double ground_along, double ground_across, double ground_through) {
		accel_ground_along = ground_along;
		accel_ground_across = ground_across;
		accel_ground_through = ground_through;
		update_pad_rotation();
	}

	public void set_pad_orientation(int pad_orientation) {
		this.pad_orientation = pad_orientation;
		update_pad_rotation();
	}

	public double	gyro_zero_roll;
	public double	gyro_zero_pitch;
	public double	gyro_zero_yaw;

	public void set_gyro_zero(double roll, double pitch, double yaw) {
		if (roll != AltosLib.MISSING) {
			gyro_zero_roll = roll;
			gyro_zero_pitch = pitch;
			gyro_zero_yaw = yaw;
		}
	}

	public double	last_imu_time;

	private double radians(double degrees) {
		if (degrees == AltosLib.MISSING)
			return AltosLib.MISSING;
		return degrees * Math.PI / 180.0;
	}

	private void update_orient() {
		if (last_imu_time != AltosLib.MISSING) {
			double	t = time - last_imu_time;

			double	pitch = radians(gyro_pitch());
			double	yaw = radians(gyro_yaw());
			double	roll = radians(gyro_roll());

			if (t > 0 & pitch != AltosLib.MISSING && rotation != null) {
				rotation.rotate(t, pitch, yaw, roll);
				orient.set_computed(rotation.tilt(), time);
			}
		}
		last_imu_time = time;
	}

	public void set_imu(AltosIMU imu) {
		if (imu != null)
			imu = imu.clone();
		this.imu = imu;
		update_orient();
	}

	private double gyro_zero_overflow(double first) {
		double v = first / 128.0;
		if (v < 0)
			v = Math.ceil(v);
		else
			v = Math.floor(v);
		return v * 128.0;
	}

	public void check_imu_wrap(AltosIMU imu) {
		if (this.imu == null) {
			gyro_zero_roll += gyro_zero_overflow(imu.gyro_roll);
			gyro_zero_pitch += gyro_zero_overflow(imu.gyro_pitch);
			gyro_zero_yaw += gyro_zero_overflow(imu.gyro_yaw);
		}
	}

	public double accel_along() {
		if (imu != null && accel_zero_along != AltosLib.MISSING)
			return AltosIMU.convert_accel(imu.accel_along - accel_zero_along);
		return AltosLib.MISSING;
	}

	public double accel_across() {
		if (imu != null && accel_zero_across != AltosLib.MISSING)
			return AltosIMU.convert_accel(imu.accel_across - accel_zero_across);
		return AltosLib.MISSING;
	}

	public double accel_through() {
		if (imu != null && accel_zero_through != AltosLib.MISSING)
			return AltosIMU.convert_accel(imu.accel_through - accel_zero_through);
		return AltosLib.MISSING;
	}

	public double gyro_roll() {
		if (imu != null && gyro_zero_roll != AltosLib.MISSING) {
			return AltosIMU.convert_gyro(imu.gyro_roll - gyro_zero_roll);
		}
		return AltosLib.MISSING;
	}

	public double gyro_pitch() {
		if (imu != null && gyro_zero_pitch != AltosLib.MISSING) {
			return AltosIMU.convert_gyro(imu.gyro_pitch - gyro_zero_pitch);
		}
		return AltosLib.MISSING;
	}

	public double gyro_yaw() {
		if (imu != null && gyro_zero_yaw != AltosLib.MISSING) {
			return AltosIMU.convert_gyro(imu.gyro_yaw - gyro_zero_yaw);
		}
		return AltosLib.MISSING;
	}

	public void set_mag(AltosMag mag) {
		this.mag = mag.clone();
	}

	public double mag_along() {
		if (mag != null)
			return AltosMag.convert_gauss(mag.along);
		return AltosLib.MISSING;
	}

	public double mag_across() {
		if (mag != null)
			return AltosMag.convert_gauss(mag.across);
		return AltosLib.MISSING;
	}

	public double mag_through() {
		if (mag != null)
			return AltosMag.convert_gauss(mag.through);
		return AltosLib.MISSING;
	}

	public AltosMs5607 make_baro() {
		if (baro == null)
			baro = new AltosMs5607();
		return baro;
	}

	public void set_ms5607(AltosMs5607 ms5607) {
		baro = ms5607;

		if (baro != null) {
			set_pressure(baro.pa);
			set_temperature(baro.cc / 100.0);
		}
	}

	public void set_ms5607(int pres, int temp) {
		if (baro != null) {
			baro.set(pres, temp);

			set_pressure(baro.pa);
			set_temperature(baro.cc / 100.0);
		}
	}

	public void set_companion(AltosCompanion companion) {
		this.companion = companion;
	}

	void update_accel() {
		if (accel == AltosLib.MISSING)
			return;
		if (accel_plus_g == AltosLib.MISSING)
			return;
		if (accel_minus_g == AltosLib.MISSING)
			return;

		double counts_per_g = (accel_minus_g - accel_plus_g) / 2.0;
		double counts_per_mss = counts_per_g / 9.80665;
		acceleration.set_measured((accel_plus_g - accel) / counts_per_mss, time);
	}

	public void set_accel_g(double accel_plus_g, double accel_minus_g) {
		if (accel_plus_g != AltosLib.MISSING) {
			this.accel_plus_g = accel_plus_g;
			this.accel_minus_g = accel_minus_g;
			update_accel();
		}
	}

	public void set_ground_accel(double ground_accel) {
		if (ground_accel != AltosLib.MISSING)
			this.ground_accel = ground_accel;
	}

	public void set_accel(double accel) {
		if (accel != AltosLib.MISSING) {
			this.accel = accel;
			if (state == AltosLib.ao_flight_pad) {
				if (ground_accel_avg == AltosLib.MISSING)
					ground_accel_avg = accel;
				else
					ground_accel_avg = (ground_accel_avg * 7 + accel) / 8;
			}
		}
		update_accel();
	}

	public void set_temperature(double temperature) {
		if (temperature != AltosLib.MISSING) {
			this.temperature = temperature;
			set |= set_data;
		}
	}

	public void set_battery_voltage(double battery_voltage) {
		if (battery_voltage != AltosLib.MISSING) {
			this.battery_voltage = battery_voltage;
			set |= set_data;
		}
	}

	public void set_pyro_voltage(double pyro_voltage) {
		if (pyro_voltage != AltosLib.MISSING) {
			this.pyro_voltage = pyro_voltage;
			set |= set_data;
		}
	}

	public void set_apogee_voltage(double apogee_voltage) {
		if (apogee_voltage != AltosLib.MISSING) {
			this.apogee_voltage = apogee_voltage;
			set |= set_data;
		}
	}

	public void set_main_voltage(double main_voltage) {
		if (main_voltage != AltosLib.MISSING) {
			this.main_voltage = main_voltage;
			set |= set_data;
		}
	}

	public void set_ignitor_voltage(double[] voltage) {
		this.ignitor_voltage = voltage;
	}

	public void set_pyro_fired(int fired) {
		this.pyro_fired = fired;
	}

	public double time_since_boost() {
		if (tick == AltosLib.MISSING)
			return 0.0;

		if (boost_tick == AltosLib.MISSING)
			return tick / 100.0;
		return (tick - boost_tick) / 100.0;
	}

	public boolean valid() {
		return tick != AltosLib.MISSING && serial != AltosLib.MISSING;
	}

	public AltosGPS make_temp_gps(boolean sats) {
		if (temp_gps == null) {
			temp_gps = new AltosGPS(gps);
		}
		gps_pending = true;
		if (sats) {
			if (tick != temp_gps_sat_tick)
				temp_gps.cc_gps_sat = null;
			temp_gps_sat_tick = tick;
		}
		return temp_gps;
	}

	public void set_temp_gps() {
		set_gps(temp_gps, gps_sequence + 1);
		gps_pending = false;
		temp_gps = null;
	}

	public AltosState clone() {
		AltosState s = new AltosState();
		s.copy(this);

		if (false) {
			AltosJson	json = json();
			String		onetrip = json.toPrettyString();
			AltosJson	back = AltosJson.fromString(onetrip);
			AltosState	tripstate = AltosState.fromJson(back);
			AltosJson	tripjson = tripstate.json();
			String		twotrip = tripjson.toPrettyString();

			if (!onetrip.equals(twotrip)) {
				System.out.printf("one:\n%s\ntwo:\n%s\n", onetrip, twotrip);
				System.exit(1);
			}
		}
		return s;
	}

	public AltosState () {
		init();
	}

	public AltosJson json() {
		AltosJson	j = new AltosJson();

		j.put("valid", true);
		j.put("set", set);
		j.put("received_time", received_time);
		j.put("time", time);
		j.put("prev_time", prev_time);
		j.put("time_change", time_change);
		j.put("tick", tick);
		j.put("prev_tick", prev_tick);
		j.put("boost_tick", boost_tick);
		j.put("state", state);
		j.put("flight", flight);
		j.put("serial", serial);
		j.put("altitude_32", altitude_32);
		j.put("receiver_serial", receiver_serial);
		j.put("landed", landed);
		j.put("ascent", ascent);
		j.put("boost", boost);
		j.put("rssi", rssi);
		j.put("status", status);
		j.put("device_type", device_type);
		j.put("config_major", config_major);
		j.put("config_minor", config_minor);
		j.put("apogee_delay", apogee_delay);
		j.put("main_deploy", main_deploy);
		j.put("flight_log_max", flight_log_max);
		j.put("ground_altitude", ground_altitude);
		j.put("gps_ground_altitude", gps_ground_altitude);
		j.put("ground_pressure", ground_pressure);
		j.put("altitude", altitude);
		j.put("gps_altitude", gps_altitude);
		j.put("gps_ground_speed", gps_ground_speed);
		j.put("gps_ascent_rate", gps_ascent_rate);
		j.put("gps_course", gps_course);
		j.put("gps_speed", gps_speed);
		j.put("pressure", pressure);
		j.put("speed", speed);
		j.put("acceleration", acceleration);
		j.put("orient", orient);
		j.put("kalman_height", kalman_height);
		j.put("kalman_speed", kalman_speed);
		j.put("kalman_acceleration", kalman_acceleration);

		j.put("battery_voltage",battery_voltage);
		j.put("pyro_voltage",pyro_voltage);
		j.put("temperature",temperature);
		j.put("apogee_voltage",apogee_voltage);
		j.put("main_voltage",main_voltage);
		j.put("ignitor_voltage",ignitor_voltage);
		j.put("gps", gps);
		j.put("temp_gps", temp_gps);
		j.put("temp_gps_sat_tick", temp_gps_sat_tick);
		j.put("gps_pending", gps_pending);
		j.put("gps_sequence", gps_sequence);
		j.put("imu", imu);
		j.put("mag", mag);

		j.put("npad", npad);
		j.put("gps_waiting", gps_waiting);
		j.put("gps_ready", gps_ready);
		j.put("ngps", ngps);
		j.put("from_pad", from_pad);
		j.put("elevation", elevation);
		j.put("range", range);
		j.put("gps_height", gps_height);
		j.put("pad_lat", pad_lat);
		j.put("pad_lon", pad_lon);
		j.put("pad_alt", pad_alt);
		j.put("speak_tick", speak_tick);
		j.put("speak_altitude", speak_altitude);
		j.put("callsign", callsign);
		j.put("firmware_version", firmware_version);
		j.put("accel_plus_g", accel_plus_g);
		j.put("accel_minus_g", accel_minus_g);
		j.put("accel", accel);
		j.put("ground_accel", ground_accel);
		j.put("ground_accel_avg", ground_accel_avg);
		j.put("log_format", log_format);
		j.put("log_space", log_space);
		j.put("product", product);
		j.put("baro", baro);
		j.put("companion", companion);
		j.put("pyro_fired", pyro_fired);
		j.put("accel_zero_along", accel_zero_along);
		j.put("accel_zero_across", accel_zero_across);
		j.put("accel_zero_through", accel_zero_through);

		j.put("rotation", rotation);
		j.put("ground_rotation", ground_rotation);

		j.put("pad_orientation", pad_orientation);

		j.put("accel_ground_along", accel_ground_along);
		j.put("accel_ground_across", accel_ground_across);
		j.put("accel_ground_through", accel_ground_through);

		j.put("gyro_zero_roll", gyro_zero_roll);
		j.put("gyro_zero_pitch", gyro_zero_pitch);
		j.put("gyro_zero_yaw", gyro_zero_yaw);

		j.put("last_imu_time", last_imu_time);
		return j;
	}

	public AltosState(AltosJson j) {
		this();

		set = j.get_int("set", set);
		received_time = j.get_long("received_time", received_time);
		time = j.get_double("time", time);
		prev_time = j.get_double("prev_time", prev_time);
		time_change = j.get_double("time_change", time_change);
		tick = j.get_int("tick", tick);
		prev_tick = j.get_int("prev_tick", prev_tick);
		boost_tick = j.get_int("boost_tick", boost_tick);
		state = j.get_int("state", state);
		flight = j.get_int("flight", flight);
		serial = j.get_int("serial", serial);
		altitude_32 = j.get_int("altitude_32", altitude_32);
		receiver_serial = j.get_int("receiver_serial", receiver_serial);
		landed = j.get_boolean("landed", landed);
		ascent = j.get_boolean("ascent", ascent);
		boost = j.get_boolean("boost", boost);
		rssi = j.get_int("rssi", rssi);
		status = j.get_int("status", status);
		device_type = j.get_int("device_type", device_type);
		config_major = j.get_int("config_major", config_major);
		config_minor = j.get_int("config_minor", config_minor);
		apogee_delay = j.get_int("apogee_delay", apogee_delay);
		main_deploy = j.get_int("main_deploy", main_deploy);
		flight_log_max = j.get_int("flight_log_max", flight_log_max);
		ground_altitude = AltosCValue_fromJson(j.get("ground_altitude"), ground_altitude);
		gps_ground_altitude = AltosGpsGroundAltitude_fromJson(j.get("gps_ground_altitude"), gps_ground_altitude);
		ground_pressure = AltosGroundPressure_fromJson(j.get("ground_pressure"), ground_pressure);
		altitude = AltosAltitude_fromJson(j.get("altitude"), altitude);
		gps_altitude = AltosGpsAltitude_fromJson(j.get("gps_altitude"), gps_altitude);
		gps_ground_speed = AltosValue_fromJson(j.get("gps_ground_speed"), gps_ground_speed);
		gps_ascent_rate = AltosValue_fromJson(j.get("gps_ascent_rate"), gps_ascent_rate);
		gps_course = AltosValue_fromJson(j.get("gps_course"), gps_course);
		gps_speed = AltosValue_fromJson(j.get("gps_speed"), gps_speed);
		pressure = AltosPressure_fromJson(j.get("pressure"), pressure);
		speed = AltosSpeed_fromJson(j.get("speed"), speed);
		acceleration = AltosAccel_fromJson(j.get("acceleration"), acceleration);
		orient = AltosCValue_fromJson(j.get("orient"), orient);
		kalman_height = AltosValue_fromJson(j.get("kalman_height"), kalman_height);
		kalman_speed = AltosValue_fromJson(j.get("kalman_speed"), kalman_speed);
		kalman_acceleration = AltosValue_fromJson(j.get("kalman_acceleration"), kalman_acceleration);

		battery_voltage = j.get_double("battery_voltage", battery_voltage);
		pyro_voltage = j.get_double("pyro_voltage", pyro_voltage);
		temperature = j.get_double("temperature", temperature);
		apogee_voltage = j.get_double("apogee_voltage", apogee_voltage);
		main_voltage=  j.get_double("main_voltage", main_voltage);
		ignitor_voltage = j.get_double_array("ignitor_voltage", ignitor_voltage);
		gps = AltosGPS.fromJson(j.get("gps"), gps);
		temp_gps = AltosGPS.fromJson(j.get("temp_gps"), temp_gps);
		temp_gps_sat_tick = j.get_int("temp_gps_sat_tick", temp_gps_sat_tick);
		gps_pending = j.get_boolean("gps_pending", gps_pending);
		gps_sequence = j.get_int("gps_sequence", gps_sequence);
		imu = AltosIMU.fromJson(j.get("imu"), imu);
		mag = AltosMag.fromJson(j.get("mag"), mag);

		npad = j.get_int("npad", npad);
		gps_waiting = j.get_int("gps_waiting", gps_waiting);
		gps_ready = j.get_boolean("gps_ready", gps_ready);
		ngps = j.get_int("ngps", ngps);
		from_pad = AltosGreatCircle.fromJson(j.get("from_pad"), from_pad);
		elevation = j.get_double("elevation", elevation);
		range = j.get_double("range", range);
		gps_height = j.get_double("gps_height", gps_height);
		pad_lat = j.get_double("pad_lat", pad_lat);
		pad_lon = j.get_double("pad_lon", pad_lon);
		pad_alt = j.get_double("pad_alt", pad_alt);
		speak_tick = j.get_int("speak_tick", speak_tick);
		speak_altitude = j.get_double("speak_altitude", speak_altitude);
		callsign = j.get_string("callsign", callsign);
		firmware_version = j.get_string("firmware_version", firmware_version);
		accel_plus_g = j.get_double("accel_plus_g", accel_plus_g);
		accel_minus_g = j.get_double("accel_minus_g", accel_minus_g);
		accel = j.get_double("accel", accel);
		ground_accel = j.get_double("ground_accel", ground_accel);
		ground_accel_avg = j.get_double("ground_accel_avg", ground_accel_avg);
		log_format = j.get_int("log_format", log_format);
		log_space = j.get_int("log_space", log_space);
		product = j.get_string("product", product);
		baro = AltosMs5607.fromJson(j.get("baro"), baro);
		companion = AltosCompanion.fromJson(j.get("companion"), companion);
		pyro_fired = j.get_int("pyro_fired", pyro_fired);
		accel_zero_along = j.get_double("accel_zero_along", accel_zero_along);
		accel_zero_across = j.get_double("accel_zero_across", accel_zero_across);
		accel_zero_through = j.get_double("accel_zero_through", accel_zero_through);

		rotation = AltosRotation.fromJson(j.get("rotation"), rotation);
		ground_rotation = AltosRotation.fromJson(j.get("ground_rotation"), ground_rotation);

		pad_orientation = j.get_int("pad_orientation", pad_orientation);

		accel_ground_along = j.get_double("accel_ground_along", accel_ground_along);
		accel_ground_across = j.get_double("accel_ground_across", accel_ground_across);
		accel_ground_through = j.get_double("accel_ground_through", accel_ground_through);

		gyro_zero_roll = j.get_double("gyro_zero_roll", gyro_zero_roll);
		gyro_zero_pitch = j.get_double("gyro_zero_pitch", gyro_zero_pitch);
		gyro_zero_yaw = j.get_double("gyro_zero_yaw", gyro_zero_yaw);

		last_imu_time = j.get_double("last_imu_time", last_imu_time);
	}

	public static AltosState fromJson(AltosJson j) {
		if (j == null)
			return null;
		if (!j.get_boolean("valid", false))
			return null;
		return new AltosState(j);
	}
}
