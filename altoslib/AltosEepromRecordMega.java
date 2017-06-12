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

public class AltosEepromRecordMega extends AltosEepromRecord {
	public static final int	record_length = 32;

	public static final int max_sat = 12;

	private int log_format;

	/* AO_LOG_FLIGHT elements */
	private int flight() { return data16(0); }
	private int ground_accel() { return data16(2); }
	private int ground_pres() { return data32(4); }
	private int ground_accel_along() { return data16(8); }
	private int ground_accel_across() { return data16(10); }
	private int ground_accel_through() { return data16(12); }
	private int ground_roll() {
		switch (log_format) {
		case AltosLib.AO_LOG_FORMAT_TELEMEGA:
			return data32(16);
		case AltosLib.AO_LOG_FORMAT_TELEMEGA_OLD:
			return data16(14);
		default:
			return AltosLib.MISSING;
		}
	}
	private int ground_pitch() {
		switch (log_format) {
		case AltosLib.AO_LOG_FORMAT_TELEMEGA:
			return data32(20);
		case AltosLib.AO_LOG_FORMAT_TELEMEGA_OLD:
			return data16(16);
		default:
			return AltosLib.MISSING;
		}
	}
	private int ground_yaw() {
		switch (log_format) {
		case AltosLib.AO_LOG_FORMAT_TELEMEGA:
			return data32(24);
		case AltosLib.AO_LOG_FORMAT_TELEMEGA_OLD:
			return data16(18);
		default:
			return AltosLib.MISSING;
		}
	}

	/* AO_LOG_STATE elements */
	private int state() { return data16(0); }
	private int reason() { return data16(2); }

	/* AO_LOG_SENSOR elements */
	private int pres() { return data32(0); }
	private int temp() { return data32(4); }
	private int accel_x() { return data16(8); }
	private int accel_y() { return data16(10); }
	private int accel_z() { return data16(12); }
	private int gyro_x() { return data16(14); }
	private int gyro_y() { return data16(16); }
	private int gyro_z() { return data16(18); }
	private int mag_x() { return data16(20); }
	private int mag_z() { return data16(22); }
	private int mag_y() { return data16(24); }
	private int accel() { return data16(26); }

	/* AO_LOG_TEMP_VOLT elements */
	private int v_batt() { return data16(0); }
	private int v_pbatt() { return data16(2); }
	private int nsense() { return data16(4); }
	private int sense(int i) { return data16(6 + i * 2); }
	private int pyro() { return data16(26); }

	/* AO_LOG_GPS_TIME elements */
	private int latitude() { return data32(0); }
	private int longitude() { return data32(4); }
	private int altitude_low() { return data16(8); }
	private int hour() { return data8(10); }
	private int minute() { return data8(11); }
	private int second() { return data8(12); }
	private int flags() { return data8(13); }
	private int year() { return data8(14); }
	private int month() { return data8(15); }
	private int day() { return data8(16); }
	private int course() { return data8(17); }
	private int ground_speed() { return data16(18); }
	private int climb_rate() { return data16(20); }
	private int pdop() { return data8(22); }
	private int hdop() { return data8(23); }
	private int vdop() { return data8(24); }
	private int mode() { return data8(25); }
	private int altitude_high() { return data16(26); }

	/* AO_LOG_GPS_SAT elements */
	private int nsat() { return data16(0); }
	private int svid(int n) { return data8(2 + n * 2); }
	private int c_n(int n) { return data8(2 + n * 2 + 1); }

	public void provide_data(AltosDataListener listener, AltosCalData cal_data) {
		super.provide_data(listener, cal_data);

		AltosGPS	gps;

		switch (cmd()) {
		case AltosLib.AO_LOG_FLIGHT:
			cal_data.set_flight(flight());
			cal_data.set_ground_accel(ground_accel());
			cal_data.set_ground_pressure(ground_pres());
			listener.set_accel_ground(ground_accel_along(),
						  ground_accel_across(),
						  ground_accel_through());
			cal_data.set_gyro_zero(ground_roll() / 512.0,
					       ground_pitch() / 512.0,
					       ground_yaw() / 512.0);
			break;
		case AltosLib.AO_LOG_STATE:
			listener.set_state(state());
			break;
		case AltosLib.AO_LOG_SENSOR:
			AltosConfigData config_data = eeprom.config_data();
			AltosPresTemp pt = config_data.ms5607().pres_temp(pres(), temp());;
			listener.set_pressure(pt.pres);
			listener.set_temperature(pt.temp);

			int	accel_along = accel_y();
			int	accel_across = accel_x();
			int	accel_through = accel_z();
			int	gyro_roll = gyro_y();
			int	gyro_pitch = gyro_x();
			int	gyro_yaw = gyro_z();

			int	mag_along = mag_y();
			int	mag_across = mag_x();
			int	mag_through = mag_z();

			if (log_format == AltosLib.AO_LOG_FORMAT_TELEMEGA_OLD)
				cal_data.check_imu_wrap(gyro_roll, gyro_pitch, gyro_yaw);

			listener.set_accel(cal_data.accel_along(accel_along),
					   cal_data.accel_across(accel_across),
					   cal_data.accel_through(accel_through));
			listener.set_gyro(cal_data.gyro_roll(gyro_roll),
					  cal_data.gyro_pitch(gyro_pitch),
					  cal_data.gyro_yaw(gyro_yaw));

			listener.set_mag(cal_data.mag_along(mag_along),
					 cal_data.mag_across(mag_across),
					 cal_data.mag_through(mag_through));


			final double lsb_per_g = 1920.0/105.5;

			double acceleration = AltosConvert.acceleration_from_sensor(
				accel(),
				cal_data.ground_accel,
				cal_data.ground_accel + 2 * lsb_per_g,
				cal_data.ground_accel);

			listener.set_acceleration(acceleration);
			break;
		case AltosLib.AO_LOG_TEMP_VOLT:
			listener.set_battery_voltage(AltosConvert.mega_battery_voltage(v_batt()));
			listener.set_pyro_voltage(AltosConvert.mega_pyro_voltage(v_pbatt()));

			int nsense = nsense();

			listener.set_apogee_voltage(AltosConvert.mega_pyro_voltage(sense(nsense-2)));
			listener.set_main_voltage(AltosConvert.mega_pyro_voltage(sense(nsense-1)));

			double voltages[] = new double[nsense-2];
			for (int i = 0; i < nsense-2; i++)
				voltages[i] = AltosConvert.mega_pyro_voltage(sense(i));

			listener.set_igniter_voltage(voltages);
			listener.set_pyro_fired(pyro());
			break;
		case AltosLib.AO_LOG_GPS_TIME:
			gps = cal_data.make_temp_gps(tick(), false);
			gps.lat = latitude() / 1e7;
			gps.lon = longitude() / 1e7;

			if (config_data().altitude_32())
				gps.alt = (altitude_low() & 0xffff) | (altitude_high() << 16);
			else
				gps.alt = altitude_low();

			gps.hour = hour();
			gps.minute = minute();
			gps.second = second();

			int flags = flags();

			gps.connected = (flags & AltosLib.AO_GPS_RUNNING) != 0;
			gps.locked = (flags & AltosLib.AO_GPS_VALID) != 0;
			gps.nsat = (flags & AltosLib.AO_GPS_NUM_SAT_MASK) >>
				AltosLib.AO_GPS_NUM_SAT_SHIFT;

			gps.year = 2000 + year();
			gps.month = month();
			gps.day = day();
			gps.ground_speed = ground_speed() * 1.0e-2;
			gps.course = course() * 2;
			gps.climb_rate = climb_rate() * 1.0e-2;
			if (config_data().compare_version("1.4.9") >= 0) {
				gps.pdop = pdop() / 10.0;
				gps.hdop = hdop() / 10.0;
				gps.vdop = vdop() / 10.0;
			} else {
				gps.pdop = pdop() / 100.0;
				if (gps.pdop < 0.8)
					gps.pdop += 2.56;
				gps.hdop = hdop() / 100.0;
				if (gps.hdop < 0.8)
					gps.hdop += 2.56;
				gps.vdop = vdop() / 100.0;
				if (gps.vdop < 0.8)
					gps.vdop += 2.56;
			}
			break;
		case AltosLib.AO_LOG_GPS_SAT:
			gps = cal_data.make_temp_gps(tick(), true);

			int n = nsat();
			if (n > max_sat)
				n = max_sat;
			for (int i = 0; i < n; i++)
				gps.add_sat(svid(i), c_n(i));
			break;
		}
	}

	public AltosEepromRecord next() {
		int	s = next_start();
		if (s < 0)
			return null;
		return new AltosEepromRecordMega(eeprom, s);
	}

	public AltosEepromRecordMega(AltosEepromNew eeprom, int start) {
		super(eeprom, start, record_length);
		log_format = eeprom.config_data().log_format;
	}

	public AltosEepromRecordMega(AltosEepromNew eeprom) {
		this(eeprom, 0);
	}
}
