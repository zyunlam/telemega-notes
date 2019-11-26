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

package org.altusmetrum.altoslib_13;

import java.io.*;
import java.util.*;
import java.text.*;
import java.util.concurrent.*;

class AltosIdler {
	String	prefix;
	int[]	idlers;

	static final int	idle_gps = 0;
	static final int	idle_imu = 1;
	static final int	idle_imu_em_v2 = 2;
	static final int	idle_mag = 3;
	static final int	idle_mma655x = 4;
	static final int	idle_ms5607 = 5;
	static final int	idle_adxl375 = 6;

	static final int	idle_sensor_tm = 10;
	static final int	idle_sensor_metrum = 11;
	static final int	idle_sensor_mega = 12;
	static final int	idle_sensor_emini1 = 13;
	static final int	idle_sensor_emini2 = 14;
	static final int	idle_sensor_tmini2 = 15;
	static final int	idle_sensor_tgps1 = 16;
	static final int	idle_sensor_tgps2 = 17;
	static final int	idle_sensor_tmini3 = 18;

	public void provide_data(AltosDataListener listener, AltosLink link) throws InterruptedException, TimeoutException, AltosUnknownProduct {
		for (int idler : idlers) {
			switch (idler) {
			case idle_gps:
				AltosGPS.provide_data(listener, link);
				break;
			case idle_imu:
				AltosIMU.provide_data(listener, link, AltosIMU.orient_telemega);
				break;
			case idle_imu_em_v2:
				AltosIMU.provide_data(listener, link, AltosIMU.orient_easymega_v2);
				break;
			case idle_mag:
				AltosMag.provide_data(listener, link);
				break;
			case idle_mma655x:
				AltosMma655x.provide_data(listener, link);
				break;
			case idle_adxl375:
				AltosAdxl375.provide_data(listener, link);
				break;
			case idle_ms5607:
				AltosMs5607.provide_data(listener, link);
				break;
			case idle_sensor_tm:
				AltosSensorTM.provide_data(listener, link);
				break;
			case idle_sensor_metrum:
				AltosSensorMetrum.provide_data(listener, link);
				break;
			case idle_sensor_mega:
				AltosSensorMega.provide_data(listener, link);
				break;
			case idle_sensor_emini1:
				AltosSensorEMini.provide_data(listener, link, 1);
				break;
			case idle_sensor_emini2:
				AltosSensorEMini.provide_data(listener, link, 2);
				break;
			case idle_sensor_tmini2:
				AltosSensorTMini2.provide_data(listener, link);
				break;
			case idle_sensor_tgps1:
				AltosSensorTGPS1.provide_data(listener, link);
				break;
			case idle_sensor_tgps2:
				AltosSensorTGPS2.provide_data(listener, link);
				break;
			case idle_sensor_tmini3:
				AltosSensorTMini3.provide_data(listener, link);
				break;
			}
		}
	}

	public boolean matches(AltosConfigData config_data) {
		return config_data.product.startsWith(prefix);
	}

	public AltosIdler(String prefix, int ... idlers) {
		this.prefix = prefix;
		this.idlers = idlers;
	}
}


public class AltosIdleFetch implements AltosDataProvider {

	static final AltosIdler[] idlers = {

		new AltosIdler("EasyMini-v1",
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_sensor_emini1),

		new AltosIdler("EasyMini-v2",
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_sensor_emini2),

		new AltosIdler("TeleMini-v1",
			       AltosIdler.idle_sensor_tm),

		new AltosIdler("TeleMini-v2",
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_sensor_tmini2),

		new AltosIdler("TeleMini-v3",
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_sensor_tmini3),

		new AltosIdler("TeleMetrum-v1",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_sensor_tm),

		new AltosIdler("TeleMetrum-v2",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_mma655x,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_sensor_metrum),

		new AltosIdler("TeleMetrum-v3",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_adxl375,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_sensor_metrum),

		new AltosIdler("TeleMega-v0",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_mma655x,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_imu, AltosIdler.idle_mag,
			       AltosIdler.idle_sensor_mega),
		new AltosIdler("TeleMega-v1",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_mma655x,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_imu, AltosIdler.idle_mag,
			       AltosIdler.idle_sensor_mega),
		new AltosIdler("TeleMega-v2",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_mma655x,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_imu, AltosIdler.idle_mag,
			       AltosIdler.idle_sensor_mega),
		new AltosIdler("TeleMega-v3",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_mma655x,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_imu,
			       AltosIdler.idle_sensor_mega),
		new AltosIdler("EasyMega-v1",
			       AltosIdler.idle_mma655x,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_imu, AltosIdler.idle_mag,
			       AltosIdler.idle_sensor_mega),
		new AltosIdler("EasyMega-v2",
			       AltosIdler.idle_adxl375,
			       AltosIdler.idle_ms5607,
			       AltosIdler.idle_imu_em_v2,
			       AltosIdler.idle_sensor_mega),
		new AltosIdler("TeleGPS-v1",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_sensor_tgps1),
		new AltosIdler("TeleGPS-v2",
			       AltosIdler.idle_gps,
			       AltosIdler.idle_sensor_tgps2),
	};

	AltosLink		link;

	public void provide_data(AltosDataListener listener) throws InterruptedException, AltosUnknownProduct {
		try {
			boolean	matched = false;
			/* Fetch config data from remote */
			AltosConfigData config_data = new AltosConfigData(link);
			listener.set_state(AltosLib.ao_flight_stateless);
			for (AltosIdler idler : idlers) {
				if (idler.matches(config_data)) {
					idler.provide_data(listener, link);
					matched = true;
					break;
				}
			}
			if (!matched)
				throw new AltosUnknownProduct(config_data.product);
			listener.set_received_time(System.currentTimeMillis());
		} catch (TimeoutException te) {
		}

	}

	public AltosIdleFetch(AltosLink link) {
		this.link = link;
	}
}
