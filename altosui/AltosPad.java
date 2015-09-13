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

package altosui;

import java.util.*;
import org.altusmetrum.altoslib_8.*;
import org.altusmetrum.altosuilib_8.*;

public class AltosPad extends AltosUIFlightTab {

	class Battery extends AltosUIVoltageIndicator {
		public double voltage(AltosState state) { return state.battery_voltage; }
		public double good() { return AltosLib.ao_battery_good; }
		public Battery (AltosUIFlightTab container, int y) { super(container, y, "Battery Voltage", 2); }
	}

	class Apogee extends AltosUIVoltageIndicator {
		public boolean hide(double v) { return v == AltosLib.MISSING; }
		public double voltage(AltosState state) { return state.apogee_voltage; }
		public double good() { return AltosLib.ao_igniter_good; }
		public Apogee (AltosUIFlightTab container, int y) { super(container, y, "Apogee Igniter Voltage", 2); }
	}

	class Main extends AltosUIVoltageIndicator {
		public boolean hide(double v) { return v == AltosLib.MISSING; }
		public double voltage(AltosState state) { return state.main_voltage; }
		public double good() { return AltosLib.ao_igniter_good; }
		public Main (AltosUIFlightTab container, int y) { super(container, y, "Main Igniter Voltage", 2); }
	}

	class LoggingReady extends AltosUIIndicator {
		public void show (AltosState state, AltosListenerState listener_state) {
			if (state == null || state.flight == AltosLib.MISSING) {
				hide();
			} else {
				if (state.flight != 0) {
					if (state.state() <= Altos.ao_flight_pad)
						show("Ready to record");
					else if (state.state() < Altos.ao_flight_landed ||
						 state.state() == AltosLib.ao_flight_stateless)
						show("Recording data");
					else
						show("Recorded data");
				} else
					show("Storage full");
				set_lights(state.flight != 0);
			}
		}
		public LoggingReady (AltosUIFlightTab container, int y) {
			super(container, y, "On-board Data Logging", 1, true, 2);
		}
	}

	class GPSLocked extends AltosUIIndicator {
		public void show (AltosState state, AltosListenerState listener_state) {
			if (state == null || state.gps == null)
				hide();
			else {
				int	sol = state.gps.nsat;
				int	sat = state.gps.cc_gps_sat == null ? 0 : state.gps.cc_gps_sat.length;
				show("%d in solution", sol, "%d in view", sat);
				set_lights(state.gps.locked && sol >= 4);
			}
		}
		public GPSLocked (AltosUIFlightTab container, int y) {
			super (container, y, "GPS Locked", 2, true, 1);
		}
	}

	class GPSReady extends AltosUIIndicator {
		public void show (AltosState state, AltosListenerState listener_state) {
			if (state == null || state.gps == null)
				hide();
			else {
				if (state.gps_ready)
					show("Ready");
				else
					show("Waiting %d", state.gps_waiting);
				set_lights(state.gps_ready);
			}
		}
		public GPSReady (AltosUIFlightTab container, int y) {
			super (container, y, "GPS Ready", 1, true, 2);
		}
	}

	class ReceiverBattery extends AltosUIVoltageIndicator {

		public double voltage(AltosState state) { return AltosLib.MISSING; }

		public double good() { return AltosLib.ao_battery_good; }

		public boolean hide(AltosState state, AltosListenerState listener_state, int i) {
			return value(state, listener_state, i) == AltosLib.MISSING;
		}

		public double value(AltosState state, AltosListenerState listener_state, int i) {
			if (listener_state == null)
				return AltosLib.MISSING;
			return listener_state.battery;
		}

		public ReceiverBattery (AltosUIFlightTab container, int y) {
			super(container, y, "Receiver Battery", 2);
		}
	}

	boolean report_pad(AltosState state) {
		if ((state.state() == AltosLib.ao_flight_stateless ||
		     state.state() < AltosLib.ao_flight_pad) &&
		    state.gps != null &&
		    state.gps.lat != AltosLib.MISSING)
		{
			return false;
		}
		return true;
	}

	class PadLat extends AltosUIIndicator {

		double	last_lat = AltosLib.MISSING - 1;

		public void show (AltosState state, AltosListenerState listener_state) {
			double lat = AltosLib.MISSING;
			String label = null;

			if (state != null) {
				if (report_pad(state)) {
					lat = state.pad_lat;
					label = "Pad Latitude";
				} else {
					lat = state.gps.lat;
					label = "Latitude";
				}
			}
			if (lat != last_lat) {
				if (lat != AltosLib.MISSING) {
					show(AltosConvert.latitude.show(10, lat));
					set_label(label);
				} else
					hide();
				last_lat = lat;
			}
		}

		public void reset() {
			super.reset();
			last_lat = AltosLib.MISSING - 1;
		}

		public PadLat (AltosUIFlightTab container, int y) {
			super (container, y, "Pad Latitude", 1, false, 2);
		}
	}

	class PadLon extends AltosUIIndicator {

		double last_lon = AltosLib.MISSING - 1;

		public void show (AltosState state, AltosListenerState listener_state) {
			double lon = AltosLib.MISSING;
			String label = null;

			if (state != null) {
				if (report_pad(state)) {
					lon = state.pad_lon;
					label = "Pad Longitude";
				} else {
					lon = state.gps.lon;
					label = "Longitude";
				}
			}
			if (lon != last_lon) {
				if (lon != AltosLib.MISSING) {
					show(AltosConvert.longitude.show(10, lon));
					set_label(label);
				} else
					hide();
				last_lon = lon;
			}
		}

		public void reset() {
			super.reset();
			last_lon = AltosLib.MISSING - 1;
		}

		public PadLon (AltosUIFlightTab container, int y) {
			super (container, y, "Pad Longitude", 1, false, 2);
		}
	}

	class PadAlt extends AltosUIIndicator {

		double	last_alt = AltosLib.MISSING - 1;

		public void show (AltosState state, AltosListenerState listener_state) {
			double alt = AltosLib.MISSING;
			String label = null;

			if (state != null) {
				if (report_pad(state)) {
					alt = state.pad_alt;
					label = "Pad Altitude";
				} else {
					alt = state.gps.alt;
					label = "Altitude";
				}
			}
			if (alt != last_alt) {
				if (alt != AltosLib.MISSING) {
					show(AltosConvert.height.show(5, alt));
					set_label(label);
				} else
					hide();
				last_alt = alt;
			}
		}

		public void reset() {
			super.reset();
			last_alt =  AltosLib.MISSING - 1;
		}

		public PadAlt (AltosUIFlightTab container, int y) {
			super (container, y, "Pad Altitude", 1, false, 2);
		}
	}
	public String getName() { return "Pad"; }

	public AltosPad() {
		int y = 0;
		add(new Battery(this, y++));
		add(new ReceiverBattery(this, y++));
		add(new Apogee(this, y++));
		add(new Main(this, y++));
		add(new LoggingReady(this, y++));
		add(new GPSLocked(this, y++));
		add(new GPSReady(this, y++));
		add(new PadLat(this, y++));
		add(new PadLon(this, y++));
		add(new PadAlt(this, y++));
	}
}
