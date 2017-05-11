/*
 * Copyright © 2017 Bdale Garbee <bdale@gag.com>
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

package org.altusmetrum.teststand;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import org.altusmetrum.altoslib_11.*;
import org.altusmetrum.altosuilib_11.*;

public class TestStandState extends AltosUIFlightTab {

	JLabel	cur, max;

	abstract class Value extends AltosUIUnitsIndicator {
		public Value (Container container, int y, AltosUnits units, String text) {
			super(container, y, units, text, 1, false, 2);
		}
	}

	abstract class DualValue extends AltosUIUnitsIndicator {
		public DualValue (Container container, int y, AltosUnits units, String text) {
			super(container, y, units, text, 2, false, 1);
		}
	}

	abstract class ValueHold extends DualValue {
		public ValueHold (Container container, int y, AltosUnits units, String text) {
			super(container, y, units, text);
		}
	}

	class FirmwareVersion extends AltosUIIndicator {
		public void show(AltosState state, AltosListenerState listener_state) {
			if (state.firmware_version == null)
				show("Missing");
			else
				show(state.firmware_version);
		}

		public FirmwareVersion(Container container, int y) {
			super(container, y, "Firmware Version", 1, false, 2);
		}
	}

	class FlightLogMax extends AltosUIIndicator {
		public void show(AltosState state, AltosListenerState listener_state) {
			int storage = state.flight_log_max;
			if (storage == AltosLib.MISSING)
				storage = state.log_space >> 10;
			if (storage == AltosLib.MISSING)
				show("Missing");
			else
				show(String.format("%dkB", storage));
		}

		public FlightLogMax(Container container, int y) {
			super(container, y, "Flight Log Storage", 1, false, 2);
		}
	}

	class BatteryVoltage extends AltosUIVoltageIndicator {
		public double voltage(AltosState state) {
			return state.battery_voltage;
		}

		public double good() {
			return AltosLib.ao_battery_good;
		}

		public BatteryVoltage(Container container, int y) {
			super(container, y, "Battery Voltage", 2);
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

	public void labels(Container container, int y) {
		GridBagLayout		layout = (GridBagLayout)(container.getLayout());
		GridBagConstraints	c;

		cur = new JLabel("Current");
		cur.setFont(AltosUILib.label_font);
		c = new GridBagConstraints();
		c.gridx = 2; c.gridy = y;
		c.insets = new Insets(AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad);
		layout.setConstraints(cur, c);
		add(cur);

		max = new JLabel("Maximum");
		max.setFont(AltosUILib.label_font);
		c.gridx = 3; c.gridy = y;
		layout.setConstraints(max, c);
		add(max);
	}

	public void font_size_changed(int font_size) {
		cur.setFont(AltosUILib.label_font);
		max.setFont(AltosUILib.label_font);
		super.font_size_changed(font_size);
	}

	public String getName() {
		return "Status";
	}

	public TestStandState() {
		int y = 0;
		labels(this, y++);
		add(new FirmwareVersion(this, y++));
		add(new FlightLogMax(this, y++));
		add(new BatteryVoltage(this, y++));
		add(new ReceiverBattery(this, y++));
	}
}
