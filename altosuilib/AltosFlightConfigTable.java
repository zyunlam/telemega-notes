/*
 * Copyright © 2024 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altosuilib_14;

import java.awt.*;
import javax.swing.*;
import java.util.*;
import org.altusmetrum.altoslib_14.*;

public class AltosFlightConfigTable extends JComponent
	implements AltosFontListener, AltosUnitsListener
{
	GridBagLayout	layout;

	FlightConfig[] flight_configs;
	AltosConfigData	config_data;

	class FlightConfig implements AltosFontListener {
		JLabel		label;
		JLabel[]	value;

		public void font_size_changed(int font_size) {
			label.setFont(AltosUILib.label_font);
			for (int i = 0; i < value.length; i++)
				value[i].setFont(AltosUILib.value_font);
		}

		public void set(String l, String[] values) {
			label.setText(l);
			for (int j = 0; j < values.length; j++)
				value[j].setText(values[j]);
		}

		public FlightConfig(GridBagLayout layout, int y, String label_text, String ... values) {
			GridBagConstraints	c = new GridBagConstraints();
			c.insets = new Insets(AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad);
			c.weighty = 1;

			label = new JLabel(label_text);
			label.setFont(AltosUILib.label_font);
			label.setHorizontalAlignment(SwingConstants.LEFT);
			c.gridx = 0; c.gridy = y;
			c.anchor = GridBagConstraints.WEST;
			c.fill = GridBagConstraints.VERTICAL;
			c.weightx = 0;
			layout.setConstraints(label, c);
			add(label);

			value = new JLabel[values.length];
			for (int j = 0; j < values.length; j++) {
				value[j] = new JLabel(values[j]);
				value[j].setFont(AltosUILib.value_font);
				c.gridx = j+1; c.gridy = y;
				c.anchor = GridBagConstraints.CENTER;
				c.fill = GridBagConstraints.BOTH;
				c.weightx = 1;
				layout.setConstraints(value[j], c);
				add(value[j]);
			}
			flight_configs[y] = this;
		}

	}

	private FlightConfig set_config(int y, String label, String ... values) {
		if (flight_configs[y] == null)
			flight_configs[y] = new FlightConfig(layout, y, label, values);
		else
			flight_configs[y].set(label, values);
		return flight_configs[y];
	}

	public void font_size_changed(int font_size) {
		for (int y = 0; flight_configs[y] != null; y++)
			flight_configs[y].font_size_changed(font_size);
	}

	private String main_deploy_label() {
		return String.format("Main Deploy Altitude(%s)", AltosConvert.height.parse_units());
	}

	private String main_deploy_value() {
		return String.format("%-6.1f", AltosConvert.height.value(config_data.main_deploy, AltosConvert.imperial_units));
	}

	public void set_config() {
		int y = 0;
		if (config_data.serial != AltosLib.MISSING) {
			if (config_data.product != null && config_data.version != null)
				set_config(y++, "Device",
					   config_data.product,
					   String.format("version %s", config_data.version),
					   String.format("serial %d", config_data.serial));
			else
				set_config(y++, "Serial", String.format("%d", config_data.serial));
		}
		if (config_data.flight != AltosLib.MISSING)
			set_config(y++, "Flight", String.format("%d", config_data.flight));
		if (config_data.main_deploy != AltosLib.MISSING)
			set_config(y++, main_deploy_label(), main_deploy_value());
		if (config_data.apogee_delay != AltosLib.MISSING)
			set_config(y++, "Apogee Delay(s)", String.format("%d", config_data.apogee_delay));
		if (config_data.apogee_lockout != AltosLib.MISSING)
			set_config(y++, "Apogee Lockout(s)", String.format("%d", config_data.apogee_lockout));
		if (config_data.radio_frequency != AltosLib.MISSING)
			set_config(y++, "Radio Frequency (MHz)", String.format("%-7.3f", config_data.radio_frequency / 1000.0));
		if (config_data.radio_calibration != AltosLib.MISSING)
			set_config(y++, "Radio Calibration", String.format("%d", config_data.radio_calibration));
		if (config_data.radio_enable != AltosLib.MISSING)
			set_config(y++, "Radio Enable", String.format("%b", config_data.radio_enable != 0));
		if (config_data.radio_10mw != AltosLib.MISSING)
			set_config(y++, "Limit transmit to 10mW", String.format("%b", config_data.radio_10mw != 0));
		if (config_data.report_feet != AltosLib.MISSING)
			set_config(y++, "Beep max height in", config_data.report_feet == 0 ? "Meters" : "Feet");
		if (config_data.gps_receiver != AltosLib.MISSING)
			set_config(y++, "GPS Receiver", AltosLib.gps_receiver_names[config_data.gps_receiver]);
		if (config_data.telemetry_rate != AltosLib.MISSING)
			set_config(y++, "Telemetry baud rate", String.format("%d", AltosLib.ao_telemetry_rate_values[config_data.telemetry_rate]));
		if (config_data.aprs_interval != AltosLib.MISSING)
			set_config(y++, "APRS Interval(s)", String.format("%d", config_data.aprs_interval));
		if (config_data.aprs_ssid != AltosLib.MISSING)
			set_config(y++, "APRS SSID", String.format("%d", config_data.aprs_ssid));
		if (config_data.aprs_format != AltosLib.MISSING)
			set_config(y++, "APRS Format", AltosLib.ao_aprs_format_name[config_data.aprs_format]);
		if (config_data.callsign != null)
			set_config(y++, "Callsign", config_data.callsign);
		if (config_data.flight_log_max != AltosLib.MISSING)
			set_config(y++, "Maximum Flight Log Size(kB)", String.format("%d", config_data.flight_log_max));
		if (config_data.ignite_mode != AltosLib.MISSING)
			set_config(y++, "Igniter Firing Mode", AltosLib.ignite_mode_values[config_data.ignite_mode]);
		if (config_data.pad_orientation != AltosLib.MISSING)
			set_config(y++, "Pad Orientation", AltosLib.pad_orientation_values(config_data.has_radio())[config_data.pad_orientation]);
		if (config_data.accel_cal_plus != AltosLib.MISSING)
			set_config(y++, "Accel Calibration",
				   String.format("Plus %d", config_data.accel_cal_plus),
				   String.format("Minus %d", config_data.accel_cal_minus));
		if (config_data.beep != AltosLib.MISSING)
			set_config(y++, "Beeper(Hz)",
				   config_data.beep == 0 ? "Disabled" :
				   String.format("%-7.1f", AltosConvert.beep_value_to_freq(config_data.beep)));
		if (config_data.tracker_motion != AltosLib.MISSING)
			set_config(y++,
				   String.format("Logging Trigger Motion (%s):", AltosConvert.height.parse_units()),
				   String.format("%-6.1f",
						 AltosConvert.height.value(config_data.tracker_motion, AltosConvert.imperial_units)));
		if (config_data.tracker_interval != AltosLib.MISSING)
			set_config(y++, "Position Reporting Interval(s)",
				   String.format("%d", config_data.tracker_interval));
	}

	public void units_changed(boolean imperial_units) {
		set_config();
	}

	public void tell_closing() {
		AltosUIPreferences.unregister_font_listener(this);
	}

	public AltosFlightConfigTable() {
		layout = new GridBagLayout();

		setLayout(layout);

		AltosUIPreferences.register_font_listener(this);
	}

	public AltosFlightConfigTable(AltosConfigData config_data) {
		this();
		this.config_data = config_data;
		flight_configs = new FlightConfig[30];
		set_config();
	}
}
