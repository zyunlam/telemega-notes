/*
 * Copyright © 2010 Anthony Towns
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

package org.altusmetrum.telegps;

import java.io.*;
import java.util.ArrayList;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import java.util.concurrent.*;
import java.util.*;
import org.altusmetrum.altoslib_12.*;
import org.altusmetrum.altosuilib_12.*;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.ui.RefineryUtilities;

public class TeleGPSGraphUI extends AltosUIFrame implements AltosFontListener, AltosUnitsListener, AltosFilterListener
{
	JTabbedPane		pane;
	AltosGraph		graph;
	AltosUIEnable		enable;
	AltosUIMap		map;
	AltosState		state;
	AltosFlightStats	stats;
	AltosFlightStatsTable	statsTable;
	AltosGPS		gps;
	boolean			has_gps;

	void fill_map(AltosFlightSeries flight_series) {
		boolean			any_gps = false;
		AltosGPSTimeValue	gtv_last = null;

		if (flight_series.gps_series != null) {
			for (AltosGPSTimeValue gtv : flight_series.gps_series) {
				gtv_last = gtv;
				AltosGPS gps = gtv.gps;
				if (gps != null &&
				    gps.locked &&
				    gps.nsat >= 4) {
					if (map == null)
						map = new AltosUIMap();
					map.show(gps, (int) flight_series.value_before(AltosFlightSeries.state_name, gtv.time));
					this.gps = gps;
					has_gps = true;
				}
			}
		}
		if (gtv_last != null) {
			int state = (int) flight_series.value_after(AltosFlightSeries.state_name, gtv_last.time);
			if (state == AltosLib.ao_flight_landed)
				map.show(gtv_last.gps, state);
		}
	}

	private void close() {
		setVisible(false);
		dispose();
		TeleGPS.subtract_window();
	}

	public void font_size_changed(int font_size) {
		if (map != null)
			map.font_size_changed(font_size);
		if (statsTable != null)
			statsTable.font_size_changed(font_size);
	}

	public void units_changed(boolean imperial_units) {
		if (map != null)
			map.units_changed(imperial_units);
		if (enable != null)
			enable.units_changed(imperial_units);
	}

	AltosUIFlightSeries flight_series;

	public void filter_changed(double speed_filter, double accel_filter) {
		flight_series.set_filter(speed_filter, accel_filter);
		graph.filter_changed();
		stats = new AltosFlightStats(flight_series);
		statsTable.filter_changed(stats);
	}

	public double speed_filter() {
		return flight_series.speed_filter_width;
	}

	public double accel_filter() {
		return flight_series.accel_filter_width;
	}

	TeleGPSGraphUI(AltosRecordSet set, File file) throws InterruptedException, IOException {
		super(file.getName());
		AltosCalData cal_data = set.cal_data();

		flight_series = new AltosUIFlightSeries(cal_data);
		set.capture_series(flight_series);
		flight_series.finish();

		pane = new JTabbedPane();

		graph = new AltosGraph(enable, stats, flight_series);

		stats = new AltosFlightStats(flight_series);

		enable = new AltosUIEnable(this);

		statsTable = new AltosFlightStatsTable(stats);

		map = new AltosUIMap();

		pane.add("Graph", graph.panel);
		pane.add("Configure Graph", enable);
		pane.add("Statistics", statsTable);
		fill_map(flight_series);
		pane.add("Map", map);

		setContentPane (pane);

		AltosUIPreferences.register_font_listener(this);
		AltosPreferences.register_units_listener(this);

		addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosing(WindowEvent e) {
					close();
				}
			});

		pack();

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

		TeleGPS.add_window();

		setVisible(true);

		if (state != null)
			map.centre(state);

	}
}
