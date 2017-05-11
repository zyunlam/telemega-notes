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

import java.io.*;
import java.util.ArrayList;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import java.util.concurrent.*;
import java.util.*;
import org.altusmetrum.altoslib_11.*;
import org.altusmetrum.altosuilib_11.*;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.ui.RefineryUtilities;

public class TestStandGraphUI extends AltosUIFrame
{
	JTabbedPane		pane;
	TestStandGraph		graph;
	AltosUIEnable		enable;
	AltosState		state;
	AltosFlightStats	stats;
	TestStandDataSet	graphDataSet;
	AltosFlightStatsTable	statsTable;

	private void close() {
		setVisible(false);
		dispose();
		TestStand.subtract_window();
	}

	TestStandGraphUI(AltosStateIterable states, File file) throws InterruptedException, IOException {
		super(file.getName());
		state = null;

		pane = new JTabbedPane();

		enable = new AltosUIEnable();
		stats = new AltosFlightStats(states);
		graphDataSet = new TestStandDataSet(states);
		graph = new TestStandGraph(enable, stats, graphDataSet);
		statsTable = new AltosFlightStatsTable(stats);

		pane.add("Graph", graph.panel);
		pane.add("Configure Graph", enable);
		pane.add("Statistics", statsTable);

		setContentPane (pane);

		addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosing(WindowEvent e) {
					close();
				}
			});

		pack();

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

		TestStand.add_window();

		setVisible(true);

	}
}
