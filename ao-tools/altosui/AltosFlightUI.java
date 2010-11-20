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

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;
import java.io.*;
import java.util.*;
import java.text.*;
import java.util.prefs.*;
import java.util.concurrent.LinkedBlockingQueue;

public class AltosFlightUI extends JFrame implements AltosFlightDisplay {
	String[] statusNames = { "Height (m)", "State", "RSSI (dBm)", "Speed (m/s)" };
	Object[][] statusData = { { "0", "pad", "-50", "0" } };

	AltosVoice		voice;
	AltosFlightReader	reader;
	AltosDisplayThread	thread;

	JTabbedPane	pane;

	AltosPad	pad;
	AltosAscent	ascent;
	AltosDescent	descent;
	AltosLanded	landed;
    AltosSiteMap    sitemap;

	private AltosFlightStatus flightStatus;
	private JScrollPane flightInfoPane;
	private JScrollPane sitemapPane;
	private AltosInfoTable flightInfo;

	static final int tab_pad = 1;
	static final int tab_ascent = 2;
	static final int tab_descent = 3;
	static final int tab_landed = 4;

	int cur_tab = 0;

	boolean exit_on_close = false;

	int which_tab(AltosState state) {
		if (state.state < Altos.ao_flight_boost)
			return tab_pad;
		if (state.state <= Altos.ao_flight_coast)
			return tab_ascent;
		if (state.state <= Altos.ao_flight_main)
			return tab_descent;
		return tab_landed;
	}

	public int width() {
		return flightInfo.width();
	}

	public int height() {
		return flightStatus.height() + flightInfo.height();
	}

	void stop_display() {
		if (thread != null && thread.isAlive()) {
			thread.interrupt();
			try {
				thread.join();
			} catch (InterruptedException ie) {}
		}
		thread = null;
	}

	void disconnect() {
		stop_display();
	}

	public void reset() {
		pad.reset();
		ascent.reset();
		descent.reset();
		landed.reset();
		flightInfo.clear();
		sitemap.reset();
	}

	public void show(AltosState state, int crc_errors) {
		int	tab = which_tab(state);
		pad.show(state, crc_errors);
		ascent.show(state, crc_errors);
		descent.show(state, crc_errors);
		landed.show(state, crc_errors);
		if (tab != cur_tab) {
			switch (tab) {
			case tab_pad:
				pane.setSelectedComponent(pad);
				break;
			case tab_ascent:
				pane.setSelectedComponent(ascent);
				break;
			case tab_descent:
				pane.setSelectedComponent(descent);
				break;
			case tab_landed:
				pane.setSelectedComponent(landed);
			}
			cur_tab = tab;
		}
		flightStatus.show(state, crc_errors);
		flightInfo.show(state, crc_errors);
		sitemap.show(state, crc_errors);
	}

	public void set_exit_on_close() {
		exit_on_close = true;
	}

	Container	bag;

	public AltosFlightUI(AltosVoice in_voice, AltosFlightReader in_reader, final int serial) {
		AltosPreferences.init(this);

		voice = in_voice;
		reader = in_reader;

		bag = getContentPane();
		bag.setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();

		java.net.URL imgURL = AltosUI.class.getResource("/altus-metrum-16x16.jpg");
		if (imgURL != null)
			setIconImage(new ImageIcon(imgURL).getImage());

		setTitle(String.format("AltOS %s", reader.name));

		if (serial >= 0) {
			// Channel menu
			JComboBox channels = new AltosChannelMenu(AltosPreferences.channel(serial));
			channels.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						int channel = Integer.parseInt(e.getActionCommand());
						reader.set_channel(channel);
						AltosPreferences.set_channel(serial, channel);
					}
				});
			c.gridx = 0;
			c.gridy = 0;
			c.anchor = GridBagConstraints.WEST;
			bag.add (channels, c);
		}

		flightStatus = new AltosFlightStatus();
		c.gridx = 0;
		c.gridy = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1;
		bag.add(flightStatus, c);

		pane = new JTabbedPane();

		pad = new AltosPad();
		pane.add("Launch Pad", pad);

		ascent = new AltosAscent();
		pane.add("Ascent", ascent);

		descent = new AltosDescent();
		pane.add("Descent", descent);

		landed = new AltosLanded();
		pane.add("Landed", landed);

		flightInfo = new AltosInfoTable();
		flightInfoPane = new JScrollPane(flightInfo.box());
		pane.add("Table", flightInfoPane);

        sitemap = new AltosSiteMap();
		sitemapPane = new JScrollPane(sitemap);
        pane.add("Site Map", sitemapPane);

		c.gridx = 0;
		c.gridy = 2;
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		bag.add(pane, c);

		this.setSize(this.getPreferredSize());
		this.validate();

		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				disconnect();
				setVisible(false);
				dispose();
				if (exit_on_close)
					System.exit(0);
			}
		});

		this.setVisible(true);

		thread = new AltosDisplayThread(this, voice, this, reader);

		thread.start();
	}

	public AltosFlightUI (AltosVoice in_voice, AltosFlightReader in_reader) {
		this(in_voice, in_reader, -1);
	}
}
