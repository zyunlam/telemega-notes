/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altosuilib_10;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import java.util.*;
import java.text.*;
import java.lang.Math;
import java.net.URL;
import java.net.URLConnection;
import org.altusmetrum.altoslib_10.*;

class AltosUIMapPos extends Box {
	AltosUIFrame	owner;
	JLabel		label;
	JComboBox	hemi;
	JTextField	deg;
	JLabel		deg_label;
	JTextField	min;
	JLabel		min_label;

	public void set_value(double new_value) {
		double	d, m;
		int	h;

		h = 0;
		if (new_value < 0) {
			h = 1;
			new_value = -new_value;
		}
		d = Math.floor(new_value);
		deg.setText(String.format("%3.0f", d));
		m = (new_value - d) * 60.0;
		min.setText(String.format("%7.4f", m));
		hemi.setSelectedIndex(h);
	}

	public double get_value() throws ParseException {
		int	h = hemi.getSelectedIndex();
		String	d_t = deg.getText();
		String	m_t = min.getText();
		double 	d, m, v;
		try {
			d = AltosParse.parse_double_locale(d_t);
		} catch (ParseException pe) {
			JOptionPane.showMessageDialog(owner,
						      String.format("Invalid degrees \"%s\"",
								    d_t),
						      "Invalid number",
						      JOptionPane.ERROR_MESSAGE);
			throw pe;
		}
		try {
			if (m_t.equals(""))
				m = 0;
			else
				m = AltosParse.parse_double_locale(m_t);
		} catch (ParseException pe) {
			JOptionPane.showMessageDialog(owner,
						      String.format("Invalid minutes \"%s\"",
								    m_t),
						      "Invalid number",
						      JOptionPane.ERROR_MESSAGE);
			throw pe;
		}
		v = d + m/60.0;
		if (h == 1)
			v = -v;
		return v;
	}

	public AltosUIMapPos(AltosUIFrame in_owner,
			   String label_value,
			   String[] hemi_names,
			   double default_value) {
		super(BoxLayout.X_AXIS);
		owner = in_owner;
		label = new JLabel(label_value);
		hemi = new JComboBox<String>(hemi_names);
		hemi.setEditable(false);
		deg = new JTextField(5);
		deg.setMinimumSize(deg.getPreferredSize());
		deg.setHorizontalAlignment(JTextField.RIGHT);
		deg_label = new JLabel("°");
		min = new JTextField(9);
		min.setMinimumSize(min.getPreferredSize());
		min_label = new JLabel("'");
		set_value(default_value);
		add(label);
		add(Box.createRigidArea(new Dimension(5, 0)));
		add(hemi);
		add(Box.createRigidArea(new Dimension(5, 0)));
		add(deg);
		add(Box.createRigidArea(new Dimension(5, 0)));
		add(deg_label);
		add(Box.createRigidArea(new Dimension(5, 0)));
		add(min);
		add(Box.createRigidArea(new Dimension(5, 0)));
		add(min_label);
	}
}

public class AltosUIMapPreloadNew extends AltosUIFrame implements ActionListener, ItemListener, AltosLaunchSiteListener, AltosMapLoaderListener  {
	AltosUIFrame	owner;
	AltosUIMapNew	map;

	AltosUIMapPos	lat;
	AltosUIMapPos	lon;

	JProgressBar	pbar;

	JLabel		site_list_label;
	JComboBox<AltosLaunchSite>	site_list;

	JToggleButton	load_button;
	boolean		loading;
	JButton		close_button;

	JCheckBox[]	maptypes = new JCheckBox[AltosMap.maptype_terrain - AltosMap.maptype_hybrid + 1];

	JComboBox<Integer>	min_zoom;
	JComboBox<Integer>	max_zoom;
	JComboBox<Double>	radius;

	Integer[]		zooms = { -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6 };

	Double[]	radius_mi = { 1.0, 2.0, 5.0, 10.0, 20.0 };
	Double		radius_def_mi = 5.0;
	Double[]	radius_km = { 2.0, 5.0, 10.0, 20.0, 30.0 };
	Double		radius_def_km = 10.0;


	static final String[]	lat_hemi_names = { "N", "S" };
	static final String[]	lon_hemi_names = { "E", "W" };

	double	latitude, longitude;

	/* AltosMapLoaderListener interfaces */
	public void loader_start(final int max) {
		SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					pbar.setMaximum(max);
					pbar.setValue(0);
					pbar.setString("");
					map.clear_marks();
					map.add_mark(latitude, longitude, AltosLib.ao_flight_boost);
				}
			});
	}

	public void loader_notify(final int cur, final int max, final String name) {
		SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					pbar.setValue(cur);
					pbar.setString(name);
				}
			});
	}

	public void loader_done(int max) {
		SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					pbar.setValue(0);
					pbar.setString("");
					load_button.setSelected(false);
					loading = false;
				}
			});
	}

	public void debug(String format, Object ... arguments) {
		if (AltosSerial.debug)
			System.out.printf(format, arguments);
	}


	private int all_types() {
		int all_types = 0;
		for (int t = AltosMap.maptype_hybrid; t <= AltosMap.maptype_terrain; t++)
			if (maptypes[t].isSelected())
				all_types |= (1 << t);
		return all_types;
	}

	public void itemStateChanged(ItemEvent e) {
		int		state = e.getStateChange();

		if (state == ItemEvent.SELECTED) {
			Object	o = e.getItem();
			if (o instanceof AltosLaunchSite) {
				AltosLaunchSite	site = (AltosLaunchSite) o;
				lat.set_value(site.latitude);
				lon.set_value(site.longitude);
			}
		}
	}

	public void actionPerformed(ActionEvent e) {
		String	cmd = e.getActionCommand();

		if (cmd.equals("close"))
			setVisible(false);

		if (cmd.equals("load")) {
			if (!loading) {
				try {
					latitude = lat.get_value();
					longitude = lon.get_value();
					int min_z = (Integer) min_zoom.getSelectedItem();
					int max_z = (Integer) max_zoom.getSelectedItem();
					if (max_z < min_z)
						max_z = min_z;
					Double r = (Double) radius.getSelectedItem();

					if (AltosPreferences.imperial_units())
						r = AltosConvert.miles_to_meters(r);
					else
						r = r * 1000;
					loading = true;

					new AltosMapLoader(map.map, this,
							   latitude, longitude,
							   min_z, max_z, r, all_types());

				} catch (ParseException pe) {
					load_button.setSelected(false);
				}
			}
		}
	}

	public void notify_launch_sites(final java.util.List<AltosLaunchSite> sites) {
		SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					int	i = 1;
					for (AltosLaunchSite site : sites) {
						site_list.insertItemAt(site, i);
						i++;
					}
				}
			});
	}

	public AltosUIMapPreloadNew(AltosUIFrame in_owner) {
		owner = in_owner;

		Container		pane = getContentPane();
		GridBagConstraints	c = new GridBagConstraints();
		Insets			i = new Insets(4,4,4,4);

		setTitle("AltOS Load Maps");

		pane.setLayout(new GridBagLayout());

		map = new AltosUIMapNew();

		c.fill = GridBagConstraints.BOTH;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 1;
		c.weighty = 1;

		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 10;
		c.anchor = GridBagConstraints.CENTER;

		pane.add(map, c);

		pbar = new JProgressBar();
		pbar.setMinimum(0);
		pbar.setMaximum(1);
		pbar.setValue(0);
		pbar.setString("");
		pbar.setStringPainted(true);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 1;
		c.weighty = 0;

		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth = 10;

		pane.add(pbar, c);

		site_list_label = new JLabel ("Known Launch Sites:");

		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 1;
		c.weighty = 0;

		c.gridx = 0;
		c.gridy = 2;
		c.gridwidth = 1;

		pane.add(site_list_label, c);

		site_list = new JComboBox<AltosLaunchSite>(new AltosLaunchSite[] { new AltosLaunchSite("Site List", 0, 0) });
		site_list.addItemListener(this);

		new AltosLaunchSites(this);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 1;
		c.weighty = 0;

		c.gridx = 1;
		c.gridy = 2;
		c.gridwidth = 1;

		pane.add(site_list, c);

		lat = new AltosUIMapPos(owner,
					"Latitude:",
					lat_hemi_names,
					37.167833333);
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 0;
		c.weighty = 0;

		c.gridx = 0;
		c.gridy = 3;
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.CENTER;

		pane.add(lat, c);

		lon = new AltosUIMapPos(owner,
					"Longitude:",
					lon_hemi_names,
					-97.73975);

		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 0;
		c.weighty = 0;

		c.gridx = 1;
		c.gridy = 3;
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.CENTER;

		pane.add(lon, c);

		load_button = new JToggleButton("Load Map");
		load_button.addActionListener(this);
		load_button.setActionCommand("load");

		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 1;
		c.weighty = 0;

		c.gridx = 0;
		c.gridy = 4;
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.CENTER;

		pane.add(load_button, c);

		close_button = new JButton("Close");
		close_button.addActionListener(this);
		close_button.setActionCommand("close");

		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = i;
		c.weightx = 1;
		c.weighty = 0;

		c.gridx = 1;
		c.gridy = 4;
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.CENTER;

		pane.add(close_button, c);

		JLabel	types_label = new JLabel("Map Types");
		c.gridx = 2;
		c.gridwidth = 2;
		c.gridy = 2;
		pane.add(types_label, c);

		c.gridwidth = 1;

		for (int type = AltosMap.maptype_hybrid; type <= AltosMap.maptype_terrain; type++) {
			maptypes[type] = new JCheckBox(AltosMap.maptype_labels[type],
						       type == AltosMap.maptype_hybrid);
			c.gridx = 2 + (type >> 1);
			c.fill = GridBagConstraints.HORIZONTAL;
			c.gridy = (type & 1) + 3;
			pane.add(maptypes[type], c);
		}

		JLabel	min_zoom_label = new JLabel("Minimum Zoom");
		c.gridx = 4;
		c.gridy = 2;
		pane.add(min_zoom_label, c);

		min_zoom = new JComboBox<Integer>(zooms);
		min_zoom.setSelectedItem(zooms[10]);
		min_zoom.setEditable(false);
		c.gridx = 5;
		c.gridy = 2;
		pane.add(min_zoom, c);

		JLabel	max_zoom_label = new JLabel("Maximum Zoom");
		c.gridx = 4;
		c.gridy = 3;
		pane.add(max_zoom_label, c);

		max_zoom = new JComboBox<Integer>(zooms);
		max_zoom.setSelectedItem(zooms[14]);
		max_zoom.setEditable(false);
		c.gridx = 5;
		c.gridy = 3;
		pane.add(max_zoom, c);

		JLabel radius_label = new JLabel(String.format("Map Radius (%s)",
							       AltosPreferences.imperial_units() ? "miles" : "km"));
		c.gridx = 4;
		c.gridy = 4;
		pane.add(radius_label, c);

		Double[]	radii;
		Double		radius_default;

		if (AltosPreferences.imperial_units())
			radii = radius_mi;
		else
			radii = radius_km;
		radius = new JComboBox<Double>(radii);
		radius.setSelectedItem(radii[2]);
		radius.setEditable(true);
		c.gridx = 5;
		c.gridy = 4;
		pane.add(radius, c);

		pack();
		setLocationRelativeTo(owner);
		setVisible(true);
	}
}
