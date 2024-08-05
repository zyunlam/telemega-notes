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

public class AltosFlightPyroTable extends JComponent
	implements AltosFontListener, AltosUnitsListener
{
	GridBagLayout	layout;
	AltosPyro[]	pyros;
	int		npyro;
	FlightPyro[]	flight_pyros;

	class FlightPyro implements AltosFontListener {
		JLabel		label;
		JTextField[]	text_fields;

		public void font_size_changed(int font_size) {
			label.setFont(AltosUILib.label_font);
			for (int i = 0; i < text_fields.length; i++)
				text_fields[i].setFont(AltosUILib.value_font);
		}

		public void set_value(int y, int p, String value) {
			GridBagConstraints	c = new GridBagConstraints();
			c.insets = new Insets(AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad);
			c.weighty = 1;
			JTextField	text_field;

			if (text_fields[p] == null) {
				text_field = new JTextField(value);
				text_field.setEditable(false);
				text_field.setFont(AltosUILib.value_font);
				text_field.setHorizontalAlignment(SwingConstants.RIGHT);
				c.gridx = p+1; c.gridy = y;
				c.anchor = GridBagConstraints.EAST;
				c.fill = GridBagConstraints.BOTH;
				c.weightx = 1;
				layout.setConstraints(text_field, c);
				add(text_field);
				text_fields[p] = text_field;
			} else {
				text_fields[p].setText(value);
			}
		}

		public void set_label(String text) {
			label.setText(text);
		}

		public FlightPyro(GridBagLayout layout, int y, String label_text, int npyro) {
			GridBagConstraints	c = new GridBagConstraints();
			c.insets = new Insets(AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad, AltosUILib.tab_elt_pad);
			c.weighty = 1;

			if (label_text != null) {
				label = new JLabel(label_text);
				label.setFont(AltosUILib.label_font);
				label.setHorizontalAlignment(SwingConstants.LEFT);
				c.gridx = 0; c.gridy = y;
				c.anchor = GridBagConstraints.WEST;
				c.fill = GridBagConstraints.VERTICAL;
				c.weightx = 0;
				layout.setConstraints(label, c);
				add(label);
			}

			text_fields = new JTextField[npyro];
		}
	}

	public void font_size_changed(int font_size) {
		for (int i = 0; i < flight_pyros.length; i++)
			flight_pyros[i].font_size_changed(font_size);
	}

	public void set_pyros() {
		int nrow = 1;
		for (int flag = 1; flag < AltosPyro.pyro_all; flag <<= 1) {
			if ((AltosPyro.pyro_all_useful & flag) != 0) {
				for (int p = 0; p < npyro; p++) {
					if ((pyros[p].flags & flag) != 0) {
						String text;
						double	value = pyros[p].get_value(flag);
						if ((flag & AltosPyro.pyro_state_value) != 0) {
							text = AltosLib.state_name_capital((int) value);
						} else {
							double	scale = AltosPyro.pyro_to_scale(flag);
							double 	unit_value = value;
							AltosUnits units = AltosPyro.pyro_to_units(flag);
							if (units != null)
								unit_value = units.parse_value(value);
							String	format;
							if (scale >= 100)
								format = "%6.2f";
							else if (scale >= 10)
								format = "%6.1f";
							else
								format = "%6.0f";
							text = String.format(format, unit_value);
						}
						flight_pyros[nrow].set_value(nrow, p, text);
					}
				}
				nrow++;
			}
		}
	}

	public void units_changed(boolean imperial_units) {
		System.out.printf("units changed\n");
		int nrow = 1;
		for (int flag = 1; flag < AltosPyro.pyro_all; flag <<= 1) {
			if ((AltosPyro.pyro_all_useful & flag) != 0) {
				String name = AltosPyro.pyro_to_name(flag);
				flight_pyros[nrow].set_label(name);
			}
		}
		set_pyros();
	}

	public void tell_closing() {
		AltosUIPreferences.unregister_font_listener(this);
	}

	public AltosFlightPyroTable(AltosPyro[] pyros, int npyro) {
		layout = new GridBagLayout();

		int nrow = 0;

		for (int flag = 1; flag < AltosPyro.pyro_all; flag <<= 1) {
			if ((AltosPyro.pyro_all_useful & flag) != 0) {
				nrow++;
			}
		}

		flight_pyros = new FlightPyro[nrow + 1];

		nrow = 0;

		flight_pyros[0] = new FlightPyro(layout, 0, null, npyro);

		for (int p = 0; p < npyro; p++) {
			flight_pyros[0].set_value(0, p, String.format("Channel %c", 'A' + p));
		}
		nrow++;
		for (int flag = 1; flag < AltosPyro.pyro_all; flag <<= 1) {
			if ((AltosPyro.pyro_all_useful & flag) != 0) {
				String name = AltosPyro.pyro_to_name(flag);
				flight_pyros[nrow] = new FlightPyro(layout, nrow, name, npyro);
				nrow++;
			}
		}


		this.pyros = pyros;
		this.npyro = npyro;

		set_pyros();

		setLayout(layout);
		AltosUIPreferences.register_font_listener(this);
	}
}
