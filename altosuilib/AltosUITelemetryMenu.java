/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altosuilib_3;

import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import org.altusmetrum.altoslib_5.*;

class TelemetryMenuItem extends JMenuItem {
	public int	telemetry;

	public TelemetryMenuItem (int telemetry) {
		super(AltosLib.telemetry_name(telemetry));
		this.telemetry = telemetry;
	}
}

public class AltosUITelemetryMenu extends JMenu implements ActionListener {
	TelemetryMenuItem	selected = null;

	public int get_selected() {
		if (selected == null)
			return AltosLib.ao_telemetry_off;
		return selected.telemetry;
	}

	public void set_selected(int telemetry) {
		for (int i = 0; i < getItemCount(); i++) {
			TelemetryMenuItem	item = (TelemetryMenuItem) getItem(i);
			if (item.telemetry == telemetry) {
				selected = item;
				String new_text = String.format("Format: %s ▾", AltosLib.telemetry_name(telemetry));
				setText(new_text);
				break;
			}
		}
	}

	private LinkedList<ActionListener> action_listeners = new LinkedList<ActionListener>();

	public void addActionListener(ActionListener l) {
		action_listeners.add(l);
	}

	public void removeActionListener(ActionListener l) {
		action_listeners.remove(l);
	}

	public void actionPerformed(ActionEvent e) {
		TelemetryMenuItem item = (TelemetryMenuItem) e.getSource();
		set_selected(item.telemetry);
		ActionEvent my_e = new ActionEvent(selected, 0, "selected");
		for (ActionListener l : action_listeners)
			l.actionPerformed(my_e);
	}

	public AltosUITelemetryMenu(int serial) {
		super();
		for (int i = AltosLib.ao_telemetry_min; i <= AltosLib.ao_telemetry_max; i++) {
			TelemetryMenuItem	item = new TelemetryMenuItem(i);

			item.addActionListener(this);
			add(item);
		}

		int telemetry = AltosPreferences.telemetry(serial);
		if (telemetry < AltosLib.ao_telemetry_min || AltosLib.ao_telemetry_max < telemetry)
			telemetry = AltosLib.ao_telemetry_standard;
		set_selected(telemetry);
	}
}

