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

package org.altusmetrum.altosuilib_2;

import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import org.altusmetrum.altoslib_4.*;

class FreqEntry extends JMenuItem {
	AltosFrequency	frequency;

	public FreqEntry(AltosFrequency frequency) {
		super(frequency.toShortString());
		this.frequency = frequency;
	}
}

public class AltosFreqList extends JMenu implements ActionListener {

	String	product;
	int	serial;
	int	calibrate;

	AltosFrequency[]	frequencies = new AltosFrequency[0];

	int	selected = -1;

	LinkedList<ActionListener> action_listeners = new LinkedList<ActionListener>();

	public void addActionListener(ActionListener listener) {
		action_listeners.add(listener);
	}

	public void removeActionListener(ActionListener listener) {
		action_listeners.remove(listener);
	}

	public void actionPerformed(ActionEvent ev) {
		FreqEntry e = (FreqEntry) ev.getSource();
		set_selected(e.frequency);
		ActionEvent event = new ActionEvent(e.frequency, 0, "selected");
		for (ActionListener l : action_listeners)
			l.actionPerformed(event);
	}

	private void set_selected(AltosFrequency frequency) {
		for (int i = 0; i < frequencies.length; i++) {
			if (frequencies[i].frequency == frequency.frequency) {
				selected = i;
				String	new_text = String.format("Frequency: %7.3f MHz (%s) ▾",
								 frequency.frequency,
								 frequency.description);
				setText(new_text);
			}
		}
	}

	private AltosFrequency get_selected() {
		if (0 <= selected && selected < frequencies.length)
			return frequencies[selected];
		return null;
	}

	private void add(AltosFrequency add) {
		int insert;

		for (insert = 0; insert < frequencies.length; insert++) {
			if (frequencies[insert].frequency == add.frequency)
				return;
			if (add.frequency < frequencies[insert].frequency)
				break;
		}

		AltosFrequency[]	new_frequencies = new AltosFrequency[frequencies.length + 1];

		for (int before = 0; before < insert; before++)
			new_frequencies[before] = frequencies[before];
		new_frequencies[insert] = add;

		for (int after = insert; after < frequencies.length; after++)
			new_frequencies[after+1] = frequencies[after];

		frequencies = new_frequencies;

		FreqEntry	e = new FreqEntry(add);
		add(e, insert);
		e.addActionListener(this);
	}

	private void remove(AltosFrequency remove) {
		int delete;
		for (delete = 0; delete < frequencies.length; delete++) {
			if (frequencies[delete].frequency == remove.frequency)
				break;
			if (remove.frequency < frequencies[delete].frequency)
				return;
		}

		remove(delete);

		AltosFrequency[]	new_frequencies = new AltosFrequency[frequencies.length - 1];

		for (int before = 0; before < delete; before++)
			new_frequencies[before] = frequencies[before];

		for (int after = delete + 1; after < frequencies.length; after++)
			new_frequencies[after-1] = frequencies[after];
		frequencies = new_frequencies;
	}

	public void set_frequency(double new_frequency) {
		int i;

		if (new_frequency < 0) {
			setVisible(false);
			return;
		}

		for (i = 0; i < frequencies.length; i++) {
			AltosFrequency	f = frequencies[i];

			if (f.close(new_frequency)) {
				set_selected(f);
				return;
			}
		}

		String	description = String.format("%s serial %d", product, serial);
		AltosFrequency	frequency = new AltosFrequency(new_frequency, description);
		AltosUIPreferences.add_common_frequency(frequency);

		add(frequency);
		set_selected(frequency);
	}

	public void set_product(String new_product) {
		product = new_product;
	}

	public void set_serial(int new_serial) {
		serial = new_serial;
	}

	public double frequency() {
		AltosFrequency	f = get_selected();
		if (f != null)
			return f.frequency;
		return 434.550;
	}

	public AltosFreqList () {
		super();
		for (AltosFrequency frequency: AltosUIPreferences.common_frequencies())
			add(frequency);
		product = "Unknown";
		serial = 0;
	}

	public AltosFreqList(double in_frequency) {
		this();
		set_frequency(in_frequency);
	}
}
