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

package altosui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;
import javax.swing.event.*;
import java.io.*;
import java.util.*;
import java.text.*;
import java.util.prefs.*;
import java.util.concurrent.LinkedBlockingQueue;

public class AltosTerra
	extends JDialog
	implements ActionListener
{
	JFrame		owner;
	JTextArea	text_area;
	Container	pane;
	JButton		prev, next, enter;

	static final int	info_tab = 1;
	static final int	pad_tab = 2;
	static final int	ascent_tab = 3;
	static final int	descent_tab = 4;
	static final int	landing_tab = 5;

	int		current_tab;
	int		displayed_tab;
	AltosState	current_state;

	public void actionPerformed(ActionEvent e) {
		String cmd = e.getActionCommand();
		if (cmd.equals("prev")) {
			displayed_tab = displayed_tab - 1;
			if (displayed_tab < info_tab)
				displayed_tab = landing_tab;
			display();
		}
		if (cmd.equals("next")) {
			displayed_tab = displayed_tab + 1;
			if (displayed_tab > landing_tab)
				displayed_tab = info_tab;
			display();
		}
	}

	int which_tab(AltosState state) {
		if (state.state < Altos.ao_flight_boost)
			return pad_tab;
		if (state.state <= Altos.ao_flight_coast)
			return ascent_tab;
		if (state.state <= Altos.ao_flight_main)
			return descent_tab;
		return landing_tab;
	}

	String[]	lines = { "", "" };

	public void setLine(int line, String format, Object... args) {
		lines[line] = String.format(format, args);
		String	result = lines[0].concat("\n").concat(lines[1]);
		text_area.setText(result);
	}

	public int state_char(AltosState state) {
		switch (state.state) {
		case Altos.ao_flight_startup: return 'S';
		case Altos.ao_flight_idle: return 'I';
		case Altos.ao_flight_pad: return 'P';
		case Altos.ao_flight_boost: return 'B';
		case Altos.ao_flight_fast: return 'F';
		case Altos.ao_flight_coast: return 'C';
		case Altos.ao_flight_drogue: return 'D';
		case Altos.ao_flight_main: return 'M';
		case Altos.ao_flight_landed: return 'L';
		default:
		case Altos.ao_flight_invalid: return '*';
		}
	}

	int boolchar(boolean b) {
		if (b)
			return '+';
		return ' ';
	}

	String pos(double p, String pos, String neg) {
		String	h = pos;
		if (p < 0) {
			h = neg;
			p = -p;
		}
		int deg = (int) Math.floor(p);
		double min = (p - Math.floor(p)) * 60.0;
		return String.format("%s%3d°%9.6f'", h, deg, min);
	}

	public void display() {
		switch (displayed_tab) {
		default:
		case info_tab:
			setLine(0, "%-7sN %-5d %c",
				current_state.data.callsign,
				current_state.data.serial,
				state_char(current_state));
			setLine(1, "F %-4d RSSI %3d",
				current_state.data.flight,
				current_state.data.rssi);
			break;
		case pad_tab:
			setLine(0, "D%c  M%c  B%c  G%c %c",
				boolchar(current_state.drogue_sense > 3.2),
				boolchar(current_state.main_sense > 3.2),
				boolchar(current_state.battery > 3.7),
				boolchar(current_state.gps.locked),
				state_char(current_state));
			setLine(1, "SAT %2d RSSI %3d",
				current_state.gps.nsat,
				current_state.data.rssi);
			break;
		case ascent_tab:
			setLine(0, "S:%5d S⌈%5d%c",
				(int) Math.floor(current_state.speed + 0.5),
				(int) Math.floor(current_state.max_speed + 0.5),
				state_char(current_state));
			setLine(1, "H:%5d H⌈%5d",
				(int) Math.floor(current_state.height + 0.5),
				(int) Math.floor(current_state.max_height + 0.5));
			break;
		case descent_tab:
			setLine(0, "→%5d  ↑%5d %c",
				(int) Math.floor (current_state.from_pad.bearing + 0.5),
				(int) Math.floor (current_state.elevation + 0.5),
				state_char(current_state));
			setLine(1, "H%5d  S%5d",
				(int) Math.floor(current_state.height + 0.5),
				(int) Math.floor(current_state.baro_speed + 0.5));
			break;
		case landing_tab:
			setLine(0, "%s%c", pos(current_state.gps.lat, "N", "S"),
				state_char(current_state));
			setLine(1, "%s", pos(current_state.gps.lon, "E", "W"));
		}
	}

	public void display(AltosState state) {
		int	tab = which_tab(state);
		if (tab != current_tab)
			current_tab = displayed_tab = tab;
		current_state = state;
		display();
	}

	public AltosTerra(JFrame in_owner) {
		super(in_owner, "AltosTerra", false);

		GridBagConstraints	c;

		Insets	insets = new Insets(4,4,4,4);

		owner = in_owner;

		pane = getContentPane();
		pane.setLayout(new GridBagLayout());
		
		c = new GridBagConstraints();
		c.insets = insets;
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.WEST;

		text_area = new JTextArea(2, 16);
		text_area.setFont(new Font("Monospaced", Font.PLAIN, 22));
		text_area.setEditable(false);

		/* Text area */
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.gridheight = 3;
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.CENTER;
		pane.add(text_area, c);

		/* Prev button */
		prev = new JButton("Prev");
		prev.addActionListener(this);
		prev.setActionCommand("prev");
		c.gridx = 1;
		c.gridy = 0;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.fill = GridBagConstraints.BOTH;
		c.anchor = GridBagConstraints.CENTER;
		pane.add(prev, c);

		/* Next button */
		next = new JButton("Next");
		next.addActionListener(this);
		next.setActionCommand("next");
		c.gridx = 1;
		c.gridy = 1;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.fill = GridBagConstraints.BOTH;
		c.anchor = GridBagConstraints.CENTER;
		pane.add(next, c);

		/* Enter button */
		enter = new JButton("Enter");
		enter.addActionListener(this);
		enter.setActionCommand("enter");
		c.gridx = 1;
		c.gridy = 2;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.fill = GridBagConstraints.BOTH;
		c.anchor = GridBagConstraints.CENTER;
		pane.add(enter, c);

		pack();
	}
}
