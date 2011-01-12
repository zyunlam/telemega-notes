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

import java.lang.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import libaltosJNI.libaltos;
import libaltosJNI.altos_device;
import libaltosJNI.SWIGTYPE_p_altos_file;
import libaltosJNI.SWIGTYPE_p_altos_list;

class AltosEepromItem extends JPanel implements ActionListener {
	AltosEepromLog	log;
	JCheckBox	download;
	JCheckBox	delete;
	JLabel		label;

	public void actionPerformed(ActionEvent e) {
		System.out.printf("eeprom item action %s %d\n", e.getActionCommand(), e.getID());
		if (e.getSource() == download) {
			log.download = download.isSelected();
			System.out.printf("download set to %b\n", log.download);
		} else if (e.getSource() == delete) {
			log.delete = delete.isSelected();
			System.out.printf("delete set to %b\n", log.delete);
		}
	}

	public AltosEepromItem(AltosEepromLog in_log) {
		log = in_log;

		download = new JCheckBox("Download", log.download);
		download.addActionListener(this);
		add(download);
		delete = new JCheckBox("Delete", log.delete);
		delete.addActionListener(this);
		add(delete);
		label = new JLabel(String.format("Flight %d %4d-%02d-%02d",
						 log.flight, log.year, log.month, log.day));
		add(label);
	}
}

public class AltosEepromSelect extends JDialog implements ActionListener {
	private JList			list;
	private JFrame			frame;
	JButton				ok;
	JButton				cancel;
	boolean				success;

	/* Listen for events from our buttons */
	public void actionPerformed(ActionEvent e) {
		String	cmd = e.getActionCommand();

		if (cmd.equals("ok"))
			success = true;
		setVisible(false);
	}

	public boolean run() {
		success = false;
		setLocationRelativeTo(frame);
		setVisible(true);
		return success;
	}

	public AltosEepromSelect (JFrame in_frame,
				  AltosEepromList flights) {

		super(in_frame, String.format("Flight list for serial %d", flights.config_data.serial), true);
		frame = in_frame;

		JLabel	selectLabel = new JLabel("Select flights to download and/or delete", SwingConstants.CENTER);

		JPanel	labelPane = new JPanel();
		labelPane.setLayout(new BoxLayout(labelPane, BoxLayout.X_AXIS));
		labelPane.setBorder(BorderFactory.createEmptyBorder(10, 0, 10, 0));
		labelPane.add(Box.createHorizontalGlue());
		labelPane.add(selectLabel);
		labelPane.add(Box.createHorizontalGlue());

		JPanel	flightPane = new JPanel();
		flightPane.setLayout(new BoxLayout(flightPane, BoxLayout.Y_AXIS));
		flightPane.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		for (AltosEepromLog flight : flights) {
			flightPane.add(new AltosEepromItem(flight));
		}

		ok = new JButton("OK");
		ok.addActionListener(this);
		ok.setActionCommand("ok");

		cancel = new JButton("Cancel");
		cancel.addActionListener(this);
		cancel.setActionCommand("cancel");

		JPanel	buttonPane = new JPanel();
		buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.X_AXIS));
		buttonPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		buttonPane.add(Box.createHorizontalGlue());
		buttonPane.add(cancel);
		buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
		buttonPane.add(ok);

		Container contentPane = getContentPane();

		contentPane.add(labelPane, BorderLayout.PAGE_START);
		contentPane.add(flightPane, BorderLayout.CENTER);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);

		pack();
	}
}
