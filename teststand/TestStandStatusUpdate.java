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

import java.awt.event.*;
import org.altusmetrum.altoslib_11.*;

public class TestStandStatusUpdate implements ActionListener {

	public AltosState		saved_state;
	public AltosListenerState	saved_listener_state;
	TestStandStatus			status;

	public void actionPerformed (ActionEvent e) {
		if (saved_state != null) {
			if (saved_listener_state == null)
				saved_listener_state = new AltosListenerState();
			status.show(saved_state, saved_listener_state);
		}
	}

	public TestStandStatusUpdate (TestStandStatus in_status) {
		status = in_status;
	}
}

