/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_2;

import org.altusmetrum.altosuilib_1.*;

public interface AltosEepromMonitor {

	public void set_states(int min_state, int max_state);

	public void set_value(String in_state_name, int in_state, int in_state_block, int in_block);

	public void set_serial(int in_serial);

	public void set_flight(int in_flight);

	public void set_filename(String in_file);

	public void set_thread(Thread eeprom_thread);

	public void start();

	public void done();

	public void reset();
}
