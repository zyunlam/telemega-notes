/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altoslib_11;

import java.io.*;
import java.util.*;
import java.text.*;

public class AltosEepromFile extends AltosStateIterable implements AltosRecordSet {

	AltosEepromRecordSet	set;

	public AltosConfigData config_data() {
		return set.eeprom.config_data();
	}

	public void write_comments(PrintStream out) {
	}

	public void write(PrintStream out) {
		out.printf("%s\n", set.eeprom.toString());
	}

	public AltosEepromFile(Reader input) throws IOException {
		set = new AltosEepromRecordSet(input);
	}

	public Iterator<AltosState> iterator() {
		return set.iterator();
	}

	public void capture_series(AltosFlightSeries series) {
		set.capture_series(series);
	}
}
