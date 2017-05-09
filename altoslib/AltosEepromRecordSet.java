/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

package org.altusmetrum.altoslib_11;

import java.io.*;
import java.util.*;

public class AltosEepromRecordSet implements Iterable<AltosState> {
	AltosEepromNew			eeprom;
	TreeSet<AltosEepromRecord>	ordered;
	AltosState			start_state;

	class RecordIterator implements Iterator<AltosState> {
		Iterator<AltosEepromRecord> riterator;
		AltosState state;
		boolean started;

		public boolean hasNext() {
			return state == null || riterator.hasNext();
		}

		public AltosState next() {
			if (state == null)
				state = start_state.clone();
			else {
				state = state.clone();
				AltosEepromRecord	r = riterator.next();
				r.update_state(state);
			}
			return state;
		}

		public RecordIterator() {
			riterator = ordered.iterator();
			state = null;
		}
	}

	public Iterator<AltosState> iterator() {
		return new RecordIterator();
	}

	public AltosEepromRecordSet(AltosEepromNew eeprom) {
		this.eeprom = eeprom;

		AltosConfigData 	config_data = eeprom.config_data();

		AltosEepromRecord	record = null;

		switch (config_data.log_format) {
		case AltosLib.AO_LOG_FORMAT_FULL:
			record = new AltosEepromRecordFull(eeprom);
			break;
		case AltosLib.AO_LOG_FORMAT_TINY:
			record = new AltosEepromRecordTiny(eeprom);
			break;
		case AltosLib.AO_LOG_FORMAT_TELEMETRY:
		case AltosLib.AO_LOG_FORMAT_TELESCIENCE:
		case AltosLib.AO_LOG_FORMAT_TELEMEGA:
		case AltosLib.AO_LOG_FORMAT_TELEMEGA_OLD:
			record = new AltosEepromRecordMega(eeprom);
			break;
		case AltosLib.AO_LOG_FORMAT_TELEMETRUM:
			record = new AltosEepromRecordMetrum(eeprom);
			break;
		case AltosLib.AO_LOG_FORMAT_TELEMINI2:
		case AltosLib.AO_LOG_FORMAT_TELEMINI3:
		case AltosLib.AO_LOG_FORMAT_EASYMINI:
			record = new AltosEepromRecordMini(eeprom);
			break;
		case AltosLib.AO_LOG_FORMAT_TELEGPS:
			record = new AltosEepromRecordGps(eeprom);
			break;
		case AltosLib.AO_LOG_FORMAT_TELEFIRETWO:
			record = new AltosEepromRecordFireTwo(eeprom);
			break;
		}

		if (record == null) {
			System.out.printf("failed to parse log format %d\n", config_data.log_format);
			return;
		}
		ordered = new TreeSet<AltosEepromRecord>();
		int	tick = 0;
		boolean first = true;

		start_state = new AltosState();
		start_state.set_config_data(record.eeprom.config_data());

		for (;;) {
			int	t = record.tick();

			if (first) {
				tick = t;
				first = false;
			} else {
				while (t < tick - 32767)
					t += 65536;
				tick = t;
			}
			record.wide_tick = tick;
			ordered.add(record);
			if (!record.hasNext())
				break;
			record = record.next();
		}
	}

	public AltosEepromRecordSet(Reader input) throws IOException {
		this(new AltosEepromNew(input));
	}
}
