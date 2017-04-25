/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

public class AltosEepromFireTwo extends AltosEeprom {
	public static final int	record_length = 32;

	public int record_length() { return record_length; }

	/* AO_LOG_FLIGHT elements */
	public int flight() { return data16(0); }
	public int idle_pres() { return data16(2); }
	public int idle_thrust() { return data16(4); }

	/* AO_LOG_STATE elements */
	public int state() { return data16(0); }
	public int reason() { return data16(2); }

	/* AO_LOG_SENSOR elements */
	public int pres() { return data16(0); }
	public int thrust() { return data16(2); }
	public int temp(int i) { return data16(4+i*2); }

	public AltosEepromFireTwo (AltosEepromChunk chunk, int start) throws ParseException {
		parse_chunk(chunk, start);
	}

	private static final double r_above = 5600.0;
	private static final double r_below = 10000.0;
	private static final double v_adc = 3.3;

	private static double
	firetwo_adc(int raw) {
		return raw / 4095.0;
	}

	private static double
	adc_to_pa(int adc) {

		/* raw adc to processor voltage, then back through the
		 * voltage divider to the sensor voltage
		 */

		double	v = firetwo_adc(adc) * v_adc * (r_above + r_below) / r_below;

		/* Bound to ranges provided in sensor */
		if (v < 0.5) v = 0.5;
		if (v > 4.5) v = 4.5;

		double	psi = (v - 0.5) / 4.0 * 1600.0;
		return AltosConvert.psi_to_pa(psi);
	}

	public void update_state(AltosState state) {
		super.update_state(state);

		switch (cmd) {
		case AltosLib.AO_LOG_FLIGHT:
			state.set_flight(flight());
			state.set_ground_pressure(adc_to_pa(idle_pres()));
			break;
		case AltosLib.AO_LOG_STATE:
			state.set_state(state());
			break;
		case AltosLib.AO_LOG_SENSOR:
			state.set_pressure(adc_to_pa(pres()));
			break;
		}
	}

	public AltosEepromFireTwo (String line) {
		parse_string(line);
	}

	static public LinkedList<AltosEeprom> read(FileInputStream input) {
		LinkedList<AltosEeprom> firetwos = new LinkedList<AltosEeprom>();

		for (;;) {
			try {
				String line = AltosLib.gets(input);
				if (line == null)
					break;
				try {
					AltosEepromFireTwo firetwo = new AltosEepromFireTwo(line);

					if (firetwo.cmd != AltosLib.AO_LOG_INVALID)
						firetwos.add(firetwo);
				} catch (Exception e) {
					System.out.printf ("exception\n");
				}
			} catch (IOException ie) {
				break;
			}
		}

		return firetwos;
	}
}
