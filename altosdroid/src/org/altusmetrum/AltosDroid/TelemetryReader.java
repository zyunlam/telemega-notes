/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
 * Copyright © 2012 Mike Beattie <mike@ethernal.org>
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


package org.altusmetrum.AltosDroid;

import java.text.*;
import java.io.*;
import java.util.concurrent.*;
import android.os.Handler;

import org.altusmetrum.altoslib_7.*;


public class TelemetryReader extends Thread {

	int         crc_errors;

	Handler     handler;

	AltosLink   link;
	AltosState  state = null;

	LinkedBlockingQueue<AltosLine> telemQueue;

	public AltosState read() throws ParseException, AltosCRCException, InterruptedException, IOException {
		AltosLine l = telemQueue.take();
		if (l.line == null)
			throw new IOException("IO error");
		AltosTelemetry telem = AltosTelemetryLegacy.parse(l.line);
		if (state == null)
			state = new AltosState();
		else
			state = state.clone();
		telem.update_state(state);
		return state;
	}

	public void close() {
		state = null;
		link.remove_monitor(telemQueue);
		link = null;
		telemQueue.clear();
		telemQueue = null;
	}

	public void run() {
		AltosState  state = null;

		try {
			AltosDebug.debug("starting loop");
			while (telemQueue != null) {
				try {
					state = read();
					handler.obtainMessage(TelemetryService.MSG_TELEMETRY, state).sendToTarget();
				} catch (ParseException pp) {
					AltosDebug.error("Parse error: %d \"%s\"", pp.getErrorOffset(), pp.getMessage());
				} catch (AltosCRCException ce) {
					++crc_errors;
					handler.obtainMessage(TelemetryService.MSG_CRC_ERROR, new Integer(crc_errors)).sendToTarget();
				}
			}
		} catch (InterruptedException ee) {
		} catch (IOException ie) {
			AltosDebug.error("IO exception in telemetry reader");
			handler.obtainMessage(TelemetryService.MSG_DISCONNECTED, link).sendToTarget();
		} finally {
			close();
		}
	}

	public TelemetryReader (AltosLink in_link, Handler in_handler, AltosState in_state) {
		AltosDebug.debug("connected TelemetryReader create started");
		link    = in_link;
		handler = in_handler;

		state = in_state;
		telemQueue = new LinkedBlockingQueue<AltosLine>();
		link.add_monitor(telemQueue);
		link.set_telemetry(AltosLib.ao_telemetry_standard);

		AltosDebug.debug("connected TelemetryReader created");
	}
}
