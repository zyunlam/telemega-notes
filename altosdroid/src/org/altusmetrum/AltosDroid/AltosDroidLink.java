/*
 * Copyright © 2015 Keith Packard <keithp@keithp.com>
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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;

import android.os.Handler;
import android.util.Log;

import org.altusmetrum.altoslib_7.*;

public abstract class AltosDroidLink extends AltosLink {

	// Debugging
	private static final String TAG = "AltosDroidLink";
	private static final boolean D = true;

	Handler		handler;

	Thread          input_thread   = null;

	public double frequency() {
		return frequency;
	}

	public int telemetry_rate() {
		return telemetry_rate;
	}

	public void save_frequency() {
		AltosPreferences.set_frequency(0, frequency);
	}

	public void save_telemetry_rate() {
		AltosPreferences.set_telemetry_rate(0, telemetry_rate);
	}

	Object closed_lock = new Object();
	boolean closing = false;
	boolean closed = false;

	public boolean closed() {
		synchronized(closed_lock) {
			return closing;
		}
	}

	void connected() throws InterruptedException {
		input_thread = new Thread(this);
		input_thread.start();

		// Configure the newly connected device for telemetry
		print("~\nE 0\n");
		set_monitor(false);
		if (D) Log.d(TAG, "ConnectThread: connected");

		/* Let TelemetryService know we're connected
		 */
		handler.obtainMessage(TelemetryService.MSG_CONNECTED, this).sendToTarget();

		/* Notify other waiting threads that we're connected now
		 */
		notifyAll();
	}

	public void closing() {
		synchronized(closed_lock) {
			if (D) Log.d(TAG, "Marked closing true");
			closing = true;
		}
	}

	private boolean actually_closed() {
		synchronized(closed_lock) {
			return closed;
		}
	}

	abstract void close_device();

	public void close() {
		if (D) Log.d(TAG, "close(): begin");

		closing();

		flush_output();

		synchronized (closed_lock) {
			if (D) Log.d(TAG, "Marked closed true");
			closed = true;
		}

		close_device();

		synchronized(this) {

			if (input_thread != null) {
				if (D) Log.d(TAG, "close(): stopping input_thread");
				try {
					if (D) Log.d(TAG, "close(): input_thread.interrupt().....");
					input_thread.interrupt();
					if (D) Log.d(TAG, "close(): input_thread.join().....");
					input_thread.join();
				} catch (Exception e) {}
				input_thread = null;
			}
			notifyAll();
		}
	}

	abstract int write(byte[] buffer, int len);

	abstract int read(byte[] buffer, int len);

	private static final int buffer_size = 64;

	private byte[] in_buffer = new byte[buffer_size];
	private byte[] out_buffer = new byte[buffer_size];
	private int buffer_len = 0;
	private int buffer_off = 0;
	private int out_buffer_off = 0;

	private byte[] debug_chars = new byte[buffer_size];
	private int debug_off;

	private void debug_input(byte b) {
		if (b == '\n') {
			Log.d(TAG, "            " + new String(debug_chars, 0, debug_off));
			debug_off = 0;
		} else {
			if (debug_off < buffer_size)
				debug_chars[debug_off++] = b;
		}
	}

	private void disconnected() {
		if (closed()) {
			if (D) Log.d(TAG, "disconnected after closed");
			return;
		}

		if (D) Log.d(TAG, "Sending disconnected message");
		handler.obtainMessage(TelemetryService.MSG_DISCONNECTED, this).sendToTarget();
	}

	public int getchar() {

		if (actually_closed())
			return ERROR;

		while (buffer_off == buffer_len) {
			buffer_len = read(in_buffer, buffer_size);
			if (buffer_len < 0) {
				Log.d(TAG, "ERROR returned from getchar()");
				disconnected();
				return ERROR;
			}
			buffer_off = 0;
		}
		if (D)
			debug_input(in_buffer[buffer_off]);
		return in_buffer[buffer_off++];
	}

	public void flush_output() {
		super.flush_output();

		if (actually_closed()) {
			out_buffer_off = 0;
			return;
		}

		while (out_buffer_off != 0) {
			int	sent = write(out_buffer, out_buffer_off);

			if (sent <= 0) {
				Log.d(TAG, "flush_output() failed");
				out_buffer_off = 0;
				break;
			}

			if (sent < out_buffer_off)
				System.arraycopy(out_buffer, 0, out_buffer, sent, out_buffer_off - sent);

			out_buffer_off -= sent;
		}
	}

	public void putchar(byte c) {
		out_buffer[out_buffer_off++] = c;
		if (out_buffer_off == buffer_size)
			flush_output();
	}

	public void print(String data) {
		byte[] bytes = data.getBytes();
		if (D) Log.d(TAG, "print(): begin");
		for (byte b : bytes)
			putchar(b);
		if (D) Log.d(TAG, "print(): Wrote bytes: '" + data.replace('\n', '\\') + "'");
	}

	public AltosDroidLink(Handler handler) {
		this.handler = handler;
	}
}
