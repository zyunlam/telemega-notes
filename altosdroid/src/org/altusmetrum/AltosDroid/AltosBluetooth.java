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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
//import android.os.Bundle;
import android.os.Handler;
//import android.os.Message;
import android.util.Log;

import org.altusmetrum.altoslib_6.*;

public class AltosBluetooth extends AltosLink {

	// Debugging
	private static final String TAG = "AltosBluetooth";
	private static final boolean D = true;

	private ConnectThread    connect_thread = null;
	private Thread           input_thread   = null;

	private Handler          handler;

	private BluetoothAdapter adapter;
	private BluetoothSocket  socket;
	private InputStream      input;
	private OutputStream     output;

	// Constructor
	public AltosBluetooth(BluetoothDevice device, Handler handler) {
//		set_debug(D);
		adapter = BluetoothAdapter.getDefaultAdapter();
		this.handler = handler;

		create_socket(device);
		connect_thread = new ConnectThread();
		connect_thread.start();
	}

	private Object closed_lock = new Object();
	private boolean closed = false;

	private boolean closed() {
		synchronized(closed_lock) {
			return closed;
		}
	}

	private void connected() {
		if (closed()) {
			if (D) Log.d(TAG, "connected after closed");
			return;
		}

		try {
			synchronized(this) {
				if (socket != null) {
					input = socket.getInputStream();
					output = socket.getOutputStream();

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
			}
		} catch (IOException io) {
			connect_failed();
		}
	}

	private void connect_failed() {
		if (closed()) {
			if (D) Log.d(TAG, "connect_failed after closed");
			return;
		}

		close_socket();
		input = null;
		output = null;
		handler.obtainMessage(TelemetryService.MSG_CONNECT_FAILED, this).sendToTarget();
		if (D) Log.e(TAG, "ConnectThread: Failed to establish connection");
	}

	private void disconnected() {
		if (closed()) {
			if (D) Log.d(TAG, "disconnected after closed");
			return;
		}

		if (D) Log.d(TAG, "Sending disconnected message");
		handler.obtainMessage(TelemetryService.MSG_DISCONNECTED, this).sendToTarget();
	}

	private void close_socket() {
		BluetoothSocket	tmp_socket;

		synchronized(this) {
			tmp_socket = socket;
			socket = null;
		}

		if (tmp_socket != null) {
			try {
				tmp_socket.close();
			} catch (IOException e) {
				if (D) Log.e(TAG, "close_socket failed");
			}
		}
	}

	private final UUID SPP_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");

	private void create_socket(BluetoothDevice  device) {

		BluetoothSocket tmp_socket = null;

		try {
			tmp_socket = device.createInsecureRfcommSocketToServiceRecord(SPP_UUID);
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (socket != null) {
			if (D) Log.d(TAG, String.format("Socket already allocated %s", socket.toString()));
			close_socket();
		}
		synchronized (this) {
			socket = tmp_socket;
		}
	}

	private class ConnectThread extends Thread {

		public void run() {
			if (D) Log.d(TAG, "ConnectThread: BEGIN");
			setName("ConnectThread");

			// Always cancel discovery because it will slow down a connection
			try {
				adapter.cancelDiscovery();
			} catch (Exception e) {
				if (D) Log.d(TAG, String.format("cancelDiscovery exception %s", e.toString()));
			}

			BluetoothSocket	local_socket = null;

			synchronized (AltosBluetooth.this) {
				if (!closed())
					local_socket = socket;
			}

			if (local_socket != null) {
				try {
					// Make a connection to the BluetoothSocket
					// This is a blocking call and will only return on a
					// successful connection or an exception
					local_socket.connect();
				} catch (IOException e) {
					if (D) Log.d(TAG, String.format("Connect exception %s", e.toString()));
					local_socket = null;
				}
			}

			if (local_socket != null) {
				connected();
			} else {
				connect_failed();
			}

			if (D) Log.d(TAG, "ConnectThread: completed");
		}
	}

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

	private synchronized void wait_connected() throws InterruptedException, IOException {
		if (input == null && socket != null) {
			if (D) Log.d(TAG, "wait_connected...");
			wait();
			if (D) Log.d(TAG, "wait_connected done");
		}
		if (socket == null)
			throw new IOException();
	}

	public void print(String data) {
		byte[] bytes = data.getBytes();
		if (D) Log.d(TAG, "print(): begin");
		try {
			wait_connected();
			output.write(bytes);
			if (D) Log.d(TAG, "print(): Wrote bytes: '" + data.replace('\n', '\\') + "'");
		} catch (IOException e) {
			disconnected();
		} catch (InterruptedException e) {
			disconnected();
		}
	}

	public void putchar(byte c) {
		byte[] bytes = { c };
		if (D) Log.d(TAG, "print(): begin");
		try {
			wait_connected();
			output.write(bytes);
			if (D) Log.d(TAG, "print(): Wrote byte: '" + c + "'");
		} catch (IOException e) {
			disconnected();
		} catch (InterruptedException e) {
			disconnected();
		}
	}

	private static final int buffer_size = 1024;

	private byte[] buffer = new byte[buffer_size];
	private int buffer_len = 0;
	private int buffer_off = 0;

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

	public int getchar() {
		while (buffer_off == buffer_len) {
			try {
				wait_connected();
				buffer_len = input.read(buffer);
				buffer_off = 0;
			} catch (IOException e) {
				if (D) Log.d(TAG, "getchar IOException");
				disconnected();
				return AltosLink.ERROR;
			} catch (java.lang.InterruptedException e) {
				if (D) Log.d(TAG, "getchar Interrupted");
				disconnected();
				return AltosLink.ERROR;
			}
		}
		if (D)
			debug_input(buffer[buffer_off]);
		return buffer[buffer_off++];
	}

	public void closing() {
		synchronized(closed_lock) {
			if (D) Log.d(TAG, "Marked closed true");
			closed = true;
		}
	}


	public void close() {
		if (D) Log.d(TAG, "close(): begin");

		closing();

		close_socket();

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
			input = null;
			output = null;
			notifyAll();
		}
	}

	//public void flush_output() { super.flush_output(); }

	// Stubs of required methods when extending AltosLink
	public boolean can_cancel_reply()   { return false; }
	public boolean show_reply_timeout() { return true; }
	public void hide_reply_timeout()    { }

}
