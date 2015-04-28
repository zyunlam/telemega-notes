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

public class AltosBluetooth extends AltosDroidLink {

	// Debugging
	private static final String TAG = "AltosBluetooth";
	private static final boolean D = true;

	private ConnectThread    connect_thread = null;

	private BluetoothAdapter adapter;
	private BluetoothSocket  socket;
	private InputStream      input;
	private OutputStream     output;

	// Constructor
	public AltosBluetooth(BluetoothDevice device, Handler handler) {
		super(handler);
//		set_debug(D);
		adapter = BluetoothAdapter.getDefaultAdapter();
		this.handler = handler;

		create_socket(device);
		connect_thread = new ConnectThread();
		connect_thread.start();
	}

	void connected() {
		if (closed()) {
			if (D) Log.d(TAG, "connected after closed");
			return;
		}

		try {
			synchronized(this) {
				if (socket != null) {
					input = socket.getInputStream();
					output = socket.getOutputStream();
					super.connected();
				}
			}
		} catch (InterruptedException ie) {
			connect_failed();
		} catch (IOException io) {
			connect_failed();
		}
	}

	private void connect_failed() {
		if (closed()) {
			if (D) Log.d(TAG, "connect_failed after closed");
			return;
		}

		close_device();
		input = null;
		output = null;
		handler.obtainMessage(TelemetryService.MSG_CONNECT_FAILED, this).sendToTarget();
		if (D) Log.e(TAG, "ConnectThread: Failed to establish connection");
	}

	void close_device() {
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

	public void close() {
		super.close();
		input = null;
		output = null;
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
			close_device();
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

	private synchronized void wait_connected() throws InterruptedException, IOException {
		if (input == null && socket != null) {
			if (D) Log.d(TAG, "wait_connected...");
			wait();
			if (D) Log.d(TAG, "wait_connected done");
		}
		if (socket == null)
			throw new IOException();
	}

	int write(byte[] buffer, int len) {
		try {
			output.write(buffer, 0, len);
		} catch (IOException ie) {
			return -1;
		}
		return len;
	}

	int read(byte[] buffer, int len) {
		try {
			return input.read(buffer, 0, len);
		} catch (IOException ie) {
			return -1;
		}
	}

	// Stubs of required methods when extending AltosLink
	public boolean can_cancel_reply()   { return false; }
	public boolean show_reply_timeout() { return true; }
	public void hide_reply_timeout()    { }

}
