/*
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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.concurrent.TimeoutException;
import java.util.Timer;
import java.util.TimerTask;

import android.app.Notification;
//import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothAdapter;
import android.content.Intent;
import android.content.Context;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Handler;
import android.os.Message;
import android.os.Messenger;
import android.os.RemoteException;
import android.os.Looper;
import android.util.Log;
import android.widget.Toast;
import android.location.Location;
import android.location.LocationManager;
import android.location.LocationListener;
import android.location.Criteria;

import org.altusmetrum.altoslib_6.*;


public class TelemetryService extends Service implements LocationListener {

	private static final String TAG = "TelemetryService";
	private static final boolean D = true;

	static final int MSG_REGISTER_CLIENT   = 1;
	static final int MSG_UNREGISTER_CLIENT = 2;
	static final int MSG_CONNECT           = 3;
	static final int MSG_CONNECTED         = 4;
	static final int MSG_CONNECT_FAILED    = 5;
	static final int MSG_DISCONNECTED      = 6;
	static final int MSG_TELEMETRY         = 7;
	static final int MSG_SETFREQUENCY      = 8;
	static final int MSG_CRC_ERROR	       = 9;
	static final int MSG_SETBAUD	       = 10;

	// Unique Identification Number for the Notification.
	// We use it on Notification start, and to cancel it.
	private int NOTIFICATION = R.string.telemetry_service_label;
	//private NotificationManager mNM;

	ArrayList<Messenger> clients = new ArrayList<Messenger>(); // Keeps track of all current registered clients.
	final Handler   handler   = new IncomingHandler(this);
	final Messenger messenger = new Messenger(handler); // Target we publish for clients to send messages to IncomingHandler.

	// Name of the connected device
	DeviceAddress address;
	private AltosBluetooth  altos_bluetooth  = null;
	private TelemetryReader telemetry_reader = null;
	private TelemetryLogger telemetry_logger = null;

	// Local Bluetooth adapter
	private BluetoothAdapter bluetooth_adapter = null;

	// Last data seen; send to UI when it starts
	private TelemetryState	telemetry_state;

	// Handler of incoming messages from clients.
	static class IncomingHandler extends Handler {
		private final WeakReference<TelemetryService> service;
		IncomingHandler(TelemetryService s) { service = new WeakReference<TelemetryService>(s); }

		@Override
		public void handleMessage(Message msg) {
			TelemetryService s = service.get();
			if (s == null)
				return;
			switch (msg.what) {

				/* Messages from application */
			case MSG_REGISTER_CLIENT:
				s.add_client(msg.replyTo);
				break;
			case MSG_UNREGISTER_CLIENT:
				s.remove_client(msg.replyTo);
				break;
			case MSG_CONNECT:
				if (D) Log.d(TAG, "Connect command received");
				DeviceAddress address = (DeviceAddress) msg.obj;
				AltosDroidPreferences.set_active_device(address);
				s.start_altos_bluetooth(address);
				break;
			case MSG_SETFREQUENCY:
				if (D) Log.d(TAG, "MSG_SETFREQUENCY");
				s.telemetry_state.frequency = (Double) msg.obj;
				if (s.telemetry_state.connect == TelemetryState.CONNECT_CONNECTED) {
					try {
						s.altos_bluetooth.set_radio_frequency(s.telemetry_state.frequency);
						s.altos_bluetooth.save_frequency();
					} catch (InterruptedException e) {
					} catch (TimeoutException e) {
					}
				}
				s.send_to_clients();
				break;
			case MSG_SETBAUD:
				if (D) Log.d(TAG, "MSG_SETBAUD");
				s.telemetry_state.telemetry_rate = (Integer) msg.obj;
				if (s.telemetry_state.connect == TelemetryState.CONNECT_CONNECTED) {
					s.altos_bluetooth.set_telemetry_rate(s.telemetry_state.telemetry_rate);
					s.altos_bluetooth.save_telemetry_rate();
				}
				s.send_to_clients();
				break;

				/*
				 *Messages from AltosBluetooth
				 */
			case MSG_CONNECTED:
				if (D) Log.d(TAG, "Connected to device");
				try {
					s.connected();
				} catch (InterruptedException ie) {
				}
				break;
			case MSG_CONNECT_FAILED:
				if (s.address != null && !s.clients.isEmpty()) {
					if (D) Log.d(TAG, "Connection failed... retrying");
					s.start_altos_bluetooth(s.address);
				} else {
					s.stop_altos_bluetooth(true);
				}
				break;
			case MSG_DISCONNECTED:
				Log.d(TAG, "MSG_DISCONNECTED");
				if (s.address != null && !s.clients.isEmpty()) {
					if (D) Log.d(TAG, "Connection lost... retrying");
					s.start_altos_bluetooth(s.address);
				} else {
					s.stop_altos_bluetooth(true);
				}
				break;

				/*
				 * Messages from TelemetryReader
				 */
			case MSG_TELEMETRY:
				s.telemetry_state.state = (AltosState) msg.obj;
				if (s.telemetry_state.state != null) {
					if (D) Log.d(TAG, "Save state");
					AltosPreferences.set_state(0, s.telemetry_state.state, null);
				}
				if (D) Log.d(TAG, "MSG_TELEMETRY");
				s.send_to_clients();
				break;
			case MSG_CRC_ERROR:
				// forward crc error messages
				s.telemetry_state.crc_errors = (Integer) msg.obj;
				if (D) Log.d(TAG, "MSG_CRC_ERROR");
				s.send_to_clients();
				break;
			default:
				super.handleMessage(msg);
			}
		}
	}

	/* Construct the message to deliver to clients
	 */
	private Message message() {
		if (telemetry_state == null)
			Log.d(TAG, "telemetry_state null!");
		if (telemetry_state.state == null)
			Log.d(TAG, "telemetry_state.state null!");
		return Message.obtain(null, AltosDroid.MSG_STATE, telemetry_state);
	}

	/* A new friend has connected
	 */
	private void add_client(Messenger client) {

		clients.add(client);
		if (D) Log.d(TAG, "Client bound to service");

		/* On connect, send the current state to the new client
		 */
		send_to_client(client, message());

		/* If we've got an address from a previous session, then
		 * go ahead and try to reconnect to the device
		 */
		if (address != null && telemetry_state.connect == TelemetryState.CONNECT_DISCONNECTED) {
			if (D) Log.d(TAG, "Reconnecting now...");
			start_altos_bluetooth(address);
		}
	}

	/* A client has disconnected, clean up
	 */
	private void remove_client(Messenger client) {
		clients.remove(client);
		if (D) Log.d(TAG, "Client unbound from service");

		/* When the list of clients is empty, stop the service if
		 * we have no current telemetry source
		 */

		 if (clients.isEmpty() && telemetry_state.connect == TelemetryState.CONNECT_DISCONNECTED) {
			 if (!D) Log.d(TAG, "No clients, no connection. Stopping\n");
			 stopSelf();
		 }
	}

	private void send_to_client(Messenger client, Message m) {
		try {
			if (D) Log.d(TAG, String.format("Send message to client %s", client.toString()));
			client.send(m);
		} catch (RemoteException e) {
			if (D) Log.e(TAG, String.format("Client %s disappeared", client.toString()));
			remove_client(client);
		}
	}

	private void send_to_clients() {
		Message m = message();
		if (D) Log.d(TAG, String.format("Send message to %d clients", clients.size()));
		for (Messenger client : clients)
			send_to_client(client, m);
	}

	private void stop_altos_bluetooth(boolean notify) {
		if (D) Log.d(TAG, "stop_altos_bluetooth(): begin");
		telemetry_state.connect = TelemetryState.CONNECT_DISCONNECTED;
		telemetry_state.address = null;

		if (altos_bluetooth != null)
			altos_bluetooth.closing();

		if (telemetry_reader != null) {
			if (D) Log.d(TAG, "stop_altos_bluetooth(): stopping TelemetryReader");
			telemetry_reader.interrupt();
			try {
				telemetry_reader.join();
			} catch (InterruptedException e) {
			}
			telemetry_reader = null;
		}
		if (telemetry_logger != null) {
			if (D) Log.d(TAG, "stop_altos_bluetooth(): stopping TelemetryLogger");
			telemetry_logger.stop();
			telemetry_logger = null;
		}
		if (altos_bluetooth != null) {
			if (D) Log.d(TAG, "stop_altos_bluetooth(): stopping AltosBluetooth");
			altos_bluetooth.close();
			altos_bluetooth = null;
		}
		telemetry_state.config = null;
		if (notify) {
			if (D) Log.d(TAG, "stop_altos_bluetooth(): send message to clients");
			send_to_clients();
			if (clients.isEmpty()) {
				if (D) Log.d(TAG, "stop_altos_bluetooth(): no clients, terminating");
				stopSelf();
			}
		}
	}

	private void start_altos_bluetooth(DeviceAddress address) {
		// Get the BLuetoothDevice object
		BluetoothDevice device = bluetooth_adapter.getRemoteDevice(address.address);

		stop_altos_bluetooth(false);
		this.address = address;
		if (D) Log.d(TAG, String.format("start_altos_bluetooth(): Connecting to %s (%s)", device.getName(), device.getAddress()));
		altos_bluetooth = new AltosBluetooth(device, handler);
		telemetry_state.connect = TelemetryState.CONNECT_CONNECTING;
		telemetry_state.address = address;
		send_to_clients();
	}

	private void connected() throws InterruptedException {
		if (D) Log.d(TAG, "connected top");
		try {
			if (altos_bluetooth == null)
				throw new InterruptedException("no bluetooth");
			telemetry_state.config = altos_bluetooth.config_data();
			altos_bluetooth.set_radio_frequency(telemetry_state.frequency);
			altos_bluetooth.set_telemetry_rate(telemetry_state.telemetry_rate);
		} catch (TimeoutException e) {
			// If this timed out, then we really want to retry it, but
			// probably safer to just retry the connection from scratch.
			handler.obtainMessage(MSG_CONNECT_FAILED).sendToTarget();
			return;
		}

		if (D) Log.d(TAG, "connected bluetooth configured");
		telemetry_state.connect = TelemetryState.CONNECT_CONNECTED;
		telemetry_state.address = address;

		telemetry_reader = new TelemetryReader(altos_bluetooth, handler, telemetry_state.state);
		telemetry_reader.start();

		if (D) Log.d(TAG, "connected TelemetryReader started");

		telemetry_logger = new TelemetryLogger(this, altos_bluetooth);

		if (D) Log.d(TAG, "Notify UI of connection");

		send_to_clients();
	}


	@Override
	public void onCreate() {
		// Get local Bluetooth adapter
		bluetooth_adapter = BluetoothAdapter.getDefaultAdapter();

		// If the adapter is null, then Bluetooth is not supported
		if (bluetooth_adapter == null) {
			Toast.makeText(this, "Bluetooth is not available", Toast.LENGTH_LONG).show();
		}

		// Initialise preferences
		AltosDroidPreferences.init(this);

		telemetry_state = new TelemetryState();

		// Create a reference to the NotificationManager so that we can update our notifcation text later
		//mNM = (NotificationManager)getSystemService(NOTIFICATION_SERVICE);

		telemetry_state.connect = TelemetryState.CONNECT_DISCONNECTED;
		telemetry_state.address = null;

		AltosSavedState saved_state = AltosPreferences.state(0);

		if (saved_state != null) {
			if (D) Log.d(TAG, String.format("recovered old state flight %d\n", saved_state.state.flight));
			telemetry_state.state = saved_state.state;
		}

		// Listen for GPS and Network position updates
		LocationManager locationManager = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);

		locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 1000, 1, this);

		DeviceAddress address = AltosDroidPreferences.active_device();
		if (address != null)
			start_altos_bluetooth(address);
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		Log.i("TelemetryService", "Received start id " + startId + ": " + intent);

		CharSequence text = getText(R.string.telemetry_service_started);

		// Create notification to be displayed while the service runs
		Notification notification = new Notification(R.drawable.am_status_c, text, 0);

		// The PendingIntent to launch our activity if the user selects this notification
		PendingIntent contentIntent = PendingIntent.getActivity(this, 0,
				new Intent(this, AltosDroid.class), 0);

		// Set the info for the views that show in the notification panel.
		notification.setLatestEventInfo(this, getText(R.string.telemetry_service_label), text, contentIntent);

		// Set the notification to be in the "Ongoing" section.
		notification.flags |= Notification.FLAG_ONGOING_EVENT;

		// Move us into the foreground.
		startForeground(NOTIFICATION, notification);

		// We want this service to continue running until it is explicitly
		// stopped, so return sticky.
		return START_STICKY;
	}

	@Override
	public void onDestroy() {

		// Stop listening for location updates
		((LocationManager) getSystemService(Context.LOCATION_SERVICE)).removeUpdates(this);

		// Stop the bluetooth Comms threads
		stop_altos_bluetooth(true);

		// Demote us from the foreground, and cancel the persistent notification.
		stopForeground(true);

		// Tell the user we stopped.
		Toast.makeText(this, R.string.telemetry_service_stopped, Toast.LENGTH_SHORT).show();
	}

	@Override
	public IBinder onBind(Intent intent) {
		return messenger.getBinder();
	}


	public void onLocationChanged(Location location) {
		telemetry_state.location = location;
		if (D) Log.d(TAG, "location changed");
		send_to_clients();
	}

	public void onStatusChanged(String provider, int status, Bundle extras) {
	}

	public void onProviderEnabled(String provider) {
	}

	public void onProviderDisabled(String provider) {
	}

}
