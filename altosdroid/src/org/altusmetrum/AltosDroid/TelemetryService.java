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
import java.util.concurrent.TimeoutException;
import java.util.*;

import android.app.Notification;
//import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothAdapter;
import android.hardware.usb.*;
import android.content.Intent;
import android.content.Context;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Handler;
import android.os.Message;
import android.os.Messenger;
import android.os.RemoteException;
import android.os.Looper;
import android.widget.Toast;
import android.location.Criteria;

import org.altusmetrum.altoslib_10.*;

public class TelemetryService extends Service implements AltosIdleMonitorListener {

	static final int MSG_REGISTER_CLIENT   = 1;
	static final int MSG_UNREGISTER_CLIENT = 2;
	static final int MSG_CONNECT           = 3;
	static final int MSG_OPEN_USB	       = 4;
	static final int MSG_CONNECTED         = 5;
	static final int MSG_CONNECT_FAILED    = 6;
	static final int MSG_DISCONNECTED      = 7;
	static final int MSG_TELEMETRY         = 8;
	static final int MSG_SETFREQUENCY      = 9;
	static final int MSG_CRC_ERROR	       = 10;
	static final int MSG_SETBAUD	       = 11;
	static final int MSG_DISCONNECT	       = 12;
	static final int MSG_DELETE_SERIAL     = 13;
	static final int MSG_BLUETOOTH_ENABLED = 14;
	static final int MSG_MONITOR_IDLE_START= 15;
	static final int MSG_MONITOR_IDLE_STOP = 16;
	static final int MSG_REBOOT	       = 17;
	static final int MSG_IGNITER_QUERY     = 18;
	static final int MSG_IGNITER_FIRE      = 19;

	// Unique Identification Number for the Notification.
	// We use it on Notification start, and to cancel it.
	private int NOTIFICATION = R.string.telemetry_service_label;
	//private NotificationManager mNM;

	ArrayList<Messenger> clients = new ArrayList<Messenger>(); // Keeps track of all current registered clients.
	final Handler   handler   = new IncomingHandler(this);
	final Messenger messenger = new Messenger(handler); // Target we publish for clients to send messages to IncomingHandler.

	// Name of the connected device
	DeviceAddress address;
	private AltosDroidLink  altos_link  = null;
	private TelemetryReader telemetry_reader = null;
	private TelemetryLogger telemetry_logger = null;

	// Local Bluetooth adapter
	private BluetoothAdapter bluetooth_adapter = null;

	// Last data seen; send to UI when it starts
	private TelemetryState	telemetry_state;

	// Idle monitor if active
	AltosIdleMonitor idle_monitor = null;

	// Igniter bits
	AltosIgnite ignite = null;
	boolean ignite_running;

	// Handler of incoming messages from clients.
	static class IncomingHandler extends Handler {
		private final WeakReference<TelemetryService> service;
		IncomingHandler(TelemetryService s) { service = new WeakReference<TelemetryService>(s); }

		@Override
		public void handleMessage(Message msg) {
			DeviceAddress address;

			TelemetryService s = service.get();
			AltosDroidLink bt = null;
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
				AltosDebug.debug("Connect command received");
				address = (DeviceAddress) msg.obj;
				AltosDroidPreferences.set_active_device(address);
				s.start_altos_bluetooth(address, false);
				break;
			case MSG_OPEN_USB:
				AltosDebug.debug("Open USB command received");
				UsbDevice device = (UsbDevice) msg.obj;
				s.start_usb(device);
				break;
			case MSG_DISCONNECT:
				AltosDebug.debug("Disconnect command received");
				s.address = null;
				AltosDroidPreferences.set_active_device(null);
				s.disconnect(true);
				break;
			case MSG_DELETE_SERIAL:
				AltosDebug.debug("Delete Serial command received");
				s.delete_serial((Integer) msg.obj);
				break;
			case MSG_SETFREQUENCY:
				AltosDebug.debug("MSG_SETFREQUENCY");
				s.telemetry_state.frequency = (Double) msg.obj;
				if (s.telemetry_state.connect == TelemetryState.CONNECT_CONNECTED) {
					try {
						s.altos_link.set_radio_frequency(s.telemetry_state.frequency);
						s.altos_link.save_frequency();
					} catch (InterruptedException e) {
					} catch (TimeoutException e) {
					}
				}
				s.send_to_clients();
				break;
			case MSG_SETBAUD:
				AltosDebug.debug("MSG_SETBAUD");
				s.telemetry_state.telemetry_rate = (Integer) msg.obj;
				if (s.telemetry_state.connect == TelemetryState.CONNECT_CONNECTED) {
					s.altos_link.set_telemetry_rate(s.telemetry_state.telemetry_rate);
					s.altos_link.save_telemetry_rate();
				}
				s.send_to_clients();
				break;

				/*
				 *Messages from AltosBluetooth
				 */
			case MSG_CONNECTED:
				AltosDebug.debug("MSG_CONNECTED");
				bt = (AltosDroidLink) msg.obj;

				if (bt != s.altos_link) {
					AltosDebug.debug("Stale message");
					break;
				}
				AltosDebug.debug("Connected to device");
				try {
					s.connected();
				} catch (InterruptedException ie) {
				}
				break;
			case MSG_CONNECT_FAILED:
				AltosDebug.debug("MSG_CONNECT_FAILED");
				bt = (AltosDroidLink) msg.obj;

				if (bt != s.altos_link) {
					AltosDebug.debug("Stale message");
					break;
				}
				if (s.address != null) {
					AltosDebug.debug("Connection failed... retrying");
					s.start_altos_bluetooth(s.address, true);
				} else {
					s.disconnect(true);
				}
				break;
			case MSG_DISCONNECTED:

				/* This can be sent by either AltosDroidLink or TelemetryReader */
				AltosDebug.debug("MSG_DISCONNECTED");
				bt = (AltosDroidLink) msg.obj;

				if (bt != s.altos_link) {
					AltosDebug.debug("Stale message");
					break;
				}
				if (s.address != null) {
					AltosDebug.debug("Connection lost... retrying");
					s.start_altos_bluetooth(s.address, true);
				} else {
					s.disconnect(true);
				}
				break;

				/*
				 * Messages from TelemetryReader
				 */
			case MSG_TELEMETRY:
				s.telemetry((AltosTelemetry) msg.obj);
				break;
			case MSG_CRC_ERROR:
				// forward crc error messages
				s.telemetry_state.crc_errors = (Integer) msg.obj;
				s.send_to_clients();
				break;
			case MSG_BLUETOOTH_ENABLED:
				AltosDebug.debug("TelemetryService notes that BT is now enabled");
				address = AltosDroidPreferences.active_device();
				if (address != null && !address.address.startsWith("USB"))
					s.start_altos_bluetooth(address, false);
				break;
			case MSG_MONITOR_IDLE_START:
				AltosDebug.debug("start monitor idle");
				s.start_idle_monitor();
				break;
			case MSG_MONITOR_IDLE_STOP:
				AltosDebug.debug("stop monitor idle");
				s.stop_idle_monitor();
				break;
			case MSG_REBOOT:
				AltosDebug.debug("reboot");
				s.reboot_remote();
				break;
			case MSG_IGNITER_QUERY:
				AltosDebug.debug("igniter query");
				s.igniter_query(msg.replyTo);
				break;
			case MSG_IGNITER_FIRE:
				AltosDebug.debug("igniter fire");
				s.igniter_fire((String) msg.obj);
				break;
			default:
				super.handleMessage(msg);
			}
		}
	}

	/* Handle telemetry packet
	 */
	private void telemetry(AltosTelemetry telem) {
		AltosState	state;

		if (telemetry_state.states.containsKey(telem.serial))
			state = telemetry_state.states.get(telem.serial).clone();
		else
			state = new AltosState();
		telem.update_state(state);
		telemetry_state.states.put(telem.serial, state);
		if (state != null) {
			AltosPreferences.set_state(state);
		}
		send_to_clients();
	}

	/* Construct the message to deliver to clients
	 */
	private Message message() {
		if (telemetry_state == null)
			AltosDebug.debug("telemetry_state null!");
		if (telemetry_state.states == null)
			AltosDebug.debug("telemetry_state.states null!");
		return Message.obtain(null, AltosDroid.MSG_STATE, telemetry_state);
	}

	/* A new friend has connected
	 */
	private void add_client(Messenger client) {

		clients.add(client);
		AltosDebug.debug("Client bound to service");

		/* On connect, send the current state to the new client
		 */
		send_to_client(client);
		send_idle_mode_to_client(client);

		/* If we've got an address from a previous session, then
		 * go ahead and try to reconnect to the device
		 */
		if (address != null && telemetry_state.connect == TelemetryState.CONNECT_DISCONNECTED) {
			AltosDebug.debug("Reconnecting now...");
			start_altos_bluetooth(address, false);
		}
	}

	/* A client has disconnected, clean up
	 */
	private void remove_client(Messenger client) {
		clients.remove(client);
		AltosDebug.debug("Client unbound from service");

		/* When the list of clients is empty, stop the service if
		 * we have no current telemetry source
		 */

		 if (clients.isEmpty() && telemetry_state.connect == TelemetryState.CONNECT_DISCONNECTED) {
			 AltosDebug.debug("No clients, no connection. Stopping\n");
			 stopSelf();
		 }
	}

	private void send_to_client(Messenger client) {
		Message m = message();
		try {
			client.send(m);
		} catch (RemoteException e) {
			AltosDebug.error("Client %s disappeared", client.toString());
			remove_client(client);
		}
	}

	private void send_to_clients() {
		for (Messenger client : clients)
			send_to_client(client);
	}

	private void send_idle_mode_to_client(Messenger client) {
		Message m = Message.obtain(null, AltosDroid.MSG_IDLE_MODE, idle_monitor != null);
		try {
			client.send(m);
		} catch (RemoteException e) {
			AltosDebug.error("Client %s disappeared", client.toString());
			remove_client(client);
		}
	}

	private void send_idle_mode_to_clients() {
		for (Messenger client : clients)
			send_idle_mode_to_client(client);
	}

	private void telemetry_start() {
		if (telemetry_reader == null && idle_monitor == null && !ignite_running) {
			telemetry_reader = new TelemetryReader(altos_link, handler);
			telemetry_reader.start();
		}
	}

	private void telemetry_stop() {
		if (telemetry_reader != null) {
			AltosDebug.debug("disconnect(): stopping TelemetryReader");
			telemetry_reader.interrupt();
			try {
				telemetry_reader.join();
			} catch (InterruptedException e) {
			}
			telemetry_reader = null;
		}
	}

	private void disconnect(boolean notify) {
		AltosDebug.debug("disconnect(): begin");

		telemetry_state.connect = TelemetryState.CONNECT_DISCONNECTED;
		telemetry_state.address = null;

		if (idle_monitor != null)
			stop_idle_monitor();

		if (altos_link != null)
			altos_link.closing();

		stop_receiver_voltage_timer();

		telemetry_stop();
		if (telemetry_logger != null) {
			AltosDebug.debug("disconnect(): stopping TelemetryLogger");
			telemetry_logger.stop();
			telemetry_logger = null;
		}
		if (altos_link != null) {
			AltosDebug.debug("disconnect(): stopping AltosDroidLink");
			altos_link.close();
			altos_link = null;
			ignite = null;
		}
		telemetry_state.config = null;
		if (notify) {
			AltosDebug.debug("disconnect(): send message to clients");
			send_to_clients();
			if (clients.isEmpty()) {
				AltosDebug.debug("disconnect(): no clients, terminating");
				stopSelf();
			}
		}
	}

	private void start_usb(UsbDevice device) {
		AltosUsb	d = new AltosUsb(this, device, handler);

		if (d != null) {
			disconnect(false);
			altos_link = d;
			try {
				connected();
			} catch (InterruptedException ie) {
			}
		}
	}

	private void delete_serial(int serial) {
		telemetry_state.states.remove((Integer) serial);
		AltosPreferences.remove_state(serial);
		send_to_clients();
	}

	private void start_altos_bluetooth(DeviceAddress address, boolean pause) {
		if (bluetooth_adapter == null || !bluetooth_adapter.isEnabled())
			return;

		disconnect(false);

		// Get the BluetoothDevice object
		BluetoothDevice device = bluetooth_adapter.getRemoteDevice(address.address);

		this.address = address;
		AltosDebug.debug("start_altos_bluetooth(): Connecting to %s (%s)", device.getName(), device.getAddress());
		altos_link = new AltosBluetooth(device, handler, pause);
		telemetry_state.connect = TelemetryState.CONNECT_CONNECTING;
		telemetry_state.address = address;
		send_to_clients();
	}

	private void start_idle_monitor() {
		if (altos_link != null && idle_monitor == null) {
			telemetry_stop();
			idle_monitor = new AltosIdleMonitor(this, altos_link, true, false);
			idle_monitor.set_callsign(AltosPreferences.callsign());
			idle_monitor.start();
			send_idle_mode_to_clients();
		}
	}

	private void stop_idle_monitor() {
		if (idle_monitor != null) {
			try {
				idle_monitor.abort();
			} catch (InterruptedException ie) {
			}
			idle_monitor = null;
			telemetry_start();
			send_idle_mode_to_clients();
		}
	}

	private void reboot_remote() {
		if (altos_link != null) {
			stop_idle_monitor();
			try {
				altos_link.start_remote();
				altos_link.printf("r eboot\n");
				altos_link.flush_output();
			} catch (TimeoutException te) {
			} catch (InterruptedException ie) {
			} finally {
				try {
					altos_link.stop_remote();
				} catch (InterruptedException ie) {
				}
			}
		}
	}

	private void ensure_ignite() {
		if (ignite == null)
			ignite = new AltosIgnite(altos_link, true, false);
	}

	private synchronized void igniter_query(Messenger client) {
		ensure_ignite();
		HashMap<String,Integer> status_map = null;
		ignite_running = true;
		try {
			stop_idle_monitor();
			try {
				status_map = ignite.status();
			} catch (InterruptedException ie) {
				AltosDebug.debug("ignite.status interrupted");
			} catch (TimeoutException te) {
				AltosDebug.debug("ignite.status timeout");
			}
		} finally {
			ignite_running = false;
		}
		Message m = Message.obtain(null, AltosDroid.MSG_IGNITER_STATUS, status_map);
		try {
			client.send(m);
		} catch (RemoteException e) {
		}
	}

	private synchronized void igniter_fire(String igniter) {
		ensure_ignite();
		ignite_running = true;
		stop_idle_monitor();
		try {
			ignite.fire(igniter);
		} catch (InterruptedException ie) {
		} finally {
			ignite_running = false;
		}
	}

	// Timer for receiver battery voltage monitoring
	Timer receiver_voltage_timer;

	private void update_receiver_voltage() {
		if (altos_link != null && idle_monitor == null && !ignite_running) {
			try {
				double	voltage = altos_link.monitor_battery();
				telemetry_state.receiver_battery = voltage;
				send_to_clients();
			} catch (InterruptedException ie) {
			}
		}
	}

	private void stop_receiver_voltage_timer() {
		if (receiver_voltage_timer != null) {
			receiver_voltage_timer.cancel();
			receiver_voltage_timer.purge();
			receiver_voltage_timer = null;
		}
	}

	private void start_receiver_voltage_timer() {
		if (receiver_voltage_timer == null && altos_link.has_monitor_battery()) {
			receiver_voltage_timer = new Timer();
			receiver_voltage_timer.scheduleAtFixedRate(new TimerTask() { public void run() {update_receiver_voltage();}}, 1000L, 10000L);
		}
	}

	private void connected() throws InterruptedException {
		AltosDebug.debug("connected top");
		AltosDebug.check_ui("connected\n");
		try {
			if (altos_link == null)
				throw new InterruptedException("no bluetooth");
			telemetry_state.config = altos_link.config_data();
			altos_link.set_radio_frequency(telemetry_state.frequency);
			altos_link.set_telemetry_rate(telemetry_state.telemetry_rate);
		} catch (TimeoutException e) {
			// If this timed out, then we really want to retry it, but
			// probably safer to just retry the connection from scratch.
			AltosDebug.debug("connected timeout");
			if (address != null) {
				AltosDebug.debug("connected timeout, retrying");
				start_altos_bluetooth(address, true);
			} else {
				handler.obtainMessage(MSG_CONNECT_FAILED).sendToTarget();
				disconnect(true);
			}
			return;
		}

		AltosDebug.debug("connected bluetooth configured");
		telemetry_state.connect = TelemetryState.CONNECT_CONNECTED;
		telemetry_state.address = address;

		telemetry_start();

		AltosDebug.debug("connected TelemetryReader started");

		telemetry_logger = new TelemetryLogger(this, altos_link);

		start_receiver_voltage_timer();

		AltosDebug.debug("Notify UI of connection");

		send_to_clients();
	}


	@Override
	public void onCreate() {

		AltosDebug.init(this);

		// Initialise preferences
		AltosDroidPreferences.init(this);

		// Get local Bluetooth adapter
		bluetooth_adapter = BluetoothAdapter.getDefaultAdapter();

		telemetry_state = new TelemetryState();

		// Create a reference to the NotificationManager so that we can update our notifcation text later
		//mNM = (NotificationManager)getSystemService(NOTIFICATION_SERVICE);

		telemetry_state.connect = TelemetryState.CONNECT_DISCONNECTED;
		telemetry_state.address = null;

		/* Pull the saved state information out of the preferences database
		 */
		ArrayList<Integer> serials = AltosPreferences.list_states();

		telemetry_state.latest_serial = AltosPreferences.latest_state();

		for (int serial : serials) {
			AltosState saved_state = AltosPreferences.state(serial);
			if (saved_state != null) {
				if (telemetry_state.latest_serial == 0)
					telemetry_state.latest_serial = serial;

				AltosDebug.debug("recovered old state serial %d flight %d",
						 serial,
						 saved_state.flight);
				if (saved_state.gps != null)
					AltosDebug.debug("\tposition %f,%f",
							 saved_state.gps.lat,
							 saved_state.gps.lon);
				telemetry_state.states.put(serial, saved_state);
			} else {
				AltosDebug.debug("Failed to recover state for %d", serial);
			}
		}
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		AltosDebug.debug("Received start id %d: %s", startId, intent);

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

		/* Start bluetooth if we don't have a connection already */
		if (intent != null &&
		    (telemetry_state.connect == TelemetryState.CONNECT_NONE ||
		     telemetry_state.connect == TelemetryState.CONNECT_DISCONNECTED))
		{
			String	action = intent.getAction();

			if (action.equals(AltosDroid.ACTION_BLUETOOTH)) {
				DeviceAddress address = AltosDroidPreferences.active_device();
				if (address != null && !address.address.startsWith("USB"))
					start_altos_bluetooth(address, false);
			}
		}

		// We want this service to continue running until it is explicitly
		// stopped, so return sticky.
		return START_STICKY;
	}

	@Override
	public void onDestroy() {

		// Stop the bluetooth Comms threads
		disconnect(true);

		// Demote us from the foreground, and cancel the persistent notification.
		stopForeground(true);

		// Tell the user we stopped.
		Toast.makeText(this, R.string.telemetry_service_stopped, Toast.LENGTH_SHORT).show();
	}

	@Override
	public IBinder onBind(Intent intent) {
		return messenger.getBinder();
	}

	/* AltosIdleMonitorListener */
	public void update(AltosState state, AltosListenerState listener_state)	{
		telemetry_state.states.put(state.serial, state);
		telemetry_state.receiver_battery = listener_state.battery;
		send_to_clients();
	}

	public void failed() {
	}

	public void error(String reason) {
		stop_idle_monitor();
	}
}
