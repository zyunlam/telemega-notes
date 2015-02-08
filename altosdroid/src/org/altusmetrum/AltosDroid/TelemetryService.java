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

	// Timer - we wake up every now and then to decide if the service should stop
	private Timer timer = new Timer();

	ArrayList<Messenger> mClients = new ArrayList<Messenger>(); // Keeps track of all current registered clients.
	final Handler   mHandler   = new IncomingHandler(this);
	final Messenger mMessenger = new Messenger(mHandler); // Target we publish for clients to send messages to IncomingHandler.

	// Name of the connected device
	String address;
	private AltosBluetooth  mAltosBluetooth  = null;
	private TelemetryReader mTelemetryReader = null;
	private TelemetryLogger mTelemetryLogger = null;
	// Local Bluetooth adapter
	private BluetoothAdapter mBluetoothAdapter = null;

	private TelemetryState	telemetry_state;

	// Last data seen; send to UI when it starts

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
			case MSG_REGISTER_CLIENT:
				s.mClients.add(msg.replyTo);
				try {
					// Now we try to send the freshly connected UI any relavant information about what
					// we're talking to
					msg.replyTo.send(s.message());
				} catch (RemoteException e) {
					s.mClients.remove(msg.replyTo);
				}
				if (D) Log.d(TAG, "Client bound to service");
				break;
			case MSG_UNREGISTER_CLIENT:
				s.mClients.remove(msg.replyTo);
				if (D) Log.d(TAG, "Client unbound from service");
				break;
			case MSG_CONNECT:
				if (D) Log.d(TAG, "Connect command received");
				String address = (String) msg.obj;
				AltosDroidPreferences.set_active_device(address);
				s.startAltosBluetooth(address);
				break;
			case MSG_CONNECTED:
				if (D) Log.d(TAG, "Connected to device");
				try {
					s.connected();
				} catch (InterruptedException ie) {
				}
				break;
			case MSG_CONNECT_FAILED:
				if (D) Log.d(TAG, "Connection failed... retrying");
				if (s.address != null)
					s.startAltosBluetooth(s.address);
				break;
			case MSG_DISCONNECTED:
				Log.d(TAG, "MSG_DISCONNECTED");
				s.stopAltosBluetooth();
				break;
			case MSG_TELEMETRY:
				// forward telemetry messages
				s.telemetry_state.state = (AltosState) msg.obj;
				if (s.telemetry_state.state != null) {
					if (D) Log.d(TAG, "Save state");
					AltosPreferences.set_state(0, s.telemetry_state.state, null);
				}
				if (D) Log.d(TAG, "MSG_TELEMETRY");
				s.sendMessageToClients();
				break;
			case MSG_CRC_ERROR:
				// forward crc error messages
				s.telemetry_state.crc_errors = (Integer) msg.obj;
				if (D) Log.d(TAG, "MSG_CRC_ERROR");
				s.sendMessageToClients();
				break;
			case MSG_SETFREQUENCY:
				if (D) Log.d(TAG, "MSG_SETFREQUENCY");
				s.telemetry_state.frequency = (Double) msg.obj;
				if (s.telemetry_state.connect == TelemetryState.CONNECT_CONNECTED) {
					try {
						s.mAltosBluetooth.set_radio_frequency(s.telemetry_state.frequency);
						s.mAltosBluetooth.save_frequency();
					} catch (InterruptedException e) {
					} catch (TimeoutException e) {
					}
				}
				s.sendMessageToClients();
				break;
			case MSG_SETBAUD:
				if (D) Log.d(TAG, "MSG_SETBAUD");
				s.telemetry_state.telemetry_rate = (Integer) msg.obj;
				if (s.telemetry_state.connect == TelemetryState.CONNECT_CONNECTED) {
					s.mAltosBluetooth.set_telemetry_rate(s.telemetry_state.telemetry_rate);
					s.mAltosBluetooth.save_telemetry_rate();
				}
				s.sendMessageToClients();
				break;
			default:
				super.handleMessage(msg);
			}
		}
	}

	private Message message() {
		if (telemetry_state == null)
			Log.d(TAG, "telemetry_state null!");
		if (telemetry_state.state == null)
			Log.d(TAG, "telemetry_state.state null!");
		return Message.obtain(null, AltosDroid.MSG_STATE, telemetry_state);
	}

	private void sendMessageToClients() {
		Message m = message();
		if (D) Log.d(TAG, String.format("Send message to %d clients", mClients.size()));
		for (int i=mClients.size()-1; i>=0; i--) {
			try {
				if (D) Log.d(TAG, String.format("Send message to client %d", i));
				mClients.get(i).send(m);
			} catch (RemoteException e) {
				mClients.remove(i);
			}
		}
	}

	private void stopAltosBluetooth() {
		if (D) Log.d(TAG, "stopAltosBluetooth(): begin");
		telemetry_state.connect = TelemetryState.CONNECT_READY;
		if (mTelemetryReader != null) {
			if (D) Log.d(TAG, "stopAltosBluetooth(): stopping TelemetryReader");
			mTelemetryReader.interrupt();
			try {
				mTelemetryReader.join();
			} catch (InterruptedException e) {
			}
			mTelemetryReader = null;
		}
		if (mTelemetryLogger != null) {
			if (D) Log.d(TAG, "stopAltosBluetooth(): stopping TelemetryLogger");
			mTelemetryLogger.stop();
			mTelemetryLogger = null;
		}
		if (mAltosBluetooth != null) {
			if (D) Log.d(TAG, "stopAltosBluetooth(): stopping AltosBluetooth");
			mAltosBluetooth.close();
			mAltosBluetooth = null;
		}
		telemetry_state.config = null;
		if (D) Log.d(TAG, "stopAltosBluetooth(): send message to clients");
		sendMessageToClients();
	}

	private void startAltosBluetooth(String address) {
		// Get the BLuetoothDevice object
		BluetoothDevice device = mBluetoothAdapter.getRemoteDevice(address);

		this.address = address;
		if (mAltosBluetooth == null) {
			if (D) Log.d(TAG, String.format("startAltosBluetooth(): Connecting to %s (%s)", device.getName(), device.getAddress()));
			mAltosBluetooth = new AltosBluetooth(device, mHandler);
			telemetry_state.connect = TelemetryState.CONNECT_CONNECTING;
			sendMessageToClients();
		} else {
			// This is a bit of a hack - if it appears we're still connected, we treat this as a restart.
			// So, to give a suitable delay to teardown/bringup, we just schedule a resend of a message
			// to ourselves in a few seconds time that will ultimately call this method again.
			// ... then we tear down the existing connection.
			// We do it this way around so that we don't lose a reference to the device when this method
			// is called on reception of MSG_CONNECT_FAILED in the handler above.
			mHandler.sendMessageDelayed(Message.obtain(null, MSG_CONNECT, address), 3000);
			stopAltosBluetooth();
		}
	}

	private void connected() throws InterruptedException {
		if (D) Log.d(TAG, "connected top");
		try {
			if (mAltosBluetooth == null)
				throw new InterruptedException("no bluetooth");
			telemetry_state.config = mAltosBluetooth.config_data();
			mAltosBluetooth.set_radio_frequency(telemetry_state.frequency);
			mAltosBluetooth.set_telemetry_rate(telemetry_state.telemetry_rate);
		} catch (TimeoutException e) {
			// If this timed out, then we really want to retry it, but
			// probably safer to just retry the connection from scratch.
			mHandler.obtainMessage(MSG_CONNECT_FAILED).sendToTarget();
			return;
		}

		if (D) Log.d(TAG, "connected bluetooth configured");
		telemetry_state.connect = TelemetryState.CONNECT_CONNECTED;

		mTelemetryReader = new TelemetryReader(mAltosBluetooth, mHandler, telemetry_state.state);
		mTelemetryReader.start();

		if (D) Log.d(TAG, "connected TelemetryReader started");

		mTelemetryLogger = new TelemetryLogger(this, mAltosBluetooth);

		if (D) Log.d(TAG, "Notify UI of connection");

		sendMessageToClients();
	}

	private void onTimerTick() {
		if (D) Log.d(TAG, "Timer wakeup");
		try {
			if (mClients.size() <= 0 && telemetry_state.connect != TelemetryState.CONNECT_CONNECTED) {
				stopSelf();
			}
		} catch (Throwable t) {
			Log.e(TAG, "Timer failed: ", t);
		}
	}


	@Override
	public void onCreate() {
		// Get local Bluetooth adapter
		mBluetoothAdapter = BluetoothAdapter.getDefaultAdapter();

		// If the adapter is null, then Bluetooth is not supported
		if (mBluetoothAdapter == null) {
			Toast.makeText(this, "Bluetooth is not available", Toast.LENGTH_LONG).show();
		}

		// Initialise preferences
		AltosDroidPreferences.init(this);

		telemetry_state = new TelemetryState();

		// Create a reference to the NotificationManager so that we can update our notifcation text later
		//mNM = (NotificationManager)getSystemService(NOTIFICATION_SERVICE);

		telemetry_state.connect = TelemetryState.CONNECT_READY;

		AltosSavedState saved_state = AltosPreferences.state(0);

		if (saved_state != null) {
			if (D) Log.d(TAG, String.format("recovered old state flight %d\n", saved_state.state.flight));
			telemetry_state.state = saved_state.state;
		}

		// Start our timer - first event in 10 seconds, then every 10 seconds after that.
		timer.scheduleAtFixedRate(new TimerTask(){ public void run() {onTimerTick();}}, 10000L, 10000L);

		// Listen for GPS and Network position updates
		LocationManager locationManager = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);

		locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 1000, 1, this);

		String address = AltosDroidPreferences.active_device();
		if (address != null)
			startAltosBluetooth(address);
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
		stopAltosBluetooth();

		// Demote us from the foreground, and cancel the persistent notification.
		stopForeground(true);

		// Stop our timer
		if (timer != null) {timer.cancel();}

		// Tell the user we stopped.
		Toast.makeText(this, R.string.telemetry_service_stopped, Toast.LENGTH_SHORT).show();
	}

	@Override
	public IBinder onBind(Intent intent) {
		return mMessenger.getBinder();
	}


	public void onLocationChanged(Location location) {
		telemetry_state.location = location;
		if (D) Log.d(TAG, "location changed");
		sendMessageToClients();
	}

	public void onStatusChanged(String provider, int status, Bundle extras) {
	}

	public void onProviderEnabled(String provider) {
	}

	public void onProviderDisabled(String provider) {
	}

}
