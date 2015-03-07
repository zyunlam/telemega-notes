/*
 * Copyright © 2012-2013 Mike Beattie <mike@ethernal.org>
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
import java.util.Timer;
import java.util.TimerTask;

import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.Intent;
import android.content.Context;
import android.content.ComponentName;
import android.content.ServiceConnection;
import android.content.DialogInterface;
import android.os.IBinder;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.Messenger;
import android.os.RemoteException;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.Window;
import android.view.View;
import android.widget.TabHost;
import android.widget.TextView;
import android.widget.RelativeLayout;
import android.widget.Toast;
import android.app.AlertDialog;
import android.location.Location;

import org.altusmetrum.altoslib_6.*;

public class AltosDroid extends FragmentActivity implements AltosUnitsListener {
	// Debugging
	static final String TAG = "AltosDroid";
	static final boolean D = true;

	// Message types received by our Handler

	public static final int MSG_STATE           = 1;
	public static final int MSG_UPDATE_AGE      = 2;

	// Intent request codes
	public static final int REQUEST_CONNECT_DEVICE = 1;
	public static final int REQUEST_ENABLE_BT      = 2;

	public static FragmentManager	fm;

	private BluetoothAdapter mBluetoothAdapter = null;

	// Layout Views
	private TextView mTitle;

	// Flight state values
	private TextView mCallsignView;
	private TextView mRSSIView;
	private TextView mSerialView;
	private TextView mFlightView;
	private RelativeLayout mStateLayout;
	private TextView mStateView;
	private TextView mAgeView;

	// field to display the version at the bottom of the screen
	private TextView mVersion;

	private double frequency;
	private int telemetry_rate;

	// Tabs
	TabHost         mTabHost;
	AltosViewPager  mViewPager;
	TabsAdapter     mTabsAdapter;
	ArrayList<AltosDroidTab> mTabs = new ArrayList<AltosDroidTab>();
	int             tabHeight;

	// Timer and Saved flight state for Age calculation
	private Timer timer;
	AltosState saved_state;

	// Service
	private boolean mIsBound   = false;
	private Messenger mService = null;
	final Messenger mMessenger = new Messenger(new IncomingHandler(this));

	// Text to Speech
	private AltosVoice mAltosVoice = null;

	// The Handler that gets information back from the Telemetry Service
	static class IncomingHandler extends Handler {
		private final WeakReference<AltosDroid> mAltosDroid;
		IncomingHandler(AltosDroid ad) { mAltosDroid = new WeakReference<AltosDroid>(ad); }

		@Override
		public void handleMessage(Message msg) {
			AltosDroid ad = mAltosDroid.get();

			switch (msg.what) {
			case MSG_STATE:
				if(D) Log.d(TAG, "MSG_STATE");
				TelemetryState telemetry_state = (TelemetryState) msg.obj;
				if (telemetry_state == null) {
					Log.d(TAG, "telemetry_state null!");
					return;
				}

				ad.update_state(telemetry_state);
				break;
			case MSG_UPDATE_AGE:
				if(D) Log.d(TAG, "MSG_UPDATE_AGE");
				ad.update_age();
				break;
			}
		}
	};


	private ServiceConnection mConnection = new ServiceConnection() {
		public void onServiceConnected(ComponentName className, IBinder service) {
			mService = new Messenger(service);
			try {
				Message msg = Message.obtain(null, TelemetryService.MSG_REGISTER_CLIENT);
				msg.replyTo = mMessenger;
				mService.send(msg);
			} catch (RemoteException e) {
				// In this case the service has crashed before we could even do anything with it
			}
		}

		public void onServiceDisconnected(ComponentName className) {
			// This is called when the connection with the service has been unexpectedly disconnected - process crashed.
			mService = null;
		}
	};

	void doBindService() {
		bindService(new Intent(this, TelemetryService.class), mConnection, Context.BIND_AUTO_CREATE);
		mIsBound = true;
	}

	void doUnbindService() {
		if (mIsBound) {
			// If we have received the service, and hence registered with it, then now is the time to unregister.
			if (mService != null) {
				try {
					Message msg = Message.obtain(null, TelemetryService.MSG_UNREGISTER_CLIENT);
					msg.replyTo = mMessenger;
					mService.send(msg);
				} catch (RemoteException e) {
					// There is nothing special we need to do if the service has crashed.
				}
			}
			// Detach our existing connection.
			unbindService(mConnection);
			mIsBound = false;
		}
	}

	public void registerTab(AltosDroidTab mTab) {
		mTabs.add(mTab);
	}

	public void unregisterTab(AltosDroidTab mTab) {
		mTabs.remove(mTab);
	}

	public void units_changed(boolean imperial_units) {
		for (AltosDroidTab mTab : mTabs)
			mTab.units_changed(imperial_units);
	}

	void update_title(TelemetryState telemetry_state) {
		switch (telemetry_state.connect) {
		case TelemetryState.CONNECT_CONNECTED:
			if (telemetry_state.config != null) {
				String str = String.format("S/N %d %6.3f MHz", telemetry_state.config.serial,
							   telemetry_state.frequency);
				if (telemetry_state.telemetry_rate != AltosLib.ao_telemetry_rate_38400)
					str = str.concat(String.format(" %d bps",
								       AltosLib.ao_telemetry_rate_values[telemetry_state.telemetry_rate]));
				mTitle.setText(str);
			} else {
				mTitle.setText(R.string.title_connected_to);
			}
			break;
		case TelemetryState.CONNECT_CONNECTING:
			if (telemetry_state.address != null)
				mTitle.setText(String.format("Connecting to %s...", telemetry_state.address.name));
			else
				mTitle.setText("Connecting to something...");
			break;
		case TelemetryState.CONNECT_DISCONNECTED:
		case TelemetryState.CONNECT_NONE:
			mTitle.setText(R.string.title_not_connected);
			break;
		}
	}

	void start_timer() {
		if (timer == null) {
			timer = new Timer();
			timer.scheduleAtFixedRate(new TimerTask(){ public void run() {onTimerTick();}}, 1000L, 1000L);
		}
	}

	void stop_timer() {
		if (timer != null) {
			timer.cancel();
			timer.purge();
			timer = null;
		}
	}

	boolean	registered_units_listener;

	void update_state(TelemetryState telemetry_state) {

		if (!registered_units_listener) {
			registered_units_listener = true;
			AltosPreferences.register_units_listener(this);
		}

		update_title(telemetry_state);
		update_ui(telemetry_state.state, telemetry_state.location);
		if (telemetry_state.connect == TelemetryState.CONNECT_CONNECTED)
			start_timer();
	}

	boolean same_string(String a, String b) {
		if (a == null) {
			if (b == null)
				return true;
			return false;
		} else {
			if (b == null)
				return false;
			return a.equals(b);
		}
	}

	void update_age() {
		if (saved_state != null)
			mAgeView.setText(String.format("%d", (System.currentTimeMillis() - saved_state.received_time + 500) / 1000));
	}

	void update_ui(AltosState state, Location location) {

		int prev_state = AltosLib.ao_flight_invalid;

		AltosGreatCircle from_receiver = null;

		if (saved_state != null)
			prev_state = saved_state.state;

		if (state != null) {
			if (state.state == AltosLib.ao_flight_stateless) {
				boolean	prev_locked = false;
				boolean locked = false;

				if(state.gps != null)
					locked = state.gps.locked;
				if (saved_state != null && saved_state.gps != null)
					prev_locked = saved_state.gps.locked;
				if (prev_locked != locked) {
					String currentTab = mTabHost.getCurrentTabTag();
					if (locked) {
						if (currentTab.equals("pad")) mTabHost.setCurrentTabByTag("descent");
					} else {
						if (currentTab.equals("descent")) mTabHost.setCurrentTabByTag("pad");
					}
				}
			} else {
				if (prev_state != state.state) {
					String currentTab = mTabHost.getCurrentTabTag();
					switch (state.state) {
					case AltosLib.ao_flight_boost:
						if (currentTab.equals("pad")) mTabHost.setCurrentTabByTag("ascent");
						break;
					case AltosLib.ao_flight_drogue:
						if (currentTab.equals("ascent")) mTabHost.setCurrentTabByTag("descent");
						break;
					case AltosLib.ao_flight_landed:
						if (currentTab.equals("descent")) mTabHost.setCurrentTabByTag("landed");
						break;
					case AltosLib.ao_flight_stateless:
						if (currentTab.equals("pad")) mTabHost.setCurrentTabByTag("descent");
						break;
					}
				}
			}

			if (location != null && state.gps != null && state.gps.locked) {
				double altitude = 0;
				if (location.hasAltitude())
					altitude = location.getAltitude();
				from_receiver = new AltosGreatCircle(location.getLatitude(),
								     location.getLongitude(),
								     altitude,
								     state.gps.lat,
								     state.gps.lon,
								     state.gps.alt);
			}

			if (saved_state == null || !same_string(saved_state.callsign, state.callsign)) {
				mCallsignView.setText(state.callsign);
			}
			if (saved_state == null || state.serial != saved_state.serial) {
				mSerialView.setText(String.format("%d", state.serial));
			}
			if (saved_state == null || state.flight != saved_state.flight) {
				if (state.flight == AltosLib.MISSING)
					mFlightView.setText("");
				else
					mFlightView.setText(String.format("%d", state.flight));
			}
			if (saved_state == null || state.state != saved_state.state) {
				if (state.state == AltosLib.ao_flight_stateless) {
					mStateLayout.setVisibility(View.GONE);
				} else {
					mStateView.setText(state.state_name());
					mStateLayout.setVisibility(View.VISIBLE);
				}
			}
			if (saved_state == null || state.rssi != saved_state.rssi) {
				mRSSIView.setText(String.format("%d", state.rssi));
			}
		}

		for (AltosDroidTab mTab : mTabs)
			mTab.update_ui(state, from_receiver, location, mTab == mTabsAdapter.currentItem());

		if (state != null && mAltosVoice != null)
			mAltosVoice.tell(state, from_receiver);

		saved_state = state;
	}

	private void onTimerTick() {
		try {
			mMessenger.send(Message.obtain(null, MSG_UPDATE_AGE));
		} catch (RemoteException e) {
		}
	}

	static String pos(double p, String pos, String neg) {
		String	h = pos;
		if (p == AltosLib.MISSING)
			return "";
		if (p < 0) {
			h = neg;
			p = -p;
		}
		int deg = (int) Math.floor(p);
		double min = (p - Math.floor(p)) * 60.0;
		return String.format("%d°%9.4f\" %s", deg, min, h);
	}

	static String number(String format, double value) {
		if (value == AltosLib.MISSING)
			return "";
		return String.format(format, value);
	}

	static String integer(String format, int value) {
		if (value == AltosLib.MISSING)
			return "";
		return String.format(format, value);
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		if(D) Log.e(TAG, "+++ ON CREATE +++");

		// Get local Bluetooth adapter
		mBluetoothAdapter = BluetoothAdapter.getDefaultAdapter();

		// If the adapter is null, then Bluetooth is not supported
		if (mBluetoothAdapter == null) {
			Toast.makeText(this, "Bluetooth is not available", Toast.LENGTH_LONG).show();
			finish();
		}

		fm = getSupportFragmentManager();

		// Set up the window layout
		requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
		setContentView(R.layout.altosdroid);
		getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.custom_title);

		// Create the Tabs and ViewPager
		mTabHost = (TabHost)findViewById(android.R.id.tabhost);
		mTabHost.setup();

		mViewPager = (AltosViewPager)findViewById(R.id.pager);
		mViewPager.setOffscreenPageLimit(4);

		mTabsAdapter = new TabsAdapter(this, mTabHost, mViewPager);

		mTabsAdapter.addTab(mTabHost.newTabSpec("pad").setIndicator("Pad"), TabPad.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("ascent").setIndicator("Ascent"), TabAscent.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("descent").setIndicator("Descent"), TabDescent.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("landed").setIndicator("Landed"), TabLanded.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("map").setIndicator("Map"), TabMap.class, null);


		// Scale the size of the Tab bar for different screen densities
		// This probably won't be needed when we start supporting ICS+ tabs.
		DisplayMetrics metrics = new DisplayMetrics();
		getWindowManager().getDefaultDisplay().getMetrics(metrics);
		int density = metrics.densityDpi;

		if (density==DisplayMetrics.DENSITY_XHIGH)
			tabHeight = 65;
		else if (density==DisplayMetrics.DENSITY_HIGH)
			tabHeight = 45;
		else if (density==DisplayMetrics.DENSITY_MEDIUM)
			tabHeight = 35;
		else if (density==DisplayMetrics.DENSITY_LOW)
			tabHeight = 25;
		else
			tabHeight = 65;

		for (int i = 0; i < 5; i++)
			mTabHost.getTabWidget().getChildAt(i).getLayoutParams().height = tabHeight;

		// Set up the custom title
		mTitle = (TextView) findViewById(R.id.title_left_text);
		mTitle.setText(R.string.app_name);
		mTitle = (TextView) findViewById(R.id.title_right_text);

		// Display the Version
		mVersion = (TextView) findViewById(R.id.version);
		mVersion.setText("Version: " + BuildInfo.version +
		                 "  Built: " + BuildInfo.builddate + " " + BuildInfo.buildtime + " " + BuildInfo.buildtz +
		                 "  (" + BuildInfo.branch + "-" + BuildInfo.commitnum + "-" + BuildInfo.commithash + ")");

		mCallsignView  = (TextView) findViewById(R.id.callsign_value);
		mRSSIView      = (TextView) findViewById(R.id.rssi_value);
		mSerialView    = (TextView) findViewById(R.id.serial_value);
		mFlightView    = (TextView) findViewById(R.id.flight_value);
		mStateLayout   = (RelativeLayout) findViewById(R.id.state_container);
		mStateView     = (TextView) findViewById(R.id.state_value);
		mAgeView       = (TextView) findViewById(R.id.age_value);
	}

	@Override
	public void onStart() {
		super.onStart();
		if(D) Log.e(TAG, "++ ON START ++");

		// Start Telemetry Service
		startService(new Intent(AltosDroid.this, TelemetryService.class));

		if (!mBluetoothAdapter.isEnabled()) {
			Intent enableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			startActivityForResult(enableIntent, AltosDroid.REQUEST_ENABLE_BT);
		}

		doBindService();

		if (mAltosVoice == null)
			mAltosVoice = new AltosVoice(this);
	}

	@Override
	public void onResume() {
		super.onResume();
		if(D) Log.e(TAG, "+ ON RESUME +");
	}

	@Override
	public void onPause() {
		super.onPause();
		if(D) Log.e(TAG, "- ON PAUSE -");
	}

	@Override
	public void onStop() {
		super.onStop();
		if(D) Log.e(TAG, "-- ON STOP --");

		doUnbindService();
		if (mAltosVoice != null) {
			mAltosVoice.stop();
			mAltosVoice = null;
		}
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		if(D) Log.e(TAG, "--- ON DESTROY ---");

		if (mAltosVoice != null) mAltosVoice.stop();
		stop_timer();
	}

	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if(D) Log.d(TAG, "onActivityResult " + resultCode);
		switch (requestCode) {
		case REQUEST_CONNECT_DEVICE:
			// When DeviceListActivity returns with a device to connect to
			if (resultCode == Activity.RESULT_OK) {
				connectDevice(data);
			}
			break;
		case REQUEST_ENABLE_BT:
			// When the request to enable Bluetooth returns
			if (resultCode == Activity.RESULT_OK) {
				// Bluetooth is now enabled, so set up a chat session
				//setupChat();
			} else {
				// User did not enable Bluetooth or an error occured
				Log.e(TAG, "BT not enabled");
				stopService(new Intent(AltosDroid.this, TelemetryService.class));
				Toast.makeText(this, R.string.bt_not_enabled, Toast.LENGTH_SHORT).show();
				finish();
			}
			break;
		}
	}

	private void connectDevice(Intent data) {
		// Attempt to connect to the device
		try {
			String address = data.getExtras().getString(DeviceListActivity.EXTRA_DEVICE_ADDRESS);
			String name = data.getExtras().getString(DeviceListActivity.EXTRA_DEVICE_NAME);

			if (D) Log.d(TAG, "Connecting to " + address + " " + name);
			DeviceAddress	a = new DeviceAddress(address, name);
			mService.send(Message.obtain(null, TelemetryService.MSG_CONNECT, a));
			if (D) Log.d(TAG, "Sent connecting message");
		} catch (RemoteException e) {
			if (D) Log.e(TAG, "connect device message failed");
		}
	}

	private void disconnectDevice() {
		try {
			mService.send(Message.obtain(null, TelemetryService.MSG_DISCONNECT, null));
		} catch (RemoteException e) {
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.option_menu, menu);
		return true;
	}

	void setFrequency(double freq) {
		try {
			mService.send(Message.obtain(null, TelemetryService.MSG_SETFREQUENCY, freq));
		} catch (RemoteException e) {
		}
	}

	void setFrequency(String freq) {
		try {
			setFrequency (Double.parseDouble(freq.substring(11, 17)));
		} catch (NumberFormatException e) {
		}
	}

	void setBaud(int baud) {
		try {
			mService.send(Message.obtain(null, TelemetryService.MSG_SETBAUD, baud));
		} catch (RemoteException e) {
		}
	}

	void setBaud(String baud) {
		try {
			int	value = Integer.parseInt(baud);
			int	rate = AltosLib.ao_telemetry_rate_38400;
			switch (value) {
			case 2400:
				rate = AltosLib.ao_telemetry_rate_2400;
				break;
			case 9600:
				rate = AltosLib.ao_telemetry_rate_9600;
				break;
			case 38400:
				rate = AltosLib.ao_telemetry_rate_38400;
				break;
			}
			setBaud(rate);
		} catch (NumberFormatException e) {
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		Intent serverIntent = null;
		switch (item.getItemId()) {
		case R.id.connect_scan:
			// Launch the DeviceListActivity to see devices and do scan
			serverIntent = new Intent(this, DeviceListActivity.class);
			startActivityForResult(serverIntent, REQUEST_CONNECT_DEVICE);
			return true;
		case R.id.disconnect:
			/* Disconnect the bluetooth device
			 */
			disconnectDevice();
			return true;
		case R.id.quit:
			Log.d(TAG, "R.id.quit");
			disconnectDevice();
			finish();
			return true;
		case R.id.select_freq:
			// Set the TBT radio frequency

			final String[] frequencies = {
				"Channel 0 (434.550MHz)",
				"Channel 1 (434.650MHz)",
				"Channel 2 (434.750MHz)",
				"Channel 3 (434.850MHz)",
				"Channel 4 (434.950MHz)",
				"Channel 5 (435.050MHz)",
				"Channel 6 (435.150MHz)",
				"Channel 7 (435.250MHz)",
				"Channel 8 (435.350MHz)",
				"Channel 9 (435.450MHz)"
			};

			AlertDialog.Builder builder_freq = new AlertDialog.Builder(this);
			builder_freq.setTitle("Pick a frequency");
			builder_freq.setItems(frequencies,
					 new DialogInterface.OnClickListener() {
						 public void onClick(DialogInterface dialog, int item) {
							 setFrequency(frequencies[item]);
						 }
					 });
			AlertDialog alert_freq = builder_freq.create();
			alert_freq.show();
			return true;
		case R.id.select_rate:
			// Set the TBT baud rate

			final String[] rates = {
				"38400",
				"9600",
				"2400",
			};

			AlertDialog.Builder builder_rate = new AlertDialog.Builder(this);
			builder_rate.setTitle("Pick a baud rate");
			builder_rate.setItems(rates,
					 new DialogInterface.OnClickListener() {
						 public void onClick(DialogInterface dialog, int item) {
							 setBaud(rates[item]);
						 }
					 });
			AlertDialog alert_rate = builder_rate.create();
			alert_rate.show();
			return true;
		case R.id.change_units:
			boolean	imperial = AltosPreferences.imperial_units();
			AltosPreferences.set_imperial_units(!imperial);
			return true;
		}
		return false;
	}

}
