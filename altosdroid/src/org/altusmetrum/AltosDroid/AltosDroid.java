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
import java.text.*;
import java.util.*;
import java.io.*;

import android.app.Activity;
import android.app.PendingIntent;
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
import android.content.res.Resources;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.util.DisplayMetrics;
import android.view.*;
import android.widget.*;
import android.app.AlertDialog;
import android.location.Location;
import android.hardware.usb.*;
import android.graphics.*;
import android.graphics.drawable.*;

import org.altusmetrum.altoslib_7.*;

public class AltosDroid extends FragmentActivity implements AltosUnitsListener {

	// Actions sent to the telemetry server at startup time

	public static final String ACTION_BLUETOOTH = "org.altusmetrum.AltosDroid.BLUETOOTH";
	public static final String ACTION_USB = "org.altusmetrum.AltosDroid.USB";

	// Message types received by our Handler

	public static final int MSG_STATE           = 1;
	public static final int MSG_UPDATE_AGE      = 2;

	// Intent request codes
	public static final int REQUEST_CONNECT_DEVICE = 1;
	public static final int REQUEST_ENABLE_BT      = 2;
	public static final int REQUEST_PRELOAD_MAPS   = 3;
	public static final int REQUEST_MAP_TYPE       = 4;

	public int map_type = AltosMap.maptype_hybrid;

	public static FragmentManager	fm;

	private BluetoothAdapter mBluetoothAdapter = null;

	// Flight state values
	private TextView mCallsignView;
	private TextView mRSSIView;
	private TextView mSerialView;
	private TextView mFlightView;
	private RelativeLayout mStateLayout;
	private TextView mStateView;
	private TextView mAgeView;
	private boolean  mAgeViewOld;
	private int mAgeNewColor;
	private int mAgeOldColor;

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
	TelemetryState	telemetry_state;
	Integer[] 	serials;

	UsbDevice	pending_usb_device;
	boolean		start_with_usb;

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
				AltosDebug.debug("MSG_STATE");
				if (msg.obj == null) {
					AltosDebug.debug("telemetry_state null!");
					return;
				}
				ad.update_state((TelemetryState) msg.obj);
				break;
			case MSG_UPDATE_AGE:
				AltosDebug.debug("MSG_UPDATE_AGE");
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
			if (pending_usb_device != null) {
				try {
					mService.send(Message.obtain(null, TelemetryService.MSG_OPEN_USB, pending_usb_device));
					pending_usb_device = null;
				} catch (RemoteException e) {
				}
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
				setTitle(str);
			} else {
				setTitle(R.string.title_connected_to);
			}
			break;
		case TelemetryState.CONNECT_CONNECTING:
			if (telemetry_state.address != null)
				setTitle(String.format("Connecting to %s...", telemetry_state.address.name));
			else
				setTitle("Connecting to something...");
			break;
		case TelemetryState.CONNECT_DISCONNECTED:
		case TelemetryState.CONNECT_NONE:
			setTitle(R.string.title_not_connected);
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

	int	selected_serial = 0;
	int	current_serial;
	long	switch_time;

	void set_switch_time() {
		switch_time = System.currentTimeMillis();
		selected_serial = 0;
	}

	boolean	registered_units_listener;

	void update_state(TelemetryState new_telemetry_state) {

		if (new_telemetry_state != null)
			telemetry_state = new_telemetry_state;

		if (selected_serial != 0)
			current_serial = selected_serial;

		if (current_serial == 0)
			current_serial = telemetry_state.latest_serial;

		if (!registered_units_listener) {
			registered_units_listener = true;
			AltosPreferences.register_units_listener(this);
		}

		serials = telemetry_state.states.keySet().toArray(new Integer[0]);
		Arrays.sort(serials);

		update_title(telemetry_state);

		AltosState	state = null;
		boolean		aged = true;

		if (telemetry_state.states.containsKey(current_serial)) {
			state = telemetry_state.states.get(current_serial);
			int age = state_age(state);
			if (age < 20)
				aged = false;
			if (current_serial == selected_serial)
				aged = false;
			else if (switch_time != 0 && (switch_time - state.received_time) > 0)
				aged = true;
		}

		if (aged) {
			AltosState	newest_state = null;
			int		newest_age = 0;

			for (int serial : telemetry_state.states.keySet()) {
				AltosState	existing = telemetry_state.states.get(serial);
				int		existing_age = state_age(existing);

				if (newest_state == null || existing_age < newest_age) {
					newest_state = existing;
					newest_age = existing_age;
				}
			}

			if (newest_state != null)
				state = newest_state;
		}

		update_ui(telemetry_state, state, telemetry_state.location);

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


	private int blend_component(int a, int b, double r, int shift, int mask) {
		return ((int) (((a >> shift) & mask) * r + ((b >> shift) & mask) * (1 - r)) & mask) << shift;
	}
	private int blend_color(int a, int b, double r) {
		return (blend_component(a, b, r, 0, 0xff) |
			blend_component(a, b, r, 8, 0xff) |
			blend_component(a, b, r, 16, 0xff) |
			blend_component(a, b, r, 24, 0xff));
	}

	int state_age(AltosState state) {
		return (int) ((System.currentTimeMillis() - state.received_time + 500) / 1000);
	}

	void set_screen_on(int age) {
		if (age < 60)
			getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
		else
			getWindow().clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
	}

	void update_age() {
		if (saved_state != null) {
			int age = state_age(saved_state);

			double age_scale = age / 100.0;

			if (age_scale > 1.0)
				age_scale = 1.0;

			mAgeView.setTextColor(blend_color(mAgeOldColor, mAgeNewColor, age_scale));

			set_screen_on(age);

			String	text;
			if (age < 60)
				text = String.format("%ds", age);
			else if (age < 60 * 60)
				text = String.format("%dm", age / 60);
			else if (age < 60 * 60 * 24)
				text = String.format("%dh", age / (60 * 60));
			else
				text = String.format("%dd", age / (24 * 60 * 60));
			mAgeView.setText(text);
		}
	}

	void update_ui(TelemetryState telem_state, AltosState state, Location location) {

		int prev_state = AltosLib.ao_flight_invalid;

		AltosGreatCircle from_receiver = null;

		if (saved_state != null)
			prev_state = saved_state.state;

		if (state != null) {
			set_screen_on(state_age(state));

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
						if (currentTab.equals("pad")) mTabHost.setCurrentTabByTag("flight");
					} else {
						if (currentTab.equals("flight")) mTabHost.setCurrentTabByTag("pad");
					}
				}
			} else {
				if (prev_state != state.state) {
					String currentTab = mTabHost.getCurrentTabTag();
					switch (state.state) {
					case AltosLib.ao_flight_boost:
						if (currentTab.equals("pad")) mTabHost.setCurrentTabByTag("flight");
						break;
					case AltosLib.ao_flight_landed:
						if (currentTab.equals("flight")) mTabHost.setCurrentTabByTag("recover");
						break;
					case AltosLib.ao_flight_stateless:
						if (currentTab.equals("pad")) mTabHost.setCurrentTabByTag("flight");
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
			mTab.update_ui(telem_state, state, from_receiver, location, mTab == mTabsAdapter.currentItem());

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

	private View create_tab_view(String label) {
		LayoutInflater inflater = (LayoutInflater) this.getLayoutInflater();
		View tab_view = inflater.inflate(R.layout.tab_layout, null);
		TextView text_view = (TextView) tab_view.findViewById (R.id.tabLabel);
		text_view.setText(label);
		return tab_view;
	}

	public void set_map_source(int source) {
		for (AltosDroidTab mTab : mTabs)
			mTab.set_map_source(source);
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		AltosDebug.init(this);
		AltosDebug.debug("+++ ON CREATE +++");

		// Initialise preferences
		AltosDroidPreferences.init(this);

		fm = getSupportFragmentManager();

		// Set up the window layout
		setContentView(R.layout.altosdroid);

		// Create the Tabs and ViewPager
		mTabHost = (TabHost)findViewById(android.R.id.tabhost);
		mTabHost.setup();

		mViewPager = (AltosViewPager)findViewById(R.id.pager);
		mViewPager.setOffscreenPageLimit(4);

		mTabsAdapter = new TabsAdapter(this, mTabHost, mViewPager);

		mTabsAdapter.addTab(mTabHost.newTabSpec("pad").setIndicator(create_tab_view("Pad")), TabPad.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("flight").setIndicator(create_tab_view("Flight")), TabFlight.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("recover").setIndicator(create_tab_view("Recover")), TabRecover.class, null);
		mTabsAdapter.addTab(mTabHost.newTabSpec("map").setIndicator(create_tab_view("Map")), TabMap.class, null);

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
		mAgeNewColor   = mAgeView.getTextColors().getDefaultColor();
		mAgeOldColor   = getResources().getColor(R.color.old_color);
	}

	private boolean ensureBluetooth() {
		// Get local Bluetooth adapter
		mBluetoothAdapter = BluetoothAdapter.getDefaultAdapter();

		// If the adapter is null, then Bluetooth is not supported
		if (mBluetoothAdapter == null) {
			Toast.makeText(this, "Bluetooth is not available", Toast.LENGTH_LONG).show();
			return false;
		}

		if (!mBluetoothAdapter.isEnabled()) {
			Intent enableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			startActivityForResult(enableIntent, AltosDroid.REQUEST_ENABLE_BT);
		}

		return true;
	}

	private boolean check_usb() {
		UsbDevice	device = AltosUsb.find_device(this, AltosLib.product_basestation);

		if (device != null) {
			Intent		i = new Intent(this, AltosDroid.class);
			PendingIntent	pi = PendingIntent.getActivity(this, 0, new Intent("hello world", null, this, AltosDroid.class), 0);

			if (AltosUsb.request_permission(this, device, pi)) {
				connectUsb(device);
			}
			start_with_usb = true;
			return true;
		}

		start_with_usb = false;

		return false;
	}

	private void noticeIntent(Intent intent) {

		/* Ok, this is pretty convenient.
		 *
		 * When a USB device is plugged in, and our 'hotplug'
		 * intent registration fires, we get an Intent with
		 * EXTRA_DEVICE set.
		 *
		 * When we start up and see a usb device and request
		 * permission to access it, that queues a
		 * PendingIntent, which has the EXTRA_DEVICE added in,
		 * along with the EXTRA_PERMISSION_GRANTED field as
		 * well.
		 *
		 * So, in both cases, we get the device name using the
		 * same call. We check to see if access was granted,
		 * in which case we ignore the device field and do our
		 * usual startup thing.
		 */

		UsbDevice device = (UsbDevice) intent.getParcelableExtra(UsbManager.EXTRA_DEVICE);
		boolean granted = intent.getBooleanExtra(UsbManager.EXTRA_PERMISSION_GRANTED, true);

		AltosDebug.debug("intent %s device %s granted %s", intent, device, granted);

		if (!granted)
			device = null;

		if (device != null) {
			AltosDebug.debug("intent has usb device " + device.toString());
			connectUsb(device);
		} else {

			/* 'granted' is only false if this intent came
			 * from the request_permission call and
			 * permission was denied. In which case, we
			 * don't want to loop forever...
			 */
			if (granted) {
				AltosDebug.debug("check for a USB device at startup");
				if (check_usb())
					return;
			}
			AltosDebug.debug("Starting by looking for bluetooth devices");
			if (ensureBluetooth())
				return;
			finish();
		}
	}

	@Override
	public void onStart() {
		super.onStart();
		AltosDebug.debug("++ ON START ++");

		noticeIntent(getIntent());

		// Start Telemetry Service
		String	action = start_with_usb ? ACTION_USB : ACTION_BLUETOOTH;

		startService(new Intent(action, null, AltosDroid.this, TelemetryService.class));

		doBindService();

		if (mAltosVoice == null)
			mAltosVoice = new AltosVoice(this);

	}

	@Override
	public void onNewIntent(Intent intent) {
		super.onNewIntent(intent);
		AltosDebug.debug("onNewIntent");
		noticeIntent(intent);
	}

	@Override
	public void onResume() {
		super.onResume();
		AltosDebug.debug("+ ON RESUME +");
	}

	@Override
	public void onPause() {
		super.onPause();
		AltosDebug.debug("- ON PAUSE -");
	}

	@Override
	public void onStop() {
		super.onStop();
		AltosDebug.debug("-- ON STOP --");

		doUnbindService();
		if (mAltosVoice != null) {
			mAltosVoice.stop();
			mAltosVoice = null;
		}
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		AltosDebug.debug("--- ON DESTROY ---");

		if (mAltosVoice != null) mAltosVoice.stop();
		stop_timer();
	}

	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		AltosDebug.debug("onActivityResult " + resultCode);
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
				AltosDebug.error("BT not enabled");
				stopService(new Intent(AltosDroid.this, TelemetryService.class));
				Toast.makeText(this, R.string.bt_not_enabled, Toast.LENGTH_SHORT).show();
				finish();
			}
			break;
		case REQUEST_MAP_TYPE:
			if (resultCode == Activity.RESULT_OK)
				set_map_type(data);
			break;
		}
	}

	private void connectUsb(UsbDevice device) {
		if (mService == null)
			pending_usb_device = device;
		else {
			// Attempt to connect to the device
			try {
				mService.send(Message.obtain(null, TelemetryService.MSG_OPEN_USB, device));
				AltosDebug.debug("Sent OPEN_USB message");
			} catch (RemoteException e) {
				AltosDebug.debug("connect device message failed");
			}
		}
	}

	private void connectDevice(Intent data) {
		// Attempt to connect to the device
		try {
			String address = data.getExtras().getString(DeviceListActivity.EXTRA_DEVICE_ADDRESS);
			String name = data.getExtras().getString(DeviceListActivity.EXTRA_DEVICE_NAME);

			AltosDebug.debug("Connecting to " + address + " " + name);
			DeviceAddress	a = new DeviceAddress(address, name);
			mService.send(Message.obtain(null, TelemetryService.MSG_CONNECT, a));
			AltosDebug.debug("Sent connecting message");
		} catch (RemoteException e) {
			AltosDebug.debug("connect device message failed");
		}
	}

	private void disconnectDevice() {
		try {
			mService.send(Message.obtain(null, TelemetryService.MSG_DISCONNECT, null));
		} catch (RemoteException e) {
		}
	}

	private void set_map_type(Intent data) {
		int type = data.getIntExtra(MapTypeActivity.EXTRA_MAP_TYPE, -1);

		AltosDebug.debug("intent set_map_type %d\n", type);
		if (type != -1) {
			map_type = type;
			for (AltosDroidTab mTab : mTabs)
				mTab.set_map_type(map_type);
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
			set_switch_time();
		} catch (RemoteException e) {
		}
	}

	void setFrequency(String freq) {
		try {
			setFrequency (AltosParse.parse_double_net(freq.substring(11, 17)));
		} catch (ParseException e) {
		}
	}

	void setBaud(int baud) {
		try {
			mService.send(Message.obtain(null, TelemetryService.MSG_SETBAUD, baud));
			set_switch_time();
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

	void select_tracker(int serial) {
		int i;

		AltosDebug.debug("select tracker %d\n", serial);

		if (serial == selected_serial) {
			AltosDebug.debug("%d already selected\n", serial);
			return;
		}

		if (serial != 0) {
			for (i = 0; i < serials.length; i++)
				if (serials[i] == serial)
					break;

			if (i == serials.length) {
				AltosDebug.debug("attempt to select unknown tracker %d\n", serial);
				return;
			}
		}

		current_serial = selected_serial = serial;
		update_state(null);
	}

	void delete_track(int serial) {
		try {
			mService.send(Message.obtain(null, TelemetryService.MSG_DELETE_SERIAL, (Integer) serial));
		} catch (Exception ex) {
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		Intent serverIntent = null;
		switch (item.getItemId()) {
		case R.id.connect_scan:
			if (ensureBluetooth()) {
				// Launch the DeviceListActivity to see devices and do scan
				serverIntent = new Intent(this, DeviceListActivity.class);
				startActivityForResult(serverIntent, REQUEST_CONNECT_DEVICE);
			}
			return true;
		case R.id.disconnect:
			/* Disconnect the device
			 */
			disconnectDevice();
			return true;
		case R.id.quit:
			AltosDebug.debug("R.id.quit");
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
		case R.id.preload_maps:
			serverIntent = new Intent(this, PreloadMapActivity.class);
			startActivityForResult(serverIntent, REQUEST_PRELOAD_MAPS);
			return true;
		case R.id.map_type:
			serverIntent = new Intent(this, MapTypeActivity.class);
			startActivityForResult(serverIntent, REQUEST_MAP_TYPE);
			return true;
		case R.id.map_source:
			int source = AltosDroidPreferences.map_source();
			int new_source = source == AltosDroidPreferences.MAP_SOURCE_ONLINE ? AltosDroidPreferences.MAP_SOURCE_OFFLINE : AltosDroidPreferences.MAP_SOURCE_ONLINE;
			AltosDroidPreferences.set_map_source(new_source);
			set_map_source(new_source);
			return true;
		case R.id.select_tracker:
			if (serials != null) {
				String[] trackers = new String[serials.length+1];
				trackers[0] = "Auto";
				for (int i = 0; i < serials.length; i++)
					trackers[i+1] = String.format("%d", serials[i]);
				AlertDialog.Builder builder_serial = new AlertDialog.Builder(this);
				builder_serial.setTitle("Select a tracker");
				builder_serial.setItems(trackers,
							new DialogInterface.OnClickListener() {
								public void onClick(DialogInterface dialog, int item) {
									if (item == 0)
										select_tracker(0);
									else
										select_tracker(serials[item-1]);
								}
							});
				AlertDialog alert_serial = builder_serial.create();
				alert_serial.show();

			}
			return true;
		case R.id.delete_track:
			if (serials != null) {
				String[] trackers = new String[serials.length];
				for (int i = 0; i < serials.length; i++)
					trackers[i] = String.format("%d", serials[i]);
				AlertDialog.Builder builder_serial = new AlertDialog.Builder(this);
				builder_serial.setTitle("Delete a track");
				builder_serial.setItems(trackers,
							new DialogInterface.OnClickListener() {
								public void onClick(DialogInterface dialog, int item) {
									delete_track(serials[item]);
								}
							});
				AlertDialog alert_serial = builder_serial.create();
				alert_serial.show();

			}
			return true;
		}
		return false;
	}
}
