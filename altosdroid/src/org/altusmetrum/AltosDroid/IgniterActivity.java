/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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
import java.util.*;
import org.altusmetrum.AltosDroid.R;

import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.*;
import android.graphics.*;
import android.os.*;
import android.view.*;
import android.view.View.*;
import android.widget.*;
import android.widget.AdapterView.*;

import org.altusmetrum.altoslib_10.*;

class IgniterItem {
	public String name;
	public String pretty;
	public String status;
	public LinearLayout igniter_view = null;
	public TextView	pretty_view = null;
	public TextView	status_view = null;

	private void update() {
		AltosDebug.debug("update item %s %s", pretty, status);
		if (pretty_view != null)
			pretty_view.setText(pretty);
		if (status_view != null)
			status_view.setText(status);
	}

	public void set(String name, String pretty, String status) {
		if (!name.equals(this.name) ||
		    !pretty.equals(this.pretty) ||
		    !status.equals(this.status))
		{
			this.name = name;
			this.pretty = pretty;
			this.status = status;
			update();
		}
	}

	public void realize(LinearLayout igniter_view,
			    TextView pretty_view,
			    TextView status_view) {
		if (igniter_view != this.igniter_view ||
		    pretty_view != this.pretty_view ||
		    status_view != this.status_view)
		{
			this.igniter_view = igniter_view;
			this.pretty_view = pretty_view;
			this.status_view = status_view;
			update();
		}
	}

	public IgniterItem() {
		AltosDebug.debug("New igniter item");
	}
}

class IgniterAdapter extends ArrayAdapter<IgniterItem> {
	int resource;
	int selected_item = -1;

	public IgniterAdapter(Context context, int in_resource) {
		super(context, in_resource);
		resource = in_resource;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		IgniterItem item = getItem(position);
		if (item.igniter_view == null) {
			LinearLayout igniter_view = new LinearLayout(getContext());
			String inflater = Context.LAYOUT_INFLATER_SERVICE;
			LayoutInflater li = (LayoutInflater) getContext().getSystemService(inflater);
			li.inflate(resource, igniter_view, true);

			item.realize(igniter_view,
				     (TextView) igniter_view.findViewById(R.id.igniter_name),
				     (TextView) igniter_view.findViewById(R.id.igniter_status));
			AltosDebug.debug("Realize new igniter view");
		} else
			AltosDebug.debug("Reuse existing igniter view");
		if (position == selected_item)
			item.igniter_view.setBackgroundColor(Color.RED);
		else
			item.igniter_view.setBackgroundColor(Color.BLACK);
		return item.igniter_view;
	}
}

public class IgniterActivity extends Activity {
	private ListView igniters_view;
	private ToggleButton arm;
	private Button fire;

	private HashMap<String,IgniterItem> igniters = new HashMap<String,IgniterItem>();;

	private IgniterAdapter igniters_adapter;

	private boolean is_bound;
	private boolean timer_running;
	private Messenger service = null;
	private final Messenger messenger = new Messenger(new IncomingHandler(this));

	private Timer timer;

	private Timer arm_timer;
	private int arm_remaining;

	public static final int IGNITER_QUERY = 1;
	public static final int IGNITER_FIRE = 2;

	// The Handler that gets information back from the Telemetry Service
	static class IncomingHandler extends Handler {
		private final WeakReference<IgniterActivity> igniter_activity;
		IncomingHandler(IgniterActivity ia) { igniter_activity = new WeakReference<IgniterActivity>(ia); }

		@Override
		public void handleMessage(Message msg) {
			IgniterActivity ia = igniter_activity.get();

			switch (msg.what) {
			case AltosDroid.MSG_IGNITER_STATUS:
				ia.igniter_status((HashMap <String,Integer>) msg.obj);
				break;
			}
		}
	};


	private ServiceConnection connection = new ServiceConnection() {
		public void onServiceConnected(ComponentName className, IBinder binder) {
			service = new Messenger(binder);
		}

		public void onServiceDisconnected(ComponentName className) {
			// This is called when the connection with the service has been unexpectedly disconnected - process crashed.
			service = null;
		}
	};

	void doBindService() {
		bindService(new Intent(this, TelemetryService.class), connection, Context.BIND_AUTO_CREATE);
		is_bound = true;
	}

	void doUnbindService() {
		if (is_bound) {
			// If we have received the service, and hence registered with it, then now is the time to unregister.
			unbindService(connection);
			is_bound = false;
		}
	}

	private void done() {
		Intent intent = new Intent();
		setResult(Activity.RESULT_OK, intent);
		finish();
	}

	class FireThread extends Thread {
		private final String igniter;

		@Override
		public void run() {
			Message msg = Message.obtain(null, TelemetryService.MSG_IGNITER_FIRE, igniter);
			try {
				service.send(msg);
			} catch (RemoteException re) {
			}
		}

		public FireThread(String igniter) {
			this.igniter = igniter;
		}
	}

	private void fire_igniter() {
		if (igniters_adapter.selected_item >= 0) {
			IgniterItem	item = igniters_adapter.getItem(igniters_adapter.selected_item);
			FireThread	ft = new FireThread(item.name);
			ft.run();
		}
	}

	private void arm_igniter(boolean is_checked) {
		if (is_checked) {
			arm_timer = new Timer();
			fire.setEnabled(true);
			arm_timer.scheduleAtFixedRate(new TimerTask() {
					public void run() {
						arm_timer_tick();
					}},
				1000L, 1000L);
		} else {
			arm_timer.cancel();
			fire.setEnabled(false);
		}
	}

	private synchronized void timer_tick() {
		if (timer_running)
			return;
		timer_running = true;
		try {
			Message msg = Message.obtain(null, TelemetryService.MSG_IGNITER_QUERY);
			msg.replyTo = messenger;
			service.send(msg);
		} catch (RemoteException re) {
			timer_running = false;
		}
	}

	private boolean set_igniter(HashMap <String,Integer> status, String name, String pretty) {
		if (!status.containsKey(name))
			return false;

		IgniterItem item;
		if (!igniters.containsKey(name)) {
			item = new IgniterItem();
			igniters.put(name, item);
			igniters_adapter.add(item);
		} else
			item = igniters.get(name);

		item.set(name, pretty, AltosIgnite.status_string(status.get(name)));
		return true;
	}

	private synchronized void igniter_status(HashMap <String,Integer> status) {
		timer_running = false;
		if (status == null) {
			AltosDebug.debug("no igniter status");
			return;
		}
		set_igniter(status, "drogue", "Apogee");
		set_igniter(status, "main", "Main");
		for (int extra = 0;; extra++) {
			String	name = String.format("%d", extra);
			String	pretty = String.format("%c", 'A' + extra);
			if (!set_igniter(status, name, pretty))
				break;
		}
//		if (igniters_adapter.selected_item >= 0)
//			igniters_view.setSelection(selected_item);
	}

	private void arm_set_text() {
		String	text = String.format("Armed %d", arm_remaining);

		arm.setTextOn(text);
	}

	private void arm_timer_tick() {
		--arm_remaining;
		if (arm_remaining <= 0) {
			timer.cancel();
			arm.setChecked(false);
			fire.setEnabled(false);
		} else
			arm_set_text();
	}

	private void select_item(int position) {
		if (position != igniters_adapter.selected_item) {
			if (igniters_adapter.selected_item >= 0)
				igniters_view.setItemChecked(igniters_adapter.selected_item, false);
			if (position >= 0) {
				igniters_view.setItemChecked(position, true);
				arm.setEnabled(true);
				arm_remaining = 10;
				arm_set_text();
			} else
				arm.setEnabled(false);
			igniters_adapter.selected_item = position;
		}
	}

	private class IgniterItemClickListener implements ListView.OnItemClickListener {
		@Override
		public void onItemClick(AdapterView<?> av, View v, int position, long id) {
			select_item(position);
		}
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Setup the window
		requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
		setContentView(R.layout.igniters);

		igniters_view = (ListView) findViewById(R.id.igniters);
		igniters_view.setClickable(true);

		igniters_adapter = new IgniterAdapter(this, R.layout.igniter_status);

		igniters_view.setAdapter(igniters_adapter);
		igniters_view.setOnItemClickListener(new IgniterItemClickListener());

		fire = (Button) findViewById(R.id.igniter_fire);
		fire.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					fire_igniter();
				}
			});

		arm = (ToggleButton) findViewById(R.id.igniter_arm);
		arm.setEnabled(false);
		arm.setOnCheckedChangeListener(new ToggleButton.OnCheckedChangeListener() {
				public void onCheckedChanged(CompoundButton v, boolean is_checked) {
					arm_igniter(is_checked);
				}
			});

		// Set result CANCELED incase the user backs out
		setResult(Activity.RESULT_CANCELED);
	}

	@Override
	protected void onStart() {
		super.onStart();
		doBindService();
	}

	@Override
	protected void onResume() {
		super.onResume();
		timer = new Timer(true);
		timer.scheduleAtFixedRate(new TimerTask() {
				public void run() {
					timer_tick();
				}},
			1000L, 1000L);
	}

	@Override
	protected void onPause() {
		super.onPause();
		timer.cancel();
		timer = null;
	}

	@Override
	protected void onStop() {
		super.onStop();
		doUnbindService();
	}
}
