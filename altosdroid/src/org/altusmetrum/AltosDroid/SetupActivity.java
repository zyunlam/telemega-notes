/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
import android.bluetooth.*;
import android.content.*;
import android.os.*;
import android.view.*;
import android.view.View.*;
import android.widget.*;
import android.widget.AdapterView.*;

import org.altusmetrum.altoslib_13.*;

public class SetupActivity extends Activity {
	private Spinner select_rate;
	private Spinner set_units;
	private Spinner map_type;
	private Spinner map_source;
	private Button manage_frequencies;
	private Button preload_maps;
	private Button done;

	private boolean is_bound;
	private Messenger service = null;

	public final static String EXTRA_SETUP_CHANGES = "setup_changes";

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

	static final String[] rates = {
		"38400",
		"9600",
		"2400",
	};

	static final String[] map_types = {
		"Hybrid",
		"Satellite",
		"Roadmap",
		"Terrain"
	};

	static final int[] map_type_values = {
		AltosMap.maptype_hybrid,
		AltosMap.maptype_satellite,
		AltosMap.maptype_roadmap,
		AltosMap.maptype_terrain,
	};

	static final String[] map_sources = {
		"Online",
		"Offline"
	};

	private int	set_telemetry_rate;
	private int	set_map_source;
	private int	set_map_type;
	private boolean	set_imperial_units;

	private int	changes = 0;

	private void add_change(int change) {
		changes |= change;
	}

	private void done() {
		Intent intent = new Intent();
		if ((changes & AltosDroid.SETUP_BAUD) != 0)
			AltosPreferences.set_telemetry_rate(1, set_telemetry_rate);
		if ((changes & AltosDroid.SETUP_UNITS) != 0)
			AltosPreferences.set_imperial_units(set_imperial_units);
		if ((changes & AltosDroid.SETUP_MAP_SOURCE) != 0)
			AltosDroidPreferences.set_map_source(set_map_source);
		if ((changes & AltosDroid.SETUP_MAP_TYPE) != 0)
			AltosPreferences.set_map_type(set_map_type);
		intent.putExtra(EXTRA_SETUP_CHANGES, changes);
		setResult(Activity.RESULT_OK, intent);
		finish();
	}

	private void add_strings(Spinner spinner, String[] strings, int def) {
		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item);

		for (int i = 0; i < strings.length; i++)
			adapter.add(strings[i]);

		spinner.setAdapter(adapter);
		if (def >= 0)
			spinner.setSelection(def);
	}

	private int default_rate_pos() {
		int	default_rate = AltosPreferences.telemetry_rate(1);

		for (int pos = 0; pos < rates.length; pos++) {
			if (string_to_rate(rates[pos]) == default_rate)
				return pos;
		}
		return -1;
	}

	private void setBaud(int baud) {
		try {
			service.send(Message.obtain(null, TelemetryService.MSG_SETBAUD, baud));
			set_telemetry_rate = baud;
			add_change(AltosDroid.SETUP_BAUD);
		} catch (RemoteException e) {
		}
	}

	private int string_to_rate(String baud) {
		int	rate = AltosLib.ao_telemetry_rate_38400;
		try {
			int	value = Integer.parseInt(baud);
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
		} catch (NumberFormatException e) {
		}
		return rate;
	}

	private void setBaud(String baud) {
		setBaud(string_to_rate(baud));
	}

	private void select_rate(int pos) {
		setBaud(rates[pos]);
	}

	static final String[] units = {
		"Metric",
		"Imperial"
	};

	private int default_units_pos() {
		boolean	imperial = AltosPreferences.imperial_units();

		if (imperial)
			return 1;
		return 0;
	}

	private void set_units(int pos) {
		switch (pos) {
		default:
			set_imperial_units = false;
			break;
		case 1:
			set_imperial_units = true;
			break;
		}
		add_change(AltosDroid.SETUP_UNITS);
	}

	private int default_map_type_pos() {
		int	default_map_type = AltosPreferences.map_type();

		for (int pos = 0; pos < map_types.length; pos++)
			if (map_type_values[pos] == default_map_type)
				return pos;
		return 0;
	}

	private void select_map_type(int pos) {
		set_map_type = map_type_values[pos];
		add_change(AltosDroid.SETUP_MAP_TYPE);
	}

	private int default_map_source_pos() {
		int	default_source = AltosDroidPreferences.map_source();

		switch (default_source) {
		case AltosDroidPreferences.MAP_SOURCE_OFFLINE:
			return 1;
		default:
			return 0;
		}
	}

	private void select_map_source(int pos) {
		switch (pos) {
		default:
			set_map_source = AltosDroidPreferences.MAP_SOURCE_ONLINE;
			break;
		case 1:
			set_map_source = AltosDroidPreferences.MAP_SOURCE_OFFLINE;
			break;
		}
		add_change(AltosDroid.SETUP_MAP_SOURCE);
	}

	private void manage_frequencies(){
		Intent intent = new Intent(this, ManageFrequenciesActivity.class);
		startActivity(intent);
	}

	private void preload_maps(){
		Intent intent = new Intent(this, PreloadMapActivity.class);
		startActivity(intent);
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		AltosDebug.init(this);
		AltosDebug.debug("+++ ON CREATE +++");

		// Initialise preferences
		AltosDroidPreferences.init(this);

		// Setup the window
		requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
		setContentView(R.layout.setup);

		select_rate = (Spinner) findViewById(R.id.select_rate);
		add_strings(select_rate, rates, default_rate_pos());
		select_rate.setOnItemSelectedListener(new OnItemSelectedListener() {
				public void onItemSelected(AdapterView<?> parent, View view, int pos, long id) {
					select_rate(pos);
				}
				public void onNothingSelected(AdapterView<?> parent) {
				}
			});

		set_units = (Spinner) findViewById(R.id.set_units);
		add_strings(set_units, units, default_units_pos());
		set_units.setOnItemSelectedListener(new OnItemSelectedListener() {
				public void onItemSelected(AdapterView<?> parent, View view, int pos, long id) {
					set_units(pos);
				}
				public void onNothingSelected(AdapterView<?> parent) {
				}
			});

		map_type = (Spinner) findViewById(R.id.map_type);
		add_strings(map_type, map_types, default_map_type_pos());
		map_type.setOnItemSelectedListener(new OnItemSelectedListener() {
				public void onItemSelected(AdapterView<?> parent, View view, int pos, long id) {
					select_map_type(pos);
				}
				public void onNothingSelected(AdapterView<?> parent) {
				}
			});

		map_source = (Spinner) findViewById(R.id.map_source);
		add_strings(map_source, map_sources, default_map_source_pos());
		map_source.setOnItemSelectedListener(new OnItemSelectedListener() {
				public void onItemSelected(AdapterView<?> parent, View view, int pos, long id) {
					select_map_source(pos);
				}
				public void onNothingSelected(AdapterView<?> parent) {
				}
			});


		manage_frequencies = (Button) findViewById(R.id.manage_frequencies);
		manage_frequencies.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					manage_frequencies();
				}
			});

		preload_maps = (Button) findViewById(R.id.preload_maps);
		preload_maps.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					preload_maps();
				}
			});

		done = (Button) findViewById(R.id.done);
		done.setOnClickListener(new OnClickListener() {
				public void onClick(View v) {
					done();
				}
			});

		// Set result for when the user backs out
		setResult(Activity.RESULT_CANCELED);
	}

	@Override
	protected void onStart() {
		super.onStart();
		doBindService();
	}

	@Override
	protected void onStop() {
		super.onStop();
		doUnbindService();
	}
}
