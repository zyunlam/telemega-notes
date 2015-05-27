package org.altusmetrum.AltosDroid;

import org.altusmetrum.altoslib_7.*;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Environment;

public class TelemetryLogger {
	private Context   context = null;
	private AltosLink link    = null;
	private AltosLog  logger  = null;

	private BroadcastReceiver mExternalStorageReceiver;

	public TelemetryLogger(Context in_context, AltosLink in_link) {
		context = in_context;
		link    = in_link;

		startWatchingExternalStorage();
	}

	public void stop() {
		stopWatchingExternalStorage();
		close();
	}

	private void close() {
		if (logger != null) {
			AltosDebug.debug("Shutting down Telemetry Logging");
			logger.close();
			logger = null;
		}
	}
	
	void handleExternalStorageState() {
		String state = Environment.getExternalStorageState();
		if (Environment.MEDIA_MOUNTED.equals(state)) {
			if (logger == null) {
				AltosDebug.debug("Starting up Telemetry Logging");
				logger = new AltosLog(link);
			}
		} else {
			AltosDebug.debug("External Storage not present - stopping");
			close();
		}
	}

	void startWatchingExternalStorage() {
		mExternalStorageReceiver = new BroadcastReceiver() {
			@Override
			public void onReceive(Context context, Intent intent) {
				handleExternalStorageState();
			}
		};
		IntentFilter filter = new IntentFilter();
		filter.addAction(Intent.ACTION_MEDIA_MOUNTED);
		filter.addAction(Intent.ACTION_MEDIA_REMOVED);
		context.registerReceiver(mExternalStorageReceiver, filter);
		handleExternalStorageState();
	}

	void stopWatchingExternalStorage() {
		context.unregisterReceiver(mExternalStorageReceiver);
	}

}
