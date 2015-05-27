/*
 * Copyright © 2015 Keith Packard <keithp@keithp.com>
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

import java.util.Arrays;
import java.io.*;
import java.lang.*;

import org.altusmetrum.altoslib_7.*;

import android.app.Activity;
import android.graphics.*;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;
import android.view.*;
import android.widget.*;
import android.location.Location;
import android.content.*;
import android.util.Log;
import android.os.Looper;

public class AltosDebug {
	// Debugging
	static final String TAG = "AltosDroid";
	public static final boolean D = true;

	static void info(String format, Object ... arguments) {
		Log.i(TAG, String.format(format, arguments));
	}

	static void debug(String format, Object ... arguments) {
		if (D)
			Log.d(TAG, String.format(format, arguments));
	}

	static void error(String format, Object ... arguments) {
		Log.e(TAG, String.format(format, arguments));
	}

	static void check_ui(String format, Object ... arguments) {
		if (Looper.myLooper() == Looper.getMainLooper()) {
			Log.e(TAG, String.format("ON UI THREAD " + format, arguments));
			for (StackTraceElement el : Thread.currentThread().getStackTrace())
				Log.e(TAG, "\t" + el.toString() + "\n");
		}
	}
}
