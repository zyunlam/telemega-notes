/*
 * Copyright © 2013 Mike Beattie <mike@ethernal.org>
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

import org.altusmetrum.altoslib_7.*;
import android.location.Location;
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.app.FragmentManager;
import android.location.Location;
import android.widget.TextView;

public abstract class AltosDroidTab extends Fragment implements AltosUnitsListener {
	TelemetryState		last_telem_state;
	AltosState		last_state;
	AltosGreatCircle	last_from_receiver;
	Location		last_receiver;
	AltosDroid		altos_droid;

	public abstract void show(TelemetryState telem_state, AltosState state, AltosGreatCircle from_receiver, Location receiver);

	public abstract String tab_name();

	public void set_map_type(int map_type) {
	}

	public void units_changed(boolean imperial_units) {
		if (!isHidden())
			show(last_telem_state, last_state, last_from_receiver, last_receiver);
	}

	public void set_value(TextView text_view,
			      AltosUnits units,
			      int width,
			      double value) {
		if (value == AltosLib.MISSING)
			text_view.setText("");
		else
			text_view.setText(units.show(width, value));
	}

	public void set_visible(boolean visible) {
		FragmentTransaction	ft = AltosDroid.fm.beginTransaction();
		AltosDebug.debug("set visible %b %s\n", visible, tab_name());
		if (visible) {
			ft.show(this);
			show(last_telem_state, last_state, last_from_receiver, last_receiver);
		} else
			ft.hide(this);
		ft.commitAllowingStateLoss();
	}

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		altos_droid = (AltosDroid) activity;
		altos_droid.registerTab(this);
	}

	@Override
	public void onDetach() {
		super.onDetach();
		altos_droid.unregisterTab(this);
		altos_droid = null;
	}

	@Override
	public void onResume() {
		super.onResume();
		AltosDebug.debug("onResume tab %s\n", tab_name());
		set_visible(true);
	}

	public void update_ui(TelemetryState telem_state, AltosState state,
			      AltosGreatCircle from_receiver, Location receiver, boolean is_current)
	{
		last_telem_state = telem_state;
		last_state = state;
		last_from_receiver = from_receiver;
		last_receiver = receiver;
		AltosDebug.debug("update_ui tab %s is_current %b\n", tab_name(), is_current);
		if (is_current)
			show(telem_state, state, from_receiver, receiver);
		else
			return;
	}
}
