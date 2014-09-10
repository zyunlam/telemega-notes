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

import org.altusmetrum.altoslib_5.*;
import android.location.Location;
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.app.FragmentManager;
import android.location.Location;
import android.util.Log;
import android.widget.TextView;

public abstract class AltosDroidTab extends Fragment implements AltosUnitsListener {
	AltosState		last_state;
	AltosGreatCircle	last_from_receiver;
	Location		last_receiver;

	public abstract void show(AltosState state, AltosGreatCircle from_receiver, Location receiver);

	public abstract String tab_name();

	public void units_changed(boolean imperial_units) {
		if (!isHidden() && last_state != null)
			show(last_state, last_from_receiver, last_receiver);
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
		if (visible) {
			AltosState		state = last_state;
			AltosGreatCircle	from_receiver = last_from_receiver;
			Location		receiver = last_receiver;

			show(state, from_receiver, receiver);
			ft.show(this);
		} else
			ft.hide(this);
		ft.commit();
	}

	public void update_ui(AltosState state, AltosGreatCircle from_receiver, Location receiver, boolean is_current) {
		last_state = state;
		last_from_receiver = from_receiver;
		last_receiver = receiver;
		if (is_current) {
			if (AltosDroid.D) Log.d(AltosDroid.TAG, String.format("%s: visible, performing update", tab_name()));

			show(state, from_receiver, receiver);
		} else {
			if (AltosDroid.D) Log.d(AltosDroid.TAG, String.format("%s: not visible, skipping update", tab_name()));
			return;
		}
	}
}
