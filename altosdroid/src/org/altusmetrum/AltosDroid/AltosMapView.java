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

import java.util.*;
import java.io.*;

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
import android.util.*;

public class AltosMapView extends View implements ScaleGestureDetector.OnScaleGestureListener {
	ScaleGestureDetector	scale_detector;
	boolean			scaling;
	TabMapOffline		tab;

	class Line {
		AltosLatLon	a, b;

		void paint() {
			if (a != null && b != null) {
				AltosPointDouble	a_screen = tab.map.transform.screen(a);
				AltosPointDouble	b_screen = tab.map.transform.screen(b);
				tab.paint.setColor(0xff8080ff);
				tab.canvas.drawLine((float) a_screen.x, (float) a_screen.y,
						    (float) b_screen.x, (float) b_screen.y,
						    tab.paint);
			}
		}

		void set_a(AltosLatLon a) {
			this.a = a;
		}

		void set_b(AltosLatLon b) {
			this.b = b;
		}

		Line() {
		}
	}

	Line line = new Line();

	private void draw_bitmap(AltosLatLon lat_lon, Bitmap bitmap, int off_x, int off_y) {
		if (lat_lon != null && tab.map != null && tab.map.transform != null) {
			AltosPointInt pt = new AltosPointInt(tab.map.transform.screen(lat_lon));

			tab.canvas.drawBitmap(bitmap, pt.x - off_x, pt.y - off_y, tab.paint);
		}
	}

	private void draw_positions() {
		line.set_a(tab.map.last_position);
		line.set_b(tab.here);
		line.paint();
		draw_bitmap(tab.pad, tab.pad_bitmap, tab.pad_off_x, tab.pad_off_y);

		Rocket[]	rockets = tab.rockets.values().toArray(new Rocket[0]);

		Arrays.sort(rockets);
		for (Rocket rocket : rockets)
			rocket.paint();
		draw_bitmap(tab.here, tab.here_bitmap, tab.here_off_x, tab.here_off_y);
	}

	@Override public void invalidate() {
		Rect r = new Rect();
		getDrawingRect(r);
		super.invalidate();
	}

	@Override public void invalidate(int l, int t, int r, int b) {
		Rect rect = new Rect();
		getDrawingRect(rect);
		super.invalidate();
	}

	@Override
	protected void onDraw(Canvas view_canvas) {
		tab.canvas = view_canvas;
		tab.paint = new Paint(Paint.ANTI_ALIAS_FLAG);
		tab.paint.setStrokeWidth(tab.stroke_width);
		tab.paint.setStrokeCap(Paint.Cap.ROUND);
		tab.paint.setStrokeJoin(Paint.Join.ROUND);
		tab.paint.setTextSize(40);
		tab.map.paint();
		draw_positions();
		tab.canvas = null;
	}

	public boolean onScale(ScaleGestureDetector detector) {
		float	f = detector.getScaleFactor();
		if (f <= 0.8) {
			tab.map.set_zoom(tab.map.get_zoom() - 1);
			return true;
		}
		if (f >= 1.2) {
			tab.map.set_zoom(tab.map.get_zoom() + 1);
			return true;
		}
		return false;
	}

	public boolean onScaleBegin(ScaleGestureDetector detector) {
		return true;
	}

	public void onScaleEnd(ScaleGestureDetector detector) {
	}

	@Override
	public boolean dispatchTouchEvent(MotionEvent event) {
		scale_detector.onTouchEvent(event);

		if (scale_detector.isInProgress()) {
			scaling = true;
		}

		if (scaling) {
			if (event.getAction() == MotionEvent.ACTION_UP) {
				scaling = false;
			}
			return true;
		}

		if (event.getAction() == MotionEvent.ACTION_DOWN) {
			tab.map.touch_start((int) event.getX(), (int) event.getY(), true);
		} else if (event.getAction() == MotionEvent.ACTION_MOVE) {
			tab.map.touch_continue((int) event.getX(), (int) event.getY(), true);
		}
		return true;
	}

	public void set_tab(TabMapOffline tab) {
		this.tab = tab;
	}

	public AltosMapView(Context context, AttributeSet attrs) {
		super(context, attrs);
		scale_detector = new ScaleGestureDetector(context, this);
	}
}
