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

import java.util.*;

import org.altusmetrum.altoslib_7.*;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;

import android.app.Activity;
import android.graphics.Color;
import android.graphics.*;
import android.os.Bundle;
import android.support.v4.app.Fragment;
//import android.support.v4.app.FragmentTransaction;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.location.Location;
import android.content.*;

class RocketOnline implements Comparable {
	Marker		marker;
	int		serial;
	long		last_packet;

	void set_position(AltosLatLon position, long last_packet) {
		marker.setPosition(new LatLng(position.lat, position.lon));
		this.last_packet = last_packet;
	}

	private Bitmap rocket_bitmap(Context context, String text) {

		/* From: http://mapicons.nicolasmollet.com/markers/industry/military/missile-2/
		 */
		Bitmap orig_bitmap = BitmapFactory.decodeResource(context.getResources(), R.drawable.rocket);
		Bitmap bitmap = orig_bitmap.copy(Bitmap.Config.ARGB_8888, true);

		Canvas canvas = new Canvas(bitmap);
		Paint paint = new Paint();
		paint.setTextSize(40);
		paint.setColor(0xff000000);

		Rect	bounds = new Rect();
		paint.getTextBounds(text, 0, text.length(), bounds);

		int	width = bounds.right - bounds.left;
		int	height = bounds.bottom - bounds.top;

		float x = bitmap.getWidth() / 2.0f - width / 2.0f;
		float y = bitmap.getHeight() / 2.0f - height / 2.0f;

		canvas.drawText(text, 0, text.length(), x, y, paint);
		return bitmap;
	}

	public void remove() {
		marker.remove();
	}

	public int compareTo(Object o) {
		RocketOnline other = (RocketOnline) o;

		long	diff = last_packet - other.last_packet;

		if (diff > 0)
			return 1;
		if (diff < 0)
			return -1;
		return 0;
	}

	RocketOnline(Context context, int serial, GoogleMap map, double lat, double lon, long last_packet) {
		this.serial = serial;
		String name = String.format("%d", serial);
		this.marker = map.addMarker(new MarkerOptions()
					    .icon(BitmapDescriptorFactory.fromBitmap(rocket_bitmap(context, name)))
					    .position(new LatLng(lat, lon))
					    .visible(true));
		this.last_packet = last_packet;
	}
}

public class AltosMapOnline implements AltosDroidMapInterface, GoogleMap.OnMarkerClickListener {
	public SupportMapFragment mMapFragment;
	private GoogleMap mMap;
	private boolean mapLoaded = false;
	Context context;

	private HashMap<Integer,RocketOnline> rockets = new HashMap<Integer,RocketOnline>();
	private Marker mPadMarker;
	private boolean pad_set;
	private Polyline mPolyline;

	private View map_view;

	private double mapAccuracy = -1;

	private AltosLatLon my_position = null;
	private AltosLatLon target_position = null;

	private AltosDroid altos_droid;

	public void onCreateView(AltosDroid altos_droid) {
		this.altos_droid = altos_droid;
		final int map_type = altos_droid.map_type;
		mMapFragment = new SupportMapFragment() {
			@Override
			public void onActivityCreated(Bundle savedInstanceState) {
				super.onActivityCreated(savedInstanceState);
				setupMap(map_type);
			}
			@Override
			public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
				map_view = super.onCreateView(inflater, container, savedInstanceState);
				return map_view;
			}
			@Override
			public void onDestroyView() {
				super.onDestroyView();
				map_view = null;
			}
		};
	}

//	public void onActivityCreated() {
//		getChildFragmentManager().beginTransaction().add(R.id.map, mMapFragment).commit();
//	}

	public boolean onMarkerClick(Marker marker) {
		for (RocketOnline rocket : rockets.values()) {
			if (rocket.marker.equals(marker)) {
				altos_droid.select_tracker(rocket.serial);
				return true;
			}
		}
		return false;
	}

	public void setupMap(int map_type) {
		mMap = mMapFragment.getMap();
		if (mMap != null) {
			set_map_type(map_type);
			mMap.setMyLocationEnabled(true);
			mMap.getUiSettings().setTiltGesturesEnabled(false);
			mMap.getUiSettings().setZoomControlsEnabled(false);
			mMap.setOnMarkerClickListener(this);

			mPadMarker = mMap.addMarker(
					new MarkerOptions().icon(BitmapDescriptorFactory.fromResource(R.drawable.pad))
					                   .position(new LatLng(0,0))
					                   .visible(false)
					);

			mPolyline = mMap.addPolyline(
					new PolylineOptions().add(new LatLng(0,0), new LatLng(0,0))
					                     .width(20)
					                     .color(Color.BLUE)
					                     .visible(false)
					);

			mapLoaded = true;
		}
	}

	public void center(double lat, double lon, double accuracy) {
		if (mMap == null)
			return;

		if (mapAccuracy < 0 || accuracy < mapAccuracy/10) {
			mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(new LatLng(lat, lon),14));
			mapAccuracy = accuracy;
		}
	}

	private void set_rocket(int serial, AltosState state) {
		RocketOnline	rocket;

		if (state.gps == null || state.gps.lat == AltosLib.MISSING)
			return;

		if (mMap == null)
			return;

		if (rockets.containsKey(serial)) {
			rocket = rockets.get(serial);
			rocket.set_position(new AltosLatLon(state.gps.lat, state.gps.lon), state.received_time);
		} else {
			rocket = new RocketOnline(context,
						  serial,
						  mMap, state.gps.lat, state.gps.lon,
						  state.received_time);
			rockets.put(serial, rocket);
		}
	}

	private void remove_rocket(int serial) {
		RocketOnline rocket = rockets.get(serial);
		rocket.remove();
		rockets.remove(serial);
	}

	public void set_visible(boolean visible) {
		if (map_view == null)
			return;
		if (visible)
			map_view.setVisibility(View.VISIBLE);
		else
			map_view.setVisibility(View.GONE);
	}

	public void show(TelemetryState telem_state, AltosState state, AltosGreatCircle from_receiver, Location receiver) {

		if (telem_state != null) {
			for (int serial : rockets.keySet()) {
				if (!telem_state.states.containsKey(serial))
					remove_rocket(serial);
			}

			for (int serial : telem_state.states.keySet()) {
				set_rocket(serial, telem_state.states.get(serial));
			}
		}

		if (state != null) {
			if (mapLoaded) {
				if (!pad_set && state.pad_lat != AltosLib.MISSING) {
					pad_set = true;
					mPadMarker.setPosition(new LatLng(state.pad_lat, state.pad_lon));
					mPadMarker.setVisible(true);
				}
			}
			if (state.gps != null) {

				target_position = new AltosLatLon(state.gps.lat, state.gps.lon);
				if (state.gps.locked && state.gps.nsat >= 4)
					center (state.gps.lat, state.gps.lon, 10);
			}
		}

		if (receiver != null) {
			double accuracy;

			if (receiver.hasAccuracy())
				accuracy = receiver.getAccuracy();
			else
				accuracy = 1000;

			my_position = new AltosLatLon(receiver.getLatitude(), receiver.getLongitude());
			center (my_position.lat, my_position.lon, accuracy);
		}

		if (my_position != null && target_position != null && mPolyline != null) {
			mPolyline.setPoints(Arrays.asList(new LatLng(my_position.lat, my_position.lon), new LatLng(target_position.lat, target_position.lon)));
			mPolyline.setVisible(true);
		}

	}

	public void set_map_type(int map_type) {
		if (mMap != null) {
			if (map_type == AltosMap.maptype_hybrid)
				mMap.setMapType(GoogleMap.MAP_TYPE_HYBRID);
			else if (map_type == AltosMap.maptype_satellite)
				mMap.setMapType(GoogleMap.MAP_TYPE_SATELLITE);
			else if (map_type == AltosMap.maptype_terrain)
				mMap.setMapType(GoogleMap.MAP_TYPE_TERRAIN);
			else
				mMap.setMapType(GoogleMap.MAP_TYPE_NORMAL);
		}
	}

	public AltosMapOnline(Context context) {
		this.context = context;
	}
}
