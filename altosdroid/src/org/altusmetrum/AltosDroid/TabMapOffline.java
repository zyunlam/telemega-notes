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

class Rocket implements Comparable {
	AltosLatLon	position;
	String		name;
	long		last_packet;
	TabMapOffline	tab;

	void paint() {
		tab.draw_bitmap(position, tab.rocket_bitmap, tab.rocket_off_x, tab.rocket_off_y);
		tab.draw_text(position, name, 0, 3*tab.rocket_bitmap.getHeight()/4);
	}

	void set_position(AltosLatLon position, long last_packet) {
		this.position = position;
		this.last_packet = last_packet;
	}

	Rocket(String name, TabMapOffline tab) {
		this.name = name;
		this.tab = tab;
	}

	public int compareTo(Object o) {
		Rocket other = (Rocket) o;

		long	diff = last_packet - other.last_packet;

		if (diff > 0)
			return 1;
		if (diff < 0)
			return -1;
		return 0;
	}
}

public class TabMapOffline extends AltosDroidTab implements AltosMapInterface {

	AltosMap map;

	AltosLatLon	here;
	AltosLatLon	pad;

	Canvas	canvas;
	Paint	paint;

	Bitmap	pad_bitmap;
	int	pad_off_x, pad_off_y;
	Bitmap	rocket_bitmap;
	int	rocket_off_x, rocket_off_y;
	Bitmap	here_bitmap;
	int	here_off_x, here_off_y;

	private boolean pad_set;

	private TextView mDistanceView;
	private TextView mBearingView;
	private TextView mTargetLatitudeView;
	private TextView mTargetLongitudeView;
	private TextView mReceiverLatitudeView;
	private TextView mReceiverLongitudeView;
	private AltosMapView map_view;

	private double mapAccuracy = -1;

	int	stroke_width = 20;


	void draw_text(AltosLatLon lat_lon, String text, int off_x, int off_y) {
		if (lat_lon != null && map != null && map.transform != null) {
			AltosPointInt pt = new AltosPointInt(map.transform.screen(lat_lon));

			Rect	bounds = new Rect();
			paint.getTextBounds(text, 0, text.length(), bounds);

			int	width = bounds.right - bounds.left;
			int	height = bounds.bottom - bounds.top;

			float x = pt.x;
			float y = pt.y;
			x = x - width / 2.0f - off_x;
			y = y + height / 2.0f - off_y;
			paint.setColor(0xff000000);
			canvas.drawText(text, 0, text.length(), x, y, paint);
		}
	}

	void draw_bitmap(AltosLatLon lat_lon, Bitmap bitmap, int off_x, int off_y) {
		if (lat_lon != null && map != null && map.transform != null) {
			AltosPointInt pt = new AltosPointInt(map.transform.screen(lat_lon));

			canvas.drawBitmap(bitmap, pt.x - off_x, pt.y - off_y, paint);
		}
	}

	HashMap<Integer,Rocket> rockets = new HashMap<Integer,Rocket>();

	/* AltosMapInterface */

	static  final int	WHITE = 0xffffffff;
	static  final int	RED   = 0xffff0000;
	static  final int	PINK  = 0xffff8080;
	static  final int	YELLOW= 0xffffff00;
	static  final int	CYAN  = 0xff00ffff;
	static  final int	BLUE  = 0xff0000ff;
	static  final int	BLACK = 0xff000000;

	public static final int stateColors[] = {
		WHITE,  // startup
		WHITE,  // idle
		WHITE,  // pad
		RED,    // boost
		PINK,   // fast
		YELLOW, // coast
		CYAN,   // drogue
		BLUE,   // main
		BLACK,  // landed
		BLACK,  // invalid
		CYAN,   // stateless
	};

	public AltosMapPath new_path() {
		return null;
	}

	public AltosMapLine new_line() {
		return null;
	}

	class MapImage implements AltosImage {
		public Bitmap	bitmap;

		public void flush() {
			if (bitmap != null) {
				bitmap.recycle();
				bitmap = null;
			}
		}

		public MapImage(File file) {
			bitmap = BitmapFactory.decodeFile(file.getPath());
		}
	}

	public AltosImage load_image(File file) throws Exception {
		return new MapImage(file);
	}

	class MapMark extends AltosMapMark {
		public void paint(AltosMapTransform t) {
		}

		MapMark(double lat, double lon, int state) {
			super(lat, lon, state);
		}
	}

	public AltosMapMark new_mark(double lat, double lon, int state) {
		return new MapMark(lat, lon, state);
	}

	class MapTile extends AltosMapTile {
		public void paint(AltosMapTransform t) {
			AltosPointInt		pt = new AltosPointInt(t.screen(upper_left));

			if (canvas.quickReject(pt.x, pt.y, pt.x + px_size, pt.y + px_size, Canvas.EdgeType.AA))
				return;

			AltosImage		altos_image = cache.get(this, store, px_size, px_size);

			MapImage 		map_image = (MapImage) altos_image;

			Bitmap			bitmap = null;

			if (map_image != null)
				bitmap = map_image.bitmap;

			if (bitmap != null) {
				canvas.drawBitmap(bitmap, pt.x, pt.y, paint);
			} else {
				paint.setColor(0xff808080);
				canvas.drawRect(pt.x, pt.y, pt.x + px_size, pt.y + px_size, paint);
				if (t.has_location()) {
					String	message = null;
					switch (status) {
					case AltosMapTile.loading:
						message = "Loading...";
						break;
					case AltosMapTile.bad_request:
						message = "Internal error";
						break;
					case AltosMapTile.failed:
						message = "Network error, check connection";
						break;
					case AltosMapTile.forbidden:
						message = "Too many requests, try later";
						break;
					}
					if (message != null) {
						Rect	bounds = new Rect();
						paint.getTextBounds(message, 0, message.length(), bounds);

						int	width = bounds.right - bounds.left;
						int	height = bounds.bottom - bounds.top;

						float x = pt.x + px_size / 2.0f;
						float y = pt.y + px_size / 2.0f;
						x = x - width / 2.0f;
						y = y + height / 2.0f;
						paint.setColor(0xff000000);
						canvas.drawText(message, 0, message.length(), x, y, paint);
					}
				}
			}
		}

		public MapTile(AltosMapTileListener listener, AltosLatLon upper_left, AltosLatLon center, int zoom, int maptype, int px_size) {
			super(listener, upper_left, center, zoom, maptype, px_size, 2);
		}

	}

	public AltosMapTile new_tile(AltosMapTileListener listener, AltosLatLon upper_left, AltosLatLon center, int zoom, int maptype, int px_size) {
		return new MapTile(listener, upper_left, center, zoom, maptype, px_size);
	}

	public int width() {
		if (map_view != null)
			return map_view.getWidth();
		return 500;
	}

	public int height() {
		if (map_view != null)
			return map_view.getHeight();
		return 500;
	}

	public void repaint() {
		if (map_view != null)
			map_view.postInvalidate();
	}

	public void repaint(AltosRectangle damage) {
		if (map_view != null)
			map_view.postInvalidate(damage.x, damage.y, damage.x + damage.width, damage.y + damage.height);
	}

	public void set_zoom_label(String label) {
	}

	public void debug(String format, Object ... arguments) {
		AltosDebug.debug(format, arguments);
	}

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);

		map = new AltosMap(this);
		map.set_maptype(altos_droid.map_type);

		pad_bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.pad);
		/* arrow at the bottom of the launchpad image */
		pad_off_x = pad_bitmap.getWidth() / 2;
		pad_off_y = pad_bitmap.getHeight();

		rocket_bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.rocket);
		/* arrow at the bottom of the rocket image */
		rocket_off_x = rocket_bitmap.getWidth() / 2;
		rocket_off_y = rocket_bitmap.getHeight();

		here_bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.ic_maps_indicator_current_position);
		/* Center of the dot */
		here_off_x = here_bitmap.getWidth() / 2;
		here_off_y = here_bitmap.getHeight() / 2;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		View v = inflater.inflate(R.layout.tab_map_offline, container, false);

		map_view = (AltosMapView)v.findViewById(R.id.map_view_offline);
		map_view.set_tab(this);
		mDistanceView  = (TextView)v.findViewById(R.id.distance_value_offline);
		mBearingView   = (TextView)v.findViewById(R.id.bearing_value_offline);
		mTargetLatitudeView  = (TextView)v.findViewById(R.id.target_lat_value_offline);
		mTargetLongitudeView = (TextView)v.findViewById(R.id.target_lon_value_offline);
		mReceiverLatitudeView  = (TextView)v.findViewById(R.id.receiver_lat_value_offline);
		mReceiverLongitudeView = (TextView)v.findViewById(R.id.receiver_lon_value_offline);
		return v;
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
	}

	@Override
	public void onDestroyView() {
		super.onDestroyView();

	}

	private void center(double lat, double lon, double accuracy) {
		if (mapAccuracy < 0 || accuracy < mapAccuracy/10) {
			if (map != null)
				map.maybe_centre(lat, lon);
			mapAccuracy = accuracy;
		}
	}

	public String tab_name() { return "offmap"; }

	public void show(TelemetryState telem_state, AltosState state, AltosGreatCircle from_receiver, Location receiver) {
		if (from_receiver != null) {
			mBearingView.setText(String.format("%3.0f°", from_receiver.bearing));
			set_value(mDistanceView, AltosConvert.distance, 6, from_receiver.distance);
		}

		if (state != null) {
			map.show(state, null);
			if (state.gps != null) {
				mTargetLatitudeView.setText(AltosDroid.pos(state.gps.lat, "N", "S"));
				mTargetLongitudeView.setText(AltosDroid.pos(state.gps.lon, "E", "W"));
				if (state.gps.locked && state.gps.nsat >= 4)
					center (state.gps.lat, state.gps.lon, 10);
			}
			if (state.pad_lat != AltosLib.MISSING && pad == null)
				pad = new AltosLatLon(state.pad_lat, state.pad_lon);
		}

		if (telem_state != null) {
			Integer[] old_serial = rockets.keySet().toArray(new Integer[0]);
			Integer[] new_serial = telem_state.states.keySet().toArray(new Integer[0]);

			/* remove deleted keys */
			for (int serial : old_serial) {
				if (!telem_state.states.containsKey(serial))
					rockets.remove(serial);
			}

			/* set remaining keys */

			for (int serial : new_serial) {
				Rocket 		rocket;
				AltosState	t_state = telem_state.states.get(serial);
				if (rockets.containsKey(serial))
					rocket = rockets.get(serial);
				else {
					rocket = new Rocket(String.format("%d", serial), this);
					rockets.put(serial, rocket);
				}
				if (t_state.gps != null)
					rocket.set_position(new AltosLatLon(t_state.gps.lat, t_state.gps.lon), t_state.received_time);
			}
		}

		if (receiver != null) {
			double accuracy;

			here = new AltosLatLon(receiver.getLatitude(), receiver.getLongitude());
			if (receiver.hasAccuracy())
				accuracy = receiver.getAccuracy();
			else
				accuracy = 1000;
			mReceiverLatitudeView.setText(AltosDroid.pos(here.lat, "N", "S"));
			mReceiverLongitudeView.setText(AltosDroid.pos(here.lon, "E", "W"));
			center (here.lat, here.lon, accuracy);
		}

	}

	@Override
	public void set_map_type(int map_type) {
		if (map != null)
			map.set_maptype(map_type);
	}

	public TabMapOffline() {
	}
}
