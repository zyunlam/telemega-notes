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

import java.util.Arrays;
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
import android.util.Log;

public class TabMapOffline extends AltosDroidTab implements AltosMapInterface {
	// Debugging
	static final String TAG = "AltosDroid";
	static final boolean D = true;

	AltosDroid mAltosDroid;

	AltosMap map;

	Canvas	canvas;
	Paint	paint;

	private boolean pad_set;

	private TextView mDistanceView;
	private TextView mBearingView;
	private TextView mTargetLatitudeView;
	private TextView mTargetLongitudeView;
	private TextView mReceiverLatitudeView;
	private TextView mReceiverLongitudeView;

	private double mapAccuracy = -1;

	int	stroke_width = 20;

	class MapView extends View implements ScaleGestureDetector.OnScaleGestureListener {

		ScaleGestureDetector	scale_detector;
		boolean			scaling;

		protected void onDraw(Canvas view_canvas) {
			canvas = view_canvas;
			paint = new Paint(Paint.ANTI_ALIAS_FLAG);
			paint.setStrokeWidth(stroke_width);
			paint.setStrokeCap(Paint.Cap.ROUND);
			paint.setStrokeJoin(Paint.Join.ROUND);
			map.paint();
			canvas = null;
		}

		public boolean onScale(ScaleGestureDetector detector) {
			float	f = detector.getScaleFactor();
			if (D) Log.d(TAG, String.format("onScale %f\n", f));
			if (f <= 0.8) {
				map.set_zoom(map.get_zoom() - 1);
				return true;
			}
			if (f >= 1.2) {
				map.set_zoom(map.get_zoom() + 1);
				return true;
			}
			return false;
		}

		public boolean onScaleBegin(ScaleGestureDetector detector) {
			if (D) Log.d(TAG, String.format("onScaleBegin %f\n", detector.getScaleFactor()));
			return true;
		}

		public void onScaleEnd(ScaleGestureDetector detector) {
			if (D) Log.d(TAG, String.format("onScaleEnd %f\n", detector.getScaleFactor()));
		}

		@Override
		public boolean dispatchTouchEvent(MotionEvent event) {
			scale_detector.onTouchEvent(event);

			if (scale_detector.isInProgress()) {
				scaling = true;
			}

			if (scaling) {
				if(D) Log.d(TAG, "scale in progress\n");
				if (event.getAction() == MotionEvent.ACTION_UP) {
					if (D) Log.d(TAG, "scale finished\n");
					scaling = false;
				}
				return true;
			}

			if (event.getAction() == MotionEvent.ACTION_DOWN) {
				if(D) Log.d(TAG, String.format("down event %g %g\n", event.getX(), event.getY()));
				map.touch_start((int) event.getX(), (int) event.getY(), true);
			} else if (event.getAction() == MotionEvent.ACTION_MOVE) {
				if(D) Log.d(TAG, String.format("continue event %g %g\n", event.getX(), event.getY()));
				map.touch_continue((int) event.getX(), (int) event.getY(), true);
			}
			return true;
		}

		public MapView(Context context) {
			super(context);
			scale_detector = new ScaleGestureDetector(this.getContext(), this);
		}
	}

	class MapFragment extends Fragment {
		MapView	map_view;

		public View onCreateView(LayoutInflater inflator, ViewGroup container, Bundle savedInstanceState) {
			map_view = new MapView(container.getContext());
			return map_view;
		}

		public MapFragment() {
		}
	}

	MapFragment map_fragment;

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

	class MapPath extends AltosMapPath {

		boolean line_in(AltosPointDouble a, AltosPointDouble b) {
			final Rect bounds = canvas.getClipBounds();
			int left = (int) Math.floor (Math.min((float) a.x, (float) b.x) - stroke_width / 2.0f);
			int right = (int) Math.ceil(Math.max((float) a.x, (float) b.x) + stroke_width / 2.0f);
			int top = (int) Math.floor(Math.min((float) a.y, (float) b.y) - stroke_width / 2.0f);
			int bottom = (int) Math.ceil(Math.max((float) a.y, (float) b.y) + stroke_width / 2.0f);

			return left < bounds.right && bounds.left < right &&
				top < bounds.bottom && bounds.top < bottom;
		}

		public void paint(AltosMapTransform t) {
			AltosPointDouble	prev = null;
			int			cur_color = paint.getColor();

			for (AltosMapPathPoint point : points) {
				AltosPointDouble	cur = t.screen(point.lat_lon);

				if (prev != null && line_in(prev, cur)) {
					int color;
					if (0 <= point.state && point.state < stateColors.length)
						color = stateColors[point.state];
					else
						color = stateColors[AltosLib.ao_flight_invalid];
					if (color != cur_color) {
						paint.setColor(color);
						cur_color = color;
					}
					canvas.drawLine((float) prev.x, (float) prev.y, (float) cur.x, (float) cur.y, paint);
				}
				prev = cur;
			}
		}

		public MapPath() {
			stroke_width = TabMapOffline.this.stroke_width;
		}
	}

	public AltosMapPath new_path() {
		return new MapPath();
	}

	class MapLine extends AltosMapLine {
		public void paint(AltosMapTransform t) {
		}

		public MapLine() {
		}
	}

	public AltosMapLine new_line() {
		return new MapLine();
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
		if (map_fragment != null && map_fragment.map_view != null)
			return map_fragment.map_view.getWidth();
		return 500;
	}

	public int height() {
		if (map_fragment != null && map_fragment.map_view != null)
			return map_fragment.map_view.getHeight();
		return 500;
	}

	public void repaint() {
		this.getActivity().runOnUiThread(new Runnable() {
				public void run() {
					if (map_fragment != null && map_fragment.map_view != null)
						map_fragment.map_view.invalidate();
				}
			});
	}

	public void repaint(AltosRectangle t_damage) {
		final AltosRectangle damage = t_damage;
		this.getActivity().runOnUiThread(new Runnable() {
				public void run() {
					if (map_fragment != null && map_fragment.map_view != null)
						map_fragment.map_view.invalidate(damage.x, damage.y, damage.x + damage.width, damage.y + damage.height);
				}
			});
	}

	public void set_zoom_label(String label) {
	}

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		mAltosDroid = (AltosDroid) activity;
		mAltosDroid.registerTab(this);
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		View v = inflater.inflate(R.layout.tab_map, container, false);

		map_fragment = new MapFragment();
		map = new AltosMap(this);
		mDistanceView  = (TextView)v.findViewById(R.id.distance_value);
		mBearingView   = (TextView)v.findViewById(R.id.bearing_value);
		mTargetLatitudeView  = (TextView)v.findViewById(R.id.target_lat_value);
		mTargetLongitudeView = (TextView)v.findViewById(R.id.target_lon_value);
		mReceiverLatitudeView  = (TextView)v.findViewById(R.id.receiver_lat_value);
		mReceiverLongitudeView = (TextView)v.findViewById(R.id.receiver_lon_value);
		return v;
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		getChildFragmentManager().beginTransaction().add(R.id.map, map_fragment).commit();
	}
 
	@Override
	public void onDestroyView() {
		super.onDestroyView();

		mAltosDroid.unregisterTab(this);
		mAltosDroid = null;
		map_fragment = null;

//		Fragment fragment = (getFragmentManager().findFragmentById(R.id.map));
//		FragmentTransaction ft = getActivity().getSupportFragmentManager().beginTransaction();
//		ft.remove(fragment);
//		ft.commit();
	}

	private void setupMap() {
/*
		mMap = mMapFragment.getMap();
		if (mMap != null) {
			mMap.setMyLocationEnabled(true);
			mMap.getUiSettings().setTiltGesturesEnabled(false);
			mMap.getUiSettings().setZoomControlsEnabled(false);

			mRocketMarker = mMap.addMarker(
					// From: http://mapicons.nicolasmollet.com/markers/industry/military/missile-2/
					new MarkerOptions().icon(BitmapDescriptorFactory.fromResource(R.drawable.rocket))
					                   .position(new LatLng(0,0))
					                   .visible(false)
					);

			mPadMarker = mMap.addMarker(
					new MarkerOptions().icon(BitmapDescriptorFactory.fromResource(R.drawable.pad))
					                   .position(new LatLng(0,0))
					                   .visible(false)
					);

			mPolyline = mMap.addPolyline(
					new PolylineOptions().add(new LatLng(0,0), new LatLng(0,0))
					                     .width(3)
					                     .color(Color.BLUE)
					                     .visible(false)
					);

			mapLoaded = true;
		}
*/
	}

	private void center(double lat, double lon, double accuracy) {
		if (mapAccuracy < 0 || accuracy < mapAccuracy/10) {
			if (map != null)
				map.centre(lat, lon);
			mapAccuracy = accuracy;
		}
	}

	public String tab_name() { return "offmap"; }

	public void show(AltosState state, AltosGreatCircle from_receiver, Location receiver) {
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
		}

		if (receiver != null) {
			double accuracy;

			if (receiver.hasAccuracy())
				accuracy = receiver.getAccuracy();
			else
				accuracy = 1000;
			mReceiverLatitudeView.setText(AltosDroid.pos(receiver.getLatitude(), "N", "S"));
			mReceiverLongitudeView.setText(AltosDroid.pos(receiver.getLongitude(), "E", "W"));
			center (receiver.getLatitude(), receiver.getLongitude(), accuracy);
		}

	}

	public TabMapOffline() {
	}
}
