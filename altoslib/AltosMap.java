/*
 * Copyright © 2010 Anthony Towns <aj@erisian.com.au>
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

package org.altusmetrum.altoslib_6;

import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.concurrent.*;

public class AltosMap implements AltosFlightDisplay, AltosMapTileListener, AltosMapStoreListener {

	public static final int px_size = 512;

	public static final int maptype_hybrid = 0;
	public static final int maptype_roadmap = 1;
	public static final int maptype_satellite = 2;
	public static final int maptype_terrain = 3;
	public static final int maptype_default = maptype_hybrid;

	public static final int default_zoom = 15;
	public static final int min_zoom = 3;
	public static final int max_zoom = 21;

	public static final String[] maptype_names = {
		"hybrid",
		"roadmap",
		"satellite",
		"terrain"
	};

	public static final String[] maptype_labels = {
		"Hybrid",
		"Roadmap",
		"Satellite",
		"Terrain"
	};

	AltosMapInterface	map_interface;

	AltosMapCache		cache;

	LinkedList<AltosMapMark> marks = new LinkedList<AltosMapMark>();

	LinkedList<AltosMapZoomListener> zoom_listeners = new LinkedList<AltosMapZoomListener>();

	public void add_zoom_listener(AltosMapZoomListener listener) {
		if (!zoom_listeners.contains(listener))
			zoom_listeners.add(listener);
	}

	public void remove_zoom_listener(AltosMapZoomListener listener) {
		zoom_listeners.remove(listener);
	}

	boolean		have_boost = false;
	boolean		have_landed = false;

	ConcurrentHashMap<AltosPointInt,AltosMapTile> tiles = new ConcurrentHashMap<AltosPointInt,AltosMapTile>();
	int		load_radius;
	AltosLatLon	load_centre = null;
	AltosMapTileListener	load_listener;

	int 		zoom = AltosMap.default_zoom;
	int		maptype = AltosMap.maptype_default;

	long		user_input_time;

	/* Milliseconds to wait after user action before auto-scrolling
	 */
	static final long auto_scroll_delay = 20 * 1000;

	AltosMapTransform	transform;
	AltosLatLon		centre;

	public void reset() {
		// nothing
	}

	/* MapInterface wrapping functions */
	public void set_units() {
		map_interface.set_units();
	}

	public void repaint(AltosMapRectangle damage, int pad) {
		map_interface.repaint(damage, pad);
	}

	public void repaint(double x, double y, double w, double h) {
		map_interface.repaint(x, y, w, h);
	}

	public void repaint() {
		map_interface.repaint();
	}

	public int width() {
		return map_interface.width();
	}

	public int height() {
		return map_interface.height();
	}

	public AltosPointInt floor(AltosPointDouble point) {
		return new AltosPointInt ((int) Math.floor(point.x / AltosMap.px_size) * AltosMap.px_size,
					      (int) Math.floor(point.y / AltosMap.px_size) * AltosMap.px_size);
	}

	public AltosPointInt ceil(AltosPointDouble point) {
		return new AltosPointInt ((int) Math.ceil(point.x / AltosMap.px_size) * AltosMap.px_size,
					      (int) Math.ceil(point.y / AltosMap.px_size) * AltosMap.px_size);
	}

	public void notice_user_input() {
		user_input_time = System.currentTimeMillis();
	}

	public boolean recent_user_input() {
		return (System.currentTimeMillis() - user_input_time) < auto_scroll_delay;
	}

	public boolean far_from_centre(AltosLatLon lat_lon) {

		if (centre == null || transform == null)
			return true;

		AltosPointDouble	screen = transform.screen(lat_lon);

		int		width = width();
		int		dx = Math.abs ((int) (double) screen.x - width/2);

		if (dx > width / 4)
			return true;

		int		height = height();
		int		dy = Math.abs ((int) (double) screen.y - height/2);

		if (dy > height / 4)
			return true;

		return false;
	}

	public void font_size_changed(int font_size) {
		map_interface.line().font_size_changed(font_size);
		for (AltosMapTile tile : tiles.values())
			tile.font_size_changed(font_size);
		repaint();
	}

	public void units_changed(boolean imperial_units) {
	}

	private void set_transform() {
		transform = new AltosMapTransform(width(), height(), zoom, centre);
		repaint();
	}

	public boolean set_zoom(int zoom) {
		if (AltosMap.min_zoom <= zoom && zoom <= AltosMap.max_zoom && zoom != this.zoom) {
			this.zoom = zoom;
			tiles.clear();
			set_transform();

			for (AltosMapZoomListener listener : zoom_listeners)
				listener.zoom_changed(this.zoom);

			return true;
		}
		return false;
	}

	public int get_zoom() {
		return zoom;
	}

	public boolean set_maptype(int maptype) {
		if (maptype != this.maptype) {
			this.maptype = maptype;
			tiles.clear();
			repaint();
			return true;
		}
		return false;
	}

	public void show(AltosState state, AltosListenerState listener_state) {

		/* If insufficient gps data, nothing to update
		 */
		AltosGPS	gps = state.gps;

		if (gps == null)
			return;

		if (!gps.locked && gps.nsat < 4)
			return;

		AltosMapRectangle	damage = map_interface.path().add(gps.lat, gps.lon, state.state);

		switch (state.state) {
		case AltosLib.ao_flight_boost:
			if (!have_boost) {
				add_mark(gps.lat, gps.lon, state.state);
				have_boost = true;
			}
			break;
		case AltosLib.ao_flight_landed:
			if (!have_landed) {
				add_mark(gps.lat, gps.lon, state.state);
				have_landed = true;
			}
			break;
		}

		if (damage != null)
			repaint(damage, AltosMapPath.stroke_width);
		maybe_centre(gps.lat, gps.lon);
	}

	public void centre(AltosLatLon lat_lon) {
		centre = lat_lon;
		set_transform();
	}

	public void centre(double lat, double lon) {
		centre(new AltosLatLon(lat, lon));
	}

	public void centre(AltosState state) {
		if (!state.gps.locked && state.gps.nsat < 4)
			return;
		centre(state.gps.lat, state.gps.lon);
	}

	public void maybe_centre(double lat, double lon) {
		AltosLatLon	lat_lon = new AltosLatLon(lat, lon);
		if (centre == null || (!recent_user_input() && far_from_centre(lat_lon)))
			centre(lat_lon);
	}

	public void add_mark(double lat, double lon, int state) {
		synchronized(marks) {
			marks.add(map_interface.new_mark(lat, lon, state));
		}
		repaint();
	}

	public void clear_marks() {
		synchronized(marks) {
			marks.clear();
		}
	}

	private void make_tiles() {
		AltosPointInt	upper_left;
		AltosPointInt	lower_right;

		if (load_centre != null) {
			AltosPointInt centre = floor(transform.point(load_centre));

			upper_left = new AltosPointInt(centre.x - load_radius * AltosMap.px_size,
							       centre.y - load_radius * AltosMap.px_size);
			lower_right = new AltosPointInt(centre.x + load_radius * AltosMap.px_size,
								centre.y + load_radius * AltosMap.px_size);
		} else {
			upper_left = floor(transform.screen_point(new AltosPointInt(0, 0)));
			lower_right = floor(transform.screen_point(new AltosPointInt(width(), height())));
		}
		LinkedList<AltosPointInt> to_remove = new LinkedList<AltosPointInt>();

		for (AltosPointInt point : tiles.keySet()) {
			if (point.x < upper_left.x || lower_right.x < point.x ||
			    point.y < upper_left.y || lower_right.y < point.y) {
				to_remove.add(point);
			}
		}

		for (AltosPointInt point : to_remove)
			tiles.remove(point);

		cache.set_cache_size((width() / AltosMap.px_size + 2) * (height() / AltosMap.px_size + 2));
		for (int y = (int) upper_left.y; y <= lower_right.y; y += AltosMap.px_size) {
			for (int x = (int) upper_left.x; x <= lower_right.x; x += AltosMap.px_size) {
				AltosPointInt point = new AltosPointInt(x, y);

				if (!tiles.containsKey(point)) {
					AltosLatLon	ul = transform.lat_lon(new AltosPointDouble(x, y));
					AltosLatLon	center = transform.lat_lon(new AltosPointDouble(x + AltosMap.px_size/2, y + AltosMap.px_size/2));
					AltosMapTile tile = map_interface.new_tile(this, ul, center, zoom, maptype,
										   AltosMap.px_size);
					tiles.put(point, tile);
				}
			}
		}
	}

	public void set_load_params(double lat, double lon, int radius, AltosMapTileListener listener) {
		load_centre = new AltosLatLon(lat, lon);
		load_radius = radius;
		load_listener = listener;
		centre(lat, lon);
		make_tiles();
		for (AltosMapTile tile : tiles.values()) {
			tile.add_store_listener(this);
			if (tile.store_status() != AltosMapTile.loading)
				listener.notify_tile(tile, tile.store_status());
		}
		repaint();
	}

	public String getName() {
		return "Map";
	}

	public void paint() {
		make_tiles();

		for (AltosMapTile tile : tiles.values())
			tile.paint(transform);

		synchronized(marks) {
			for (AltosMapMark mark : marks)
				mark.paint(transform);
		}

		map_interface.path().paint(transform);

		map_interface.line().paint(transform);
	}

	/* AltosMapTileListener methods */
	public synchronized void notify_tile(AltosMapTile tile, int status) {
		for (AltosPointInt point : tiles.keySet()) {
			if (tile == tiles.get(point)) {
				AltosPointInt	screen = transform.screen(point);
				repaint(screen.x, screen.y, AltosMap.px_size, AltosMap.px_size);
			}
		}
	}

	/* AltosMapStoreListener methods */
	public synchronized void notify_store(AltosMapStore store, int status) {
		if (load_listener != null) {
			for (AltosMapTile tile : tiles.values())
				if (store.equals(tile.store))
					load_listener.notify_tile(tile, status);
		}
	}

	public AltosMap(AltosMapInterface map_interface) {
		this.map_interface = map_interface;
		cache = new AltosMapCache(map_interface);
		centre(0, 0);
	}
}
