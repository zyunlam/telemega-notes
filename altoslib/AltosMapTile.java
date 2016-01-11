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

package org.altusmetrum.altoslib_9;

import java.io.*;
import java.util.*;

public abstract class AltosMapTile implements AltosFontListener {
	AltosMapTileListener	listener;
	public AltosLatLon	upper_left, center;
	public int		px_size;
	int		zoom;
	int		maptype;
	int		scale;
	public AltosMapStore	store;
	public AltosMapCache	cache;
	public int	status;

	static public final int	success = 0;
	static public final int	loading = 1;
	static public final int	failed = 2;
	static public final int	bad_request = 3;
	static public final int	forbidden = 4;

	private File map_file() {
		double lat = center.lat;
		double lon = center.lon;
		char chlat = lat < 0 ? 'S' : 'N';
		char chlon = lon < 0 ? 'W' : 'E';

		if (lat < 0) lat = -lat;
		if (lon < 0) lon = -lon;
		String maptype_string = String.format("%s-", AltosMap.maptype_names[maptype]);
		String format_string;
		if (maptype == AltosMap.maptype_hybrid || maptype == AltosMap.maptype_satellite || maptype == AltosMap.maptype_terrain)
			format_string = "jpg";
		else
			format_string = "png";
		return new File(AltosPreferences.mapdir(),
				String.format("map-%c%.6f,%c%.6f-%s%d%s.%s",
					      chlat, lat, chlon, lon, maptype_string, zoom, scale == 1 ? "" : String.format("-%d", scale), format_string));
	}

	private String map_url() {
		String format_string;
		int z = zoom;

		if (maptype == AltosMap.maptype_hybrid || maptype == AltosMap.maptype_satellite || maptype == AltosMap.maptype_terrain)
			format_string = "jpg";
		else
			format_string = "png32";

		for (int s = 1; s < scale; s <<= 1)
			z--;

		if (AltosVersion.has_google_maps_api_key())
			return String.format("http://maps.google.com/maps/api/staticmap?center=%.6f,%.6f&zoom=%d&size=%dx%d&scale=%d&sensor=false&maptype=%s&format=%s&key=%s",
					     center.lat, center.lon, z, px_size/scale, px_size/scale, scale, AltosMap.maptype_names[maptype], format_string, AltosVersion.google_maps_api_key);
		else
			return String.format("http://maps.google.com/maps/api/staticmap?center=%.6f,%.6f&zoom=%d&size=%dx%d&scale=%d&sensor=false&maptype=%s&format=%s",
					     center.lat, center.lon, z, px_size/scale, px_size/scale, AltosMap.maptype_names[maptype], format_string);
	}

	public void font_size_changed(int font_size) {
	}

	public void set_status(int status) {
		this.status = status;
		listener.notify_tile(this, status);
	}

	public void notify_image(AltosImage image) {
		listener.notify_tile(this, status);
	}

	public int store_status() {
		return store.status();
	}

	public void add_store_listener(AltosMapStoreListener listener) {
		store.add_listener(listener);
	}

	public void remove_store_listener(AltosMapStoreListener listener) {
		store.remove_listener(listener);
	}

	public abstract void paint(AltosMapTransform t);

	public AltosMapTile(AltosMapTileListener listener, AltosLatLon upper_left, AltosLatLon center, int zoom, int maptype, int px_size, int scale) {
		this.listener = listener;
		this.upper_left = upper_left;
		this.cache = listener.cache();

		while (center.lon < -180.0)
			center.lon += 360.0;
		while (center.lon > 180.0)
			center.lon -= 360.0;

		this.center = center;
		this.zoom = zoom;
		this.maptype = maptype;
		this.px_size = px_size;
		this.scale = scale;

		status = AltosMapTile.loading;
		store = AltosMapStore.get(map_url(), map_file());
	}

	public AltosMapTile(AltosMapTileListener listener, AltosLatLon upper_left, AltosLatLon center, int zoom, int maptype, int px_size) {
		this(listener, upper_left, center, zoom, maptype, px_size, 1);
	}
}
