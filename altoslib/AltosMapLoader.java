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

package org.altusmetrum.altoslib_8;

import java.io.*;
import java.util.*;
import java.text.*;
import java.lang.Math;
import java.net.URL;
import java.net.URLConnection;

public class AltosMapLoader implements AltosMapTileListener, AltosMapStoreListener {
	AltosMapLoaderListener	listener;

	double	latitude, longitude;
	int	min_z;
	int	max_z;
	int	cur_z;
	int	all_types;
	int	cur_type;
	double	radius;

	int	tiles_loaded_layer;
	int	tiles_loaded_total;
	int	tiles_this_layer;
	int	tiles_total;
	int	layers_total;
	int	layers_loaded;

	AltosMap	map;

	int tile_radius(int zoom) {
		double	delta_lon = AltosMapTransform.lon_from_distance(latitude, radius);

		AltosMapTransform t = new AltosMapTransform(256, 256, zoom + AltosMap.default_zoom, new AltosLatLon(latitude, longitude));

		AltosPointDouble	center = t.point(new AltosLatLon(latitude, longitude));
		AltosPointDouble	edge = t.point(new AltosLatLon(latitude, longitude + delta_lon));

		int tile_radius = (int) Math.ceil(Math.abs(center.x - edge.x) / AltosMap.px_size);

		return tile_radius;
	}

	int tiles_per_layer(int zoom) {
		int	tile_radius = tile_radius(zoom);
		return (tile_radius * 2 + 1) * (tile_radius * 2 + 1);
	}

	public void do_load() {
		tiles_this_layer = tiles_per_layer(cur_z);
		tiles_loaded_layer = 0;
		listener.debug("tiles_this_layer %d (zoom %d)\n", tiles_this_layer, cur_z);

		int load_radius = tile_radius(cur_z);
		int zoom = cur_z + AltosMap.default_zoom;
		int maptype = cur_type;
		AltosLatLon load_centre = new AltosLatLon(latitude, longitude);
		AltosMapTransform transform = new AltosMapTransform(256, 256, zoom, load_centre);

		map.centre(load_centre);

		AltosPointInt	upper_left;
		AltosPointInt	lower_right;

		AltosPointInt centre = AltosMap.floor(transform.point(load_centre));

		upper_left = new AltosPointInt(centre.x - load_radius * AltosMap.px_size,
					       centre.y - load_radius * AltosMap.px_size);
		lower_right = new AltosPointInt(centre.x + load_radius * AltosMap.px_size,
						centre.y + load_radius * AltosMap.px_size);


		for (int y = (int) upper_left.y; y <= lower_right.y; y += AltosMap.px_size) {
			for (int x = (int) upper_left.x; x <= lower_right.x; x += AltosMap.px_size) {
				listener.debug("Make tile at %d, %d\n", x, y);
				AltosPointInt	point = new AltosPointInt(x, y);
				AltosLatLon	ul = transform.lat_lon(point);
				AltosLatLon	center = transform.lat_lon(new AltosPointDouble(x + AltosMap.px_size/2, y + AltosMap.px_size/2));
				AltosMapTile	tile = map.map_interface.new_tile(this, ul, center, zoom, maptype, AltosMap.px_size);
				tile.add_store_listener(this);
				if (tile.store_status() != AltosMapTile.loading)
					notify_tile(tile, tile.store_status());
			}
		}
	}

	public int next_type(int start) {
		int next_type;
		for (next_type = start;
		     next_type <= AltosMap.maptype_terrain && (all_types & (1 << next_type)) == 0;
		     next_type++)
			;
		return next_type;
	}

	public void next_load() {
		int next_type = next_type(cur_type + 1);

		if (next_type > AltosMap.maptype_terrain) {
			if (cur_z == max_z) {
				return;
			} else {
				cur_z++;
			}
			next_type = next_type(0);
		}
		cur_type = next_type;
		do_load();
	}

	private void start_load() {

		cur_z = min_z;
		int ntype = 0;

		for (int t = AltosMap.maptype_hybrid; t <= AltosMap.maptype_terrain; t++)
			if ((all_types & (1 << t)) != 0)
				ntype++;
		if (ntype == 0) {
			all_types = (1 << AltosMap.maptype_hybrid);
			ntype = 1;
		}

		cur_type = next_type(0);

		for (int z = min_z; z <= max_z; z++)
			tiles_total += tiles_per_layer(z);

		layers_total = (max_z - min_z + 1) * ntype;
		layers_loaded = 0;
		tiles_loaded_total = 0;

		listener.debug("total tiles %d\n", tiles_total);

		listener.loader_start(tiles_total);
		do_load();
	}

	public void load(double latitude, double longitude, int min_z, int max_z, double radius, int all_types) {
		listener.debug("lat %f lon %f min_z %d max_z %d radius %f all_types %d\n",
			       latitude, longitude, min_z, max_z, radius, all_types);
		this.latitude = latitude;
		this.longitude = longitude;
		this.min_z = min_z;
		this.max_z = max_z;
		this.radius = radius;
		this.all_types = all_types;
		start_load();
	}

	public synchronized void notify_store(AltosMapStore store, int status) {
		boolean	do_next = false;
		if (status == AltosMapTile.loading)
			return;

		if (layers_loaded >= layers_total)
			return;

		++tiles_loaded_total;
		++tiles_loaded_layer;
		listener.debug("total %d layer %d\n", tiles_loaded_total, tiles_loaded_layer);

		if (tiles_loaded_layer == tiles_this_layer) {
			++layers_loaded;
			listener.debug("%d layers loaded\n", layers_loaded);
			if (layers_loaded == layers_total) {
				listener.loader_done(tiles_total);
				return;
			} else {
				do_next = true;
			}
		}
		listener.loader_notify(tiles_loaded_total,
				       tiles_total, store.file.toString());
		if (do_next)
			next_load();
	}

	public synchronized void notify_tile(AltosMapTile tile, int status) {
		notify_store(tile.store, status);
	}

	public AltosMapCache cache() { return map.cache(); }

	public AltosMapLoader(AltosMap map, AltosMapLoaderListener listener) {
		this.map = map;
		this.listener = listener;
	}
}
