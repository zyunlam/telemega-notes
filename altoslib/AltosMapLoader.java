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

package org.altusmetrum.altoslib_7;

import java.io.*;
import java.util.*;
import java.text.*;
import java.lang.Math;
import java.net.URL;
import java.net.URLConnection;

public class AltosMapLoader implements AltosMapTileListener {
	AltosMapLoaderListener	listener;

	double	latitude, longitude;
	int	min_z;
	int	max_z;
	int	cur_z;
	int	all_types;
	int	cur_type;
	int	radius;

	int	tiles_per_layer;
	int	tiles_loaded;
	int	layers_total;
	int	layers_loaded;

	AltosMap	map;

	public void do_load() {
		map.set_load_params(cur_z + AltosMap.default_zoom, cur_type, latitude, longitude, radius, this);
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
		tiles_per_layer = (radius * 2 + 1) * (radius * 2 + 1);
		layers_total = (max_z - min_z + 1) * ntype;
		layers_loaded = 0;
		tiles_loaded = 0;

		listener.loader_start(layers_total * tiles_per_layer);
		do_load();
	}

	public void load(double latitude, double longitude, int min_z, int max_z, int radius, int all_types) {
		this.latitude = latitude;
		this.longitude = longitude;
		this.min_z = min_z;
		this.max_z = max_z;
		this.radius = radius;
		this.all_types = all_types;
		start_load();
	}

	public synchronized void notify_tile(AltosMapTile tile, int status) {
		boolean	do_next = false;
		if (status == AltosMapTile.loading)
			return;

		if (layers_loaded >= layers_total)
			return;

		++tiles_loaded;

		if (tiles_loaded == tiles_per_layer) {
			tiles_loaded = 0;
			++layers_loaded;
			if (layers_loaded == layers_total) {
				listener.loader_done(layers_total * tiles_per_layer);
				return;
			} else {
				do_next = true;
			}
		}
		listener.loader_notify(layers_loaded * tiles_per_layer + tiles_loaded,
				       layers_total * tiles_per_layer, tile.store.file.toString());
		if (do_next)
			next_load();
	}

	public AltosMapCache cache() { return map.cache(); }

	public AltosMapLoader(AltosMap map, AltosMapLoaderListener listener) {
		this.map = map;
		this.listener = listener;
	}
}
