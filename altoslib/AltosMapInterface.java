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
import java.net.*;

public interface AltosMapInterface {
	public abstract AltosMapPath new_path();

	public abstract AltosMapLine new_line();

	public abstract AltosImage load_image(File file) throws Exception;

	public abstract AltosMapMark new_mark(double lat, double lon, int state);

	public abstract AltosMapTile new_tile(AltosMapTileListener listener, AltosLatLon upper_left, AltosLatLon center, int zoom, int maptype, int px_size);

	public abstract int width();

	public abstract int height();

	public abstract void repaint();

	public abstract void repaint(AltosRectangle damage);

	public abstract void set_zoom_label(String label);

	public abstract void debug(String format, Object ... arguments);

	public abstract void select_object(AltosLatLon latlon);
}
