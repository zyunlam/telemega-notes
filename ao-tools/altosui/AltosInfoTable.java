/*
 * Copyright © 2010 Keith Packard <keithp@keithp.com>
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

package altosui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;
import java.io.*;
import java.util.*;
import java.text.*;
import java.util.prefs.*;
import java.util.concurrent.LinkedBlockingQueue;

public class AltosInfoTable extends JTable {
	private AltosFlightInfoTableModel model;

	private Font infoLabelFont = new Font("SansSerif", Font.PLAIN, 14);
	private Font infoValueFont = new Font("Monospaced", Font.PLAIN, 14);

	static final int info_columns = 3;
	static final int info_rows = 17;

	int desired_row_height() {
		FontMetrics	infoValueMetrics = getFontMetrics(infoValueFont);
		return (infoValueMetrics.getHeight() + infoValueMetrics.getLeading()) * 18 / 10;
	}

	public AltosInfoTable() {
		super(new AltosFlightInfoTableModel(info_rows, info_columns));
		model = (AltosFlightInfoTableModel) getModel();
		setFont(infoValueFont);
		setAutoResizeMode(AUTO_RESIZE_ALL_COLUMNS);
		setShowGrid(true);
		setRowHeight(desired_row_height());
		doLayout();
	}

	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	void info_reset() {
		model.reset();
	}

	void info_add_row(int col, String name, String value) {
		model.addRow(col, name, value);
	}

	void info_add_row(int col, String name, String format, Object... parameters) {
		info_add_row (col, name, String.format(format, parameters));
	}

	void info_add_deg(int col, String name, double v, int pos, int neg) {
		int	c = pos;
		if (v < 0) {
			c = neg;
			v = -v;
		}
		double	deg = Math.floor(v);
		double	min = (v - deg) * 60;

		info_add_row(col, name, String.format("%3.0f°%08.5f'", deg, min));
	}

	void info_finish() {
		model.finish();
	}

	public void clear() {
		model.clear();
	}

	public void show(AltosState state, int crc_errors) {
		if (state == null)
			return;
		info_reset();
		info_add_row(0, "Rocket state", "%s", state.data.state());
		info_add_row(0, "Callsign", "%s", state.data.callsign);
		info_add_row(0, "Rocket serial", "%6d", state.data.serial);
		info_add_row(0, "Rocket flight", "%6d", state.data.flight);

		info_add_row(0, "RSSI", "%6d    dBm", state.data.rssi);
		info_add_row(0, "CRC Errors", "%6d", crc_errors);
		info_add_row(0, "Height", "%6.0f    m", state.height);
		info_add_row(0, "Max height", "%6.0f    m", state.max_height);
		info_add_row(0, "Acceleration", "%8.1f  m/s²", state.acceleration);
		info_add_row(0, "Max acceleration", "%8.1f  m/s²", state.max_acceleration);
		info_add_row(0, "Speed", "%8.1f  m/s", state.ascent ? state.speed : state.baro_speed);
		info_add_row(0, "Max Speed", "%8.1f  m/s", state.max_speed);
		info_add_row(0, "Temperature", "%9.2f °C", state.temperature);
		info_add_row(0, "Battery", "%9.2f V", state.battery);
		info_add_row(0, "Drogue", "%9.2f V", state.drogue_sense);
		info_add_row(0, "Main", "%9.2f V", state.main_sense);
		info_add_row(0, "Pad altitude", "%6.0f    m", state.ground_altitude);
		if (state.gps == null) {
			info_add_row(1, "GPS", "not available");
		} else {
			if (state.gps_ready)
				info_add_row(1, "GPS state", "%s", "ready");
			else
				info_add_row(1, "GPS state", "wait (%d)",
					     state.gps_waiting);
			if (state.data.gps.locked)
				info_add_row(1, "GPS", "   locked");
			else if (state.data.gps.connected)
				info_add_row(1, "GPS", " unlocked");
			else
				info_add_row(1, "GPS", "  missing");
			info_add_row(1, "Satellites", "%6d", state.data.gps.nsat);
			info_add_deg(1, "Latitude", state.gps.lat, 'N', 'S');
			info_add_deg(1, "Longitude", state.gps.lon, 'E', 'W');
			info_add_row(1, "GPS altitude", "%6d", state.gps.alt);
			info_add_row(1, "GPS height", "%6.0f", state.gps_height);

			/* The SkyTraq GPS doesn't report these values */
			if (false) {
				info_add_row(1, "GPS ground speed", "%8.1f m/s %3d°",
					     state.gps.ground_speed,
					     state.gps.course);
				info_add_row(1, "GPS climb rate", "%8.1f m/s",
					     state.gps.climb_rate);
				info_add_row(1, "GPS error", "%6d m(h)%3d m(v)",
					     state.gps.h_error, state.gps.v_error);
			}
			info_add_row(1, "GPS hdop", "%8.1f", state.gps.hdop);

			if (state.npad > 0) {
				if (state.from_pad != null) {
					info_add_row(1, "Distance from pad", "%6d m",
						     (int) (state.from_pad.distance + 0.5));
					info_add_row(1, "Direction from pad", "%6d°",
						     (int) (state.from_pad.bearing + 0.5));
					info_add_row(1, "Elevation from pad", "%6d°",
						     (int) (state.elevation + 0.5));
					info_add_row(1, "Range from pad", "%6d m",
						     (int) (state.range + 0.5));
				} else {
					info_add_row(1, "Distance from pad", "unknown");
					info_add_row(1, "Direction from pad", "unknown");
					info_add_row(1, "Elevation from pad", "unknown");
					info_add_row(1, "Range from pad", "unknown");
				}
				info_add_deg(1, "Pad latitude", state.pad_lat, 'N', 'S');
				info_add_deg(1, "Pad longitude", state.pad_lon, 'E', 'W');
				info_add_row(1, "Pad GPS alt", "%6.0f m", state.pad_alt);
			}
			info_add_row(1, "GPS date", "%04d-%02d-%02d",
				       state.gps.year,
				       state.gps.month,
				       state.gps.day);
			info_add_row(1, "GPS time", "  %02d:%02d:%02d",
				       state.gps.hour,
				       state.gps.minute,
				       state.gps.second);
			int	nsat_vis = 0;
			int	c;

			if (state.gps.cc_gps_sat == null)
				info_add_row(2, "Satellites Visible", "%4d", 0);
			else {
				info_add_row(2, "Satellites Visible", "%4d", state.gps.cc_gps_sat.length);
				for (c = 0; c < state.gps.cc_gps_sat.length; c++) {
					info_add_row(2, "Satellite id,C/N0",
						     "%4d, %4d",
						     state.gps.cc_gps_sat[c].svid,
						     state.gps.cc_gps_sat[c].c_n0);
				}
			}
		}
		info_finish();
	}
}
