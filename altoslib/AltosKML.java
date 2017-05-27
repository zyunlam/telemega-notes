/*
 * Copyright © 2010 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

package org.altusmetrum.altoslib_11;

import java.io.*;
import java.util.*;

class KMLWriter extends PrintWriter {
	public PrintWriter printf(String format, Object ... arguments) {
		return printf(Locale.ROOT, format, arguments);
	}

	public KMLWriter(File name) throws FileNotFoundException {
		super(name);
	}
}

public class AltosKML implements AltosWriter {

	File			name;
	PrintWriter		out;
	int			flight_state = -1;
	AltosGPS		prev = null;
	double			gps_start_altitude = AltosLib.MISSING;
	AltosFlightStats	stats;

	static final String[] kml_state_colors = {
		"FF000000",	// startup
		"FF000000",	// idle
		"FF000000",	// pad
		"FF0000FF",	// boost
		"FF4080FF",	// fast
		"FF00FFFF",	// coast
		"FFFF0000",	// drogue
		"FF00FF00",	// main
		"FF000000",	// landed
		"FFFFFFFF",	// invalid
		"FFFF0000",	// stateless
	};

	static String state_color(int state) {
		if (state < 0 || kml_state_colors.length <= state)
			return kml_state_colors[AltosLib.ao_flight_invalid];
		return kml_state_colors[state];
	}

	static final String kml_header_start =
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
		"<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n" +
		"<Document>\n" +
		"  <name>AO Flight#%d S/N: %03d</name>\n" +
		"  <description>\n";
	static final String kml_header_end =
		"  </description>\n" +
		"  <open>0</open>\n";

	static final String kml_style_start =
		"  <Style id=\"ao-flightstate-%s\">\n" +
		"    <LineStyle><color>%s</color><width>4</width></LineStyle>\n" +
		"    <BalloonStyle>\n" +
		"      <text>\n";

	static final String kml_style_end =
		"      </text>\n" +
		"    </BalloonStyle>\n" +
		"  </Style>\n";

	static final String kml_placemark_start =
		"  <Placemark>\n" +
		"    <name>%s</name>\n" +
		"    <styleUrl>#ao-flightstate-%s</styleUrl>\n" +
		"    <LineString>\n" +
		"      <tessellate>1</tessellate>\n" +
		"      <altitudeMode>absolute</altitudeMode>\n" +
		"      <coordinates>\n";

	static final String kml_coord_fmt =
	"        %.7f,%.7f,%.7f <!-- alt %12.7f time %12.7f sats %d -->\n";

	static final String kml_placemark_end =
		"      </coordinates>\n" +
		"    </LineString>\n" +
		"  </Placemark>\n";

	static final String kml_footer =
		"</Document>\n" +
		"</kml>\n";

	void start (AltosCalData cal_data) {
		AltosGPS gps = cal_data.gps_pad;

		gps_start_altitude = cal_data.gps_pad_altitude;
		out.printf(kml_header_start, cal_data.flight, cal_data.serial);
		out.printf("Date:   %04d-%02d-%02d\n",
			   gps.year, gps.month, gps.day);
		out.printf("Time:     %2d:%02d:%02d\n",
			   gps.hour, gps.minute, gps.second);
		out.printf("%s", kml_header_end);
	}

	boolean	started = false;

	void state_start(int state) {
		String	state_name = AltosLib.state_name(state);
		String	state_color = state_color(state);
		out.printf(kml_style_start, state_name, state_color);
		out.printf("State: %s\n", state_name);
		out.printf("Time: %6.2f s\n", stats.state_end[state] - stats.state_start[state]);
		out.printf("Average speed: %s\n", AltosConvert.speed.show(6, stats.state_speed[state]));
		out.printf("Average accel: %s\n", AltosConvert.accel.show(6, stats.state_accel[state]));
		out.printf("%s", kml_style_end);
		out.printf(kml_placemark_start, state_name, state_name);
	}

	void state_end() {
		out.printf("%s", kml_placemark_end);
	}

	void coord(double time, AltosGPS gps, int state, double height) {
		double		altitude;

		if (height != AltosLib.MISSING)
			altitude = height + gps_start_altitude;
		else
			altitude = gps.alt;
		out.printf(kml_coord_fmt,
			   gps.lon, gps.lat,
			   altitude, (double) gps.alt,
			   time, gps.nsat);
	}

	void end() {
		out.printf("%s", kml_footer);
	}

	public void close() {
		if (prev != null) {
			state_end();
			end();
			prev = null;
		}
		if (out != null) {
			out.close();
			out = null;
		}
	}

	public void write(AltosGPSTimeValue gtv, AltosCalData cal_data, int state, double height) {
		AltosGPS gps = gtv.gps;
		if (gps.lat == AltosLib.MISSING)
			return;
		if (gps.lon == AltosLib.MISSING)
			return;
		if (state != flight_state) {
			flight_state = state;
			if (prev != null) {
				coord(gtv.time, gps, state, height);
				state_end();
			}
			state_start(state);
		}
		coord(0, gps, state, height);
		prev = gps;
	}

	private int state(AltosFlightSeries series, double time) {
		return (int) series.value_before(AltosFlightSeries.state_name, time);
	}

	private double height(AltosFlightSeries series, double time) {
		return series.value(AltosFlightSeries.height_name, time);
	}

	public void write(AltosFlightSeries series) {
		stats = new AltosFlightStats(series);
		start(series.cal_data);
		for (AltosGPSTimeValue gtv : series.gps_series)
			write(gtv, series.cal_data, state(series, gtv.time), height(series, gtv.time));
	}

	public AltosKML(File in_name) throws FileNotFoundException {
		name = in_name;
		out = new KMLWriter(name);
	}

	public AltosKML(String in_string) throws FileNotFoundException {
		this(new File(in_string));
	}
}
