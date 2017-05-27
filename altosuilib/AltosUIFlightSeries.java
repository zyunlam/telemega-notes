/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

package org.altusmetrum.altosuilib_11;

import java.util.*;
import java.awt.*;
import javax.swing.*;
import org.altusmetrum.altoslib_11.*;

import org.jfree.ui.*;
import org.jfree.chart.*;
import org.jfree.chart.plot.*;
import org.jfree.chart.axis.*;
import org.jfree.chart.renderer.*;
import org.jfree.chart.renderer.xy.*;
import org.jfree.chart.labels.*;
import org.jfree.data.xy.*;
import org.jfree.data.*;

class AltosUITimeSeriesAxis {
	Color		color;
	boolean		enabled;
	boolean		marker;
	boolean		marker_top;
	AltosUIAxis	axis;
	XYPlot		plot;

	public AltosUITimeSeriesAxis(Color color, boolean enabled, AltosUIAxis axis, XYPlot plot, boolean marker, boolean marker_top) {
		this.color = color;
		this.enabled = enabled;
		this.axis = axis;
		this.plot = plot;
		this.marker = marker;
		this.marker_top = marker_top;
	}
}

public class AltosUIFlightSeries extends AltosFlightSeries {

	Hashtable<String,AltosUITimeSeriesAxis> axes;

	AltosUIFlightSeries flight_series;

	void fill_axes(String label, AltosUITimeSeriesAxis axis) {
		for (AltosTimeSeries ts : series) {
			AltosUITimeSeries uts = (AltosUITimeSeries) ts;

			if (label.equals(ts.label) || (label.equals("default") && uts.color == null)) {
				if (axis.marker)
					uts.set_marker(axis.color, axis.enabled, axis.plot, axis.marker_top);
				else
					uts.set_axis(axis.color, axis.enabled, axis.axis);
			}
		}
	}

	public void register_axis(String label,
				  Color color,
				  boolean enabled,
				  AltosUIAxis axis) {
		AltosUITimeSeriesAxis tsa = new AltosUITimeSeriesAxis(color,
								      enabled,
								      axis,
								      null,
								      false,
								      false);
		axes.put(label, tsa);
		fill_axes(label, tsa);
	}

	public void register_marker(String label,
				    Color color,
				    boolean enabled,
				    XYPlot plot,
				    boolean marker_top) {
		AltosUITimeSeriesAxis tsa = new AltosUITimeSeriesAxis(color,
								      enabled,
								      null,
								      plot,
								      true,
								      marker_top);
		axes.put(label, tsa);
		fill_axes(label, tsa);
	}

	public AltosTimeSeries make_series(String label, AltosUnits units) {


		AltosUITimeSeries time_series = new AltosUITimeSeries(label, units);

		AltosUITimeSeriesAxis tsa = axes.get(label);
		if (tsa == null)
			tsa = axes.get("default");
		if (tsa != null) {
			if (tsa.marker)
				time_series.set_marker(tsa.color, tsa.enabled, tsa.plot, tsa.marker_top);
			else
				time_series.set_axis(tsa.color, tsa.enabled, tsa.axis);
		}
		return time_series;
	}

	public AltosUITimeSeries[] series(AltosCalData cal_data) {
		finish();
		return series.toArray(new AltosUITimeSeries[0]);
	}

	public AltosUIFlightSeries (AltosCalData cal_data) {
		super(cal_data);
		axes = new Hashtable<String,AltosUITimeSeriesAxis>();
	}
}
