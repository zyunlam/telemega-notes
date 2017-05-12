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

class AltosUITimeSeriesExtra {
	Color		color;
	boolean		enabled;
	AltosUIAxis	axis;

	public AltosUITimeSeriesExtra(Color color, boolean enabled, AltosUIAxis axis) {
		this.color = color;
		this.enabled = enabled;
		this.axis = axis;
	}
}

public class AltosUIFlightSeries extends AltosFlightSeries {

	Hashtable<String,AltosUITimeSeriesExtra> extra;

	public void register_extra(String label,
				   Color color,
				   boolean enabled,
				   AltosUIAxis axis) {

		AltosUITimeSeriesExtra e = new AltosUITimeSeriesExtra(color,
								      enabled,
								      axis);
		System.out.printf("register extra label %s extra %s\n", label, e);
		extra.put(label, e);
	}

	public AltosTimeSeries make_series(String label, AltosUnits units) {

		AltosUITimeSeriesExtra e = extra.get(label);

		if (e == null)
			e = extra.get("default");
		return new AltosUITimeSeries(label, units,
					     e.color, e.enabled, e.axis);
	}

	public AltosUITimeSeries[] series() {
		return series.toArray(new AltosUITimeSeries[0]);
	}

	public AltosUIFlightSeries () {
		super();
		extra = new Hashtable<String,AltosUITimeSeriesExtra>();
	}
}
