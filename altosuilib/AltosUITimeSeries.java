/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

package org.altusmetrum.altosuilib_11;

import java.io.*;
import java.util.ArrayList;

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

class AltosXYSeries extends XYSeries {

	public AltosXYSeries(String label) {
		super(label);
	}
}

public class AltosUITimeSeries extends AltosTimeSeries implements AltosUIGrapher {
	Color		color;
	boolean		enable;
	AltosUIAxis	axis;
	XYItemRenderer	renderer;
	AltosXYSeries	xy_series;

	/* AltosUIGrapher interface */
	public boolean need_reset() {
		return false;
	}

	public void clear() {
	}

	public void add(AltosUIDataPoint dataPoint) {
	}

	public void setNotify(boolean notify) {
	}

	public void fireSeriesChanged() {
	}

	void set_data() {
		xy_series.clear();

		for (AltosTimeValue v : this) {
			double y = v.y;
			if (units != null)
				y = units.graph_value(y);
			xy_series.add(v.x, y);
		}
	}

	public void set_units() {
		axis.set_units();
		StandardXYToolTipGenerator	ttg;

		if (units != null) {
			String	time_example = (new AltosUITime()).graph_format(7);
			String  example = units.graph_format(7);

			ttg = new StandardXYToolTipGenerator(String.format("{1}s: {2}%s ({0})",
									   units.graph_units()),
							     new java.text.DecimalFormat(time_example),
							     new java.text.DecimalFormat(example));
			renderer.setBaseToolTipGenerator(ttg);
		}
		set_data();
	}

	public AltosXYSeries xy_series() {
		return xy_series;
	}

	public void set_enable(boolean enable) {
		if (this.enable != enable) {
			this.enable = enable;
			renderer.setSeriesVisible(0, enable);
			axis.set_enable(enable);
		}
	}

	public AltosUITimeSeries(String label, AltosUnits units,
				 Color color, boolean enable,
				 AltosUIAxis axis) {
		super(label, units);
		System.out.printf("time series %s units %s\n", label, units);
		this.color = color;
		this.enable = enable;
		this.axis = axis;

		axis.ref(this.enable);

		renderer = new XYLineAndShapeRenderer(true, false);
		renderer.setSeriesPaint(0, color);
		renderer.setSeriesStroke(0, new BasicStroke(2, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
		renderer.setSeriesVisible(0, enable);
		xy_series = new AltosXYSeries(label);
	}
}
