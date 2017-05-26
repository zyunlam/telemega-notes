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

package org.altusmetrum.altoslib_11;

import java.util.*;

public class AltosTimeSeries implements Iterable<AltosTimeValue> {
	public String			label;
	public AltosUnits		units;
	ArrayList<AltosTimeValue>	values;

	public void add(AltosTimeValue tv) {
		values.add(tv);
	}

	public void add(double time, double value) {
		add(new AltosTimeValue(time, value));
	}

	public AltosTimeValue get(int i) {
		return values.get(i);
	}

	private double lerp(AltosTimeValue v0, AltosTimeValue v1, double t) {
		/* degenerate case */
		if (v0.time == v1.time)
			return (v0.value + v1.value) / 2;

		return (v0.value * (v1.time - t) + v1.value * (t - v0.time)) / v1.time - v0.time;
	}

	private int after_index(double time) {
		int	lo = 0;
		int	hi = values.size() - 1;

		while (lo <= hi) {
			int mid = (lo + hi) / 2;

			if (values.get(mid).time < time)
				lo = mid + 1;
			else
				hi = mid - 1;
		}
		return lo;
	}

	/* Compute a value for an arbitrary time */
	public double value(double time) {
		int after = after_index(time);
		if (after == 0)
			return values.get(0).value;
		if (after == values.size())
			return values.get(after - 1).value;

		return lerp(values.get(after-1), values.get(after), time);
	}

	/* Find the value just before an arbitrary time */
	public double value_before(double time) {
		int after = after_index(time);

		if (after == 0)
			return values.get(0).value;
		return values.get(after-1).value;
	}

	/* Find the value just after an arbitrary time */
	public double value_after(double time) {
		int after = after_index(time);

		if (after == values.size())
			return values.get(after-1).value;
		return values.get(after).value;
	}

	public double time_of(double value) {
		double	last = AltosLib.MISSING;
		for (AltosTimeValue v : values) {
			if (v.value >= value)
				return v.time;
			last = v.time;
		}
		return last;
	}

	public int size() {
		return values.size();
	}

	public Iterator<AltosTimeValue> iterator() {
		return values.iterator();
	}

	public double max() {
		double max = AltosLib.MISSING;
		for (AltosTimeValue tv : values) {
			if (max == AltosLib.MISSING || tv.value > max)
				max = tv.value;
		}
		return max;
	}

	public double max(double start_time, double end_time) {
		double max = AltosLib.MISSING;
		for (AltosTimeValue tv : values) {
			if (start_time <= tv.time && tv.time <= end_time)
				if (max == AltosLib.MISSING || tv.value > max)
					max = tv.value;
		}
		return max;
	}

	public double min() {
		double min = AltosLib.MISSING;
		for (AltosTimeValue tv : values) {
			if (min == AltosLib.MISSING || tv.value < min)
				min = tv.value;
		}
		return min;
	}

	public double min(double start_time, double end_time) {
		double min = AltosLib.MISSING;
		for (AltosTimeValue tv : values) {
			if (start_time <= tv.time && tv.time <= end_time)
				if (min == AltosLib.MISSING || tv.value < min)
					min = tv.value;
		}
		return min;
	}

	public double average() {
		double total = 0;
		int count = 0;
		for (AltosTimeValue tv : values) {
			total += tv.value;
			count++;
		}
		if (count == 0)
			return AltosLib.MISSING;
		return total / count;
	}

	public double average(double start_time, double end_time) {
		double total = 0;
		int count = 0;
		for (AltosTimeValue tv : values) {
			if (start_time <= tv.time && tv.time <= end_time) {
				total += tv.value;
				count++;
			}
		}
		if (count == 0)
			return AltosLib.MISSING;
		return total / count;
	}

	public AltosTimeSeries integrate(AltosTimeSeries integral) {
		double	value = 0.0;
		double	pvalue = 0.0;
		double 	time = 0.0;
		boolean start = true;

		for (AltosTimeValue v : values) {
			if (start) {
				value = 0.0;
				start = false;
			} else {
				value += (pvalue + v.value) / 2.0 * (v.time - time);
			}
			pvalue = v.value;
			time = v.time;
//			System.out.printf("%g %g %g\n", time, v.value, value);
			integral.add(time, value);

		}
		return integral;
	}

	public AltosTimeSeries differentiate(AltosTimeSeries diff) {
		double value = 0.0;
		double time = 0.0;
		boolean start = true;

		for (AltosTimeValue v: values) {
			if (start) {
				value = v.value;
				time = v.time;
				start = false;
			} else {
				double	dx = v.time - time;
				double	dy = v.value - value;

				if (dx != 0)
					diff.add(time, dy/dx);

				time = v.time;
				value = v.value;
			}
		}
		return diff;
	}

	private int find_left(int i, double dt) {
		int j;
		double t = values.get(i).time - dt;
		for (j = i; j >= 0; j--)	{
			if (values.get(j).time < t)
				break;
		}
		return j + 1;

	}

	private int find_right(int i, double dt) {
		int j;
		double t = values.get(i).time + dt;
		for (j = i; j < values.size(); j++) {
			if (values.get(j).time > t)
				break;
		}
		return j - 1;

	}

	private double filter_coeff(double dist, double width) {
		double ratio = dist / (width / 2);

		return Math.cos(ratio * Math.PI / 2);
	}

	public AltosTimeSeries filter(AltosTimeSeries f, double width) {
		double	half_width = width/2;
		for (int i = 0; i < values.size(); i++) {
			double	center_time = values.get(i).time;
			double	left_time = center_time - half_width;
			double	right_time = center_time + half_width;
			double	total_coeff = 0.0;
			double	total_value = 0.0;

			int	left = find_left(i, half_width);
			int	right = find_right(i, half_width);

			for (int j = left; j <= right; j++) {
				double	j_time = values.get(j).time;

				if (left_time <= j_time && j_time <= right_time) {
					double	j_left = j == left ? left_time : values.get(j-1).time;
					double	j_right = j == right ? right_time : values.get(j+1).time;
					double	interval = (j_right - j_left) / 2.0;
					double	coeff = filter_coeff(j_time - center_time, width) * interval;

					total_coeff += coeff;
					total_value += coeff * values.get(j).value;
				}
			}
			if (total_coeff != 0.0)
				f.add(center_time, total_value / total_coeff);
		}
		return f;
	}

	public AltosTimeSeries(String label, AltosUnits units) {
		this.label = label;
		this.units = units;
		this.values = new ArrayList<AltosTimeValue>();
	}
}
