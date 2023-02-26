/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
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
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "ao_draw.h"
#include "ao_draw_int.h"
#include <stdio.h>
#include <math.h>
#include <float.h>

/*
 * Return if the given edge is 'live' at the specified y coordinate.
 * That means the edge spans the y value. Horizontal lines are never
 * live
 */
static bool
ao_edge_live(const struct ao_coord	*coords,
	     uint16_t			ncoords,
	     uint16_t			edge,
	     float			y)
{
	int next_edge = (edge == ncoords - 1) ? 0 : edge + 1;
	float y1 = coords[edge].y;
	float y2 = coords[next_edge].y;

	if (y1 > y2)
		return y2 <= y && y < y1;
	else
		return y1 <= y && y < y2;
}

/*
 * Compute the X coordinate for a given edge at a specified y value
 */
static int16_t
ao_edge_x(const struct ao_coord	*coords,
	  uint16_t		ncoords,
	  uint16_t		edge,
	  float			y)
{
	int	next_edge = (edge == ncoords - 1) ? 0 : edge + 1;
	float x1 = coords[edge].x;
	float x2 = coords[next_edge].x;
	float y1 = coords[edge].y;
	float y2 = coords[next_edge].y;
	float dx = x2 - x1;
	float dy = y2 - y1;
	float off_y = y - y1;

	return x1 + (off_y * dx) / dy;
}

struct next_x {
	float		x;
	uint16_t	edge;
};

/*
 * Find the next X/edge pair along the given scanline.  If two edges
 * have the same X value, return them in edge index order. Returns false
 * if there are no more edges.
 */
static bool
ao_next_x(const struct ao_coord	*coords,
	  uint16_t		ncoords,
	  struct next_x		*this_x,
	  float		y)
{
	uint16_t 	edge;
	float		next_x = FLT_MAX;
	uint16_t	next_edge = UINT16_MAX;
	bool		ret = false;

	for (edge = 0; edge < ncoords; edge++) {
		if (ao_edge_live(coords, ncoords, edge, y)) {
			float	nx = ao_edge_x(coords, ncoords, edge, y);
			if (this_x->x < nx || (this_x->x == nx && this_x->edge < edge)) {
				if (nx < next_x) {
					next_x = nx;
					next_edge = edge;
					ret = true;
				}
			}
		}
	}
	this_x->x = next_x;
	this_x->edge = next_edge;
	return ret;
}

/*
 * Fill a span at the specified y coordinate from x1 to x2
 */
static void
ao_span(const struct ao_bitmap	*dst,
	float			x1,
	float			x2,
	float			y,
	uint32_t		fill,
	uint8_t			rop)
{
	int16_t	ix1 = floorf(x1 + 0.5f);
	int16_t ix2 = floorf(x2 + 0.5f);
	int16_t iy = (int16_t) y;
	ao_rect(dst, ix1, iy, ix2 - ix1, 1, fill, rop);
}

/*
 * Compute the 'winding' for the specified edge. Winding is simply an
 * indication of whether the edge goes upwards or downwards
 */
static int
ao_wind(const struct ao_coord	*coords,
	uint16_t		ncoords,
	uint16_t		edge)
{
	uint16_t next_edge = (edge == ncoords - 1) ? 0 : edge + 1;
	return coords[edge].y > coords[next_edge].y ? 1 : -1;
}

/*
 * Fill the specified polygon with non-zero winding rule
 */
void
ao_poly(const struct ao_bitmap	*dst,
	const struct ao_coord	*coords,
	uint16_t		ncoords,
	uint32_t		fill,
	uint8_t			rop)
{
	float		y_min, y_max;
	uint16_t	edge;
	float		y;
	float		x;
	struct next_x	next_x;
	int		wind;

	/*
	 * Find the y limits of the polygon
	 */
	y_min = y_max = coords[0].y;
	for (edge = 1; edge < ncoords; edge++) {
		y = coords[edge].y;
		if (y < y_min)
			y_min = y;
		else if (y > y_max)
			y_max = y;
	}

	y_min = floorf(y_min);
	y_max = ceilf(y_max);

	/*
	 * Walk each scanline in the range and fill included spans
	 */
	for (y = y_min; y < y_max; y++) {
		x = INT16_MIN;
		next_x.x = INT16_MIN;
		next_x.edge = 0;
		wind = 0;
		while (ao_next_x(coords, ncoords, &next_x, y)) {

			/*
			 * Fill the previous span if winding is
			 * non-zero
			 */
			if (wind != 0)
				ao_span(dst, x, next_x.x, y, fill, rop);

			/* Adjust winding for the current span */
			wind += ao_wind(coords, ncoords, next_x.edge);

			/* Step to next span start x value */
			x = next_x.x;
		}
	}
}
