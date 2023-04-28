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

#include <ao_draw.h>
#include <ao_draw_int.h>
#include <stdio.h>
#include <math.h>
#include <float.h>

const struct ao_transform ao_identity = {
	.x_scale = 1.0f, .x_off = 0.0f,
	.y_scale = 1.0f, .y_off = 0.0f
};

static float
_x(const struct ao_coord	*coords,
   const struct ao_transform	*transform,
   uint16_t			coord)
{
	return ao_t_x_c(&coords[coord], transform);
}

static float
_y(const struct ao_coord	*coords,
   const struct ao_transform	*transform,
   uint16_t			coord)
{
	return ao_t_y_c(&coords[coord], transform);
}

static uint16_t
_next(uint16_t ncoords, uint16_t edge)
{
	return edge == ncoords - 1 ? 0 : edge + 1;
}

/*
 * Return if the given edge is 'live' at the specified y coordinate.
 * That means the edge spans the y value. Horizontal lines are never
 * live
 */
static bool
ao_edge_live(const struct ao_coord	*coords,
	     uint16_t			ncoords,
	     const struct ao_transform	*transform,
	     uint16_t			edge,
	     float			y)
{
	float y1 = _y(coords, transform, edge);
	float y2 = _y(coords, transform, _next(ncoords, edge));

	if (y1 > y2)
		return y2 <= y && y < y1;
	else
		return y1 <= y && y < y2;
}

/*
 * Compute the X coordinate for a given edge at a specified y value
 */
static int16_t
ao_edge_x(const struct ao_coord		*coords,
	  uint16_t			ncoords,
	  const struct ao_transform	*transform,
	  uint16_t			edge,
	  float				y)
{
	uint16_t next_edge = _next(ncoords, edge);
	float x1 = _x(coords, transform, edge);
	float x2 = _x(coords, transform, next_edge);
	float y1 = _y(coords, transform, edge);
	float y2 = _y(coords, transform, next_edge);
	float dx = x2 - x1;
	float dy = y2 - y1;
	float off_y = y - y1;

	return (int16_t) (x1 + (off_y * dx) / dy + 0.5f);
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
ao_next_x(const struct ao_coord		*coords,
	  uint16_t			ncoords,
	  const struct ao_transform	*transform,
	  struct next_x			*this_x,
	  float				y)
{
	uint16_t 	edge;
	float		next_x = FLT_MAX;
	uint16_t	next_edge = UINT16_MAX;
	bool		ret = false;

	for (edge = 0; edge < ncoords; edge++) {
		if (ao_edge_live(coords, ncoords, transform, edge, y)) {
			float	nx = ao_edge_x(coords, ncoords, transform, edge, y);
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
	int16_t	ix1 = (int16_t) floorf(x1 + 0.5f);
	int16_t ix2 = (int16_t) floorf(x2 + 0.5f);
	int16_t iy = (int16_t) y;

	ao_clip(ix1, 0, dst->width);
	ao_clip(ix2, 0, dst->width);
	ao_solid(ao_and(rop, fill),
		 ao_xor(rop, fill),
		 dst->base + iy * dst->stride,
		 dst->stride,
		 ix1,
		 ix2 - ix1,
		 1);
}

/*
 * Compute the 'winding' for the specified edge. Winding is simply an
 * indication of whether the edge goes upwards or downwards
 */
static int
ao_wind(const struct ao_coord		*coords,
	uint16_t			ncoords,
	const struct ao_transform	*transform,
	uint16_t			edge)
{
	uint16_t next_edge = _next(ncoords, edge);
	return _y(coords, transform, edge) > _y(coords, transform, next_edge) ? 1 : -1;
}

/*
 * Fill the specified polygon with non-zero winding rule
 */
void
ao_poly(struct ao_bitmap		*dst,
	const struct ao_coord		*coords,
	uint16_t			ncoords,
	const struct ao_transform	*transform,
	uint32_t			fill,
	uint8_t			rop)
{
	float		y_min, y_max;
	float		x_min, x_max;
	uint16_t	edge;
	float		y;
	float		x;
	struct next_x	next_x;
	int		wind;

	if (!transform)
		transform = &ao_identity;

	/*
	 * Find the limits of the polygon
	 */
	x_min = x_max = _x(coords, transform, 0);
	y_min = y_max = _y(coords, transform, 0);
	for (edge = 1; edge < ncoords; edge++) {
		x = _x(coords, transform, edge);
		if (x < x_min)
			x_min = x;
		else if (x > x_max)
			x_max = x;
		y = _y(coords, transform, edge);
		if (y < y_min)
			y_min = y;
		else if (y > y_max)
			y_max = y;
	}

	x_min = floorf(x_min);
	x_max = ceilf(x_max);
	ao_clip(x_min, 0, dst->width);
	ao_clip(x_max, 0, dst->width);

	y_min = floorf(y_min);
	y_max = ceilf(y_max);
	ao_clip(y_min, 0, dst->height);
	ao_clip(y_max, 0, dst->height);

	ao_damage(dst, (int16_t) x_min, (int16_t) y_min, (int16_t) x_max, (int16_t) y_max);

	/*
	 * Walk each scanline in the range and fill included spans
	 */
	for (y = y_min; y < y_max; y++) {
		x = INT16_MIN;
		next_x.x = INT16_MIN;
		next_x.edge = 0;
		wind = 0;
		while (ao_next_x(coords, ncoords, transform, &next_x, y)) {

			/*
			 * Fill the previous span if winding is
			 * non-zero
			 */
			if (wind != 0)
				ao_span(dst, x, next_x.x, y, fill, rop);

			/* Adjust winding for the current span */
			wind += ao_wind(coords, ncoords, transform, next_x.edge);

			/* Step to next span start x value */
			x = next_x.x;
		}
	}
}
