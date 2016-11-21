/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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

#include "ao.h"
#include "ao_draw.h"
#include "ao_draw_int.h"

#define ao_mask(x,w)	(ao_right(AO_ALLONES,(x) & AO_MASK) & \
			 ao_left(AO_ALLONES,(FB_UNIT - ((x)+(w))) & AO_MASK))


/* out of clip region codes */
#define OUT_LEFT 0x08
#define OUT_RIGHT 0x04
#define OUT_ABOVE 0x02
#define OUT_BELOW 0x01

/* major axis for bresenham's line */
#define X_AXIS	0
#define Y_AXIS	1

static void
ao_bres(const struct ao_bitmap	*dst_bitmap,
	int16_t		signdx,
	int16_t		signdy,
	int16_t		axis,
	int16_t		x1,
	int16_t		y1,
	int16_t		e,
	int16_t		e1,
	int16_t		e3,
	int16_t		len,
	uint32_t	and,
	uint32_t	xor)
{
	int16_t		stride = dst_bitmap->stride;
	uint32_t	*dst = dst_bitmap->base;
	uint32_t	mask0, mask;

	mask0 = 1;
	if (signdx < 0)
		mask0 = ao_right(1, AO_UNIT - 1);

	if (signdy < 0)
		stride = -stride;

	dst = dst + y1 * stride + (x1 >> AO_SHIFT);
	mask = ao_right(1, x1 & AO_MASK);

	while (len--) {
		/* clip each point */

		if (0 <= x1 && x1 < dst_bitmap->width &&
		    0 <= y1 && y1 < dst_bitmap->height)
			*dst = ao_do_mask_rrop(*dst, and, xor, mask);

		if (axis == X_AXIS) {
			if (signdx < 0)
				mask = ao_left(mask, 1);
			else
				mask = ao_right(mask, 1);
			if (!mask) {
				dst += signdx;
				mask = mask0;
			}
			x1 += signdx;
			e += e1;
			if (e >= 0) {
				dst += stride;
				e += e3;
				y1 += signdy;
			}
		} else {
			dst += stride;
			e += e1;
			y1 += signdy;
			if (e >= 0) {
				if (signdx < 0)
					mask = ao_left(mask, 1);
				else
					mask = ao_right(mask, 1);
				if (!mask) {
					dst += signdx;
					mask = mask0;
				}
				e += e3;
				x1 += signdx;
			}
		}
	}
}


#define bound(val,max) do {			\
		if (val < 0) {			\
			val = 0;		\
		}				\
		if (val > max) {		\
			val = max;		\
		}				\
	} while (0)

void
ao_line(const struct ao_bitmap	*dst,
	int16_t			x1,
	int16_t			y1,
	int16_t			x2,
	int16_t			y2,
	uint32_t		fill,
	uint8_t			rop)
{
	int16_t	adx, ady;
	int16_t	e, e1, e2, e3;
	int16_t	signdx = 1, signdy = 1;
	int16_t axis;
	int16_t len;

	if ((adx = x2 - x1) < 0) {
		adx = -adx;
		signdx = -1;
	}
	if ((ady = y2 - y1) < 0) {
		ady = -ady;
		signdy = -1;
	}

	if (adx > ady) {
		axis = X_AXIS;
		e1 = ady << 1;
		e2 = e1 - (adx << 1);
		e = e1 - adx;
		len = adx;
	} else {
		axis = Y_AXIS;
		e1 = adx << 1;
		e2 = e1 - (ady << 1);
		e = e1 - ady;
		len = ady;
	}

	e3 = e2 - e1;
	e = e - e1;

	ao_bres(dst,
		signdx,
		signdy,
		axis,
		x1,
		y1,
		e, e1, e3, len,
		ao_and(rop, fill),
		ao_xor(rop, fill));
}
