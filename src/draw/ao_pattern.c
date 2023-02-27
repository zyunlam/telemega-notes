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

#include <ao_draw.h>
#include <ao_draw_int.h>

static inline uint32_t
ao_pattern_expand(uint8_t v, uint8_t rot)
{
	uint32_t	r;

	if (rot)
		v = (uint8_t) ao_left(v, 8-rot) | (uint8_t) ao_right(v, rot);
	r = v;
	return (r << 24) | (r << 16) | (r << 8) | (r);
}

static inline int
min(int a, int b) {
	return a < b ? a : b;
}

void
ao_pattern(struct ao_bitmap		*dst,
	   int16_t			x,
	   int16_t			y,
	   int16_t			width,
	   int16_t			height,
	   const struct ao_pattern	*pattern,
	   int16_t			pat_x,
	   int16_t			pat_y,
	   uint8_t			rop)
{
	uint32_t	pat[8];

	int16_t	x2 = x + width;
	int16_t y2 = y + height;

	ao_clip(x, 0, dst->width);
	ao_clip(x2, 0, dst->width);
	ao_clip(y, 0, dst->height);
	ao_clip(y2, 0, dst->height);

	ao_damage(dst, x, y, x2, y2);

	if (x < x2 && y < y2) {
		uint8_t	xrot = (x - pat_x) & 7;
		uint8_t	yrot = (y - pat_y) & 7;
		uint8_t	i;
		int16_t	dst_x, dst_y;

		for (i = 0; i < 8; i++)
			pat[(i + yrot) & 7] = ao_pattern_expand(pattern->pattern[i], xrot);
		for (dst_y = y; dst_y < y2; dst_y += 8) {
			int16_t	h = (int16_t) min(y2 - dst_y, 8);
			for (dst_x = x; dst_x < x2; dst_x += 8) {
				int16_t	w = (int16_t) min(x2 - dst_x, 8);

				ao_blt(pat, 1, 0,
				       dst->base + dst_y * dst->stride,
				       dst->stride,
				       dst_x,
				       w, h,
				       rop,
				       0, 0);
			}
		}
	}
}

