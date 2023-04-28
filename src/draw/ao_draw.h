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

#ifndef _AO_DRAW_H_
#define _AO_DRAW_H_

#include <stdint.h>
#include <stdbool.h>
#include "ao_font.h"
#include "ao_box.h"

struct ao_bitmap {
	uint32_t	*base;
	int16_t		stride;	/* in units */
	int16_t		width;	/* in pixels */
	int16_t		height;	/* in pixels */
	struct ao_box	damage;
};

struct ao_coord {
	float	x, y;
};

struct ao_transform {
	float	x_scale, x_off;
	float	y_scale, y_off;
};

static inline float ao_t_x(float x, float y, const struct ao_transform *t)
{
	(void) y;
	return x * t->x_scale + t->x_off;
}

static inline int16_t ao_t_xi(float x, float y, const struct ao_transform *t)
{
	return (int16_t) (ao_t_x(x, y, t) + 0.5f);
}

static inline float ao_t_y(float x, float y, const struct ao_transform *t)
{
	(void) x;
	return y * t->y_scale + t->y_off;
}

static inline int16_t ao_t_yi(float x, float y, const struct ao_transform *t)
{
	return (int16_t) (ao_t_y(x, y, t) + 0.5f);
}

static inline float ao_t_x_c(const struct ao_coord	*c,
			     const struct ao_transform	*t)
{
	return ao_t_x(c->x, c->y, t);
}

static inline float ao_t_y_c(const struct ao_coord	*c,
			     const struct ao_transform	*t)
{
	return ao_t_y(c->x, c->y, t);
}

static inline int16_t
ao_stride(int16_t width)
{
	return (int16_t) ((width + 31) >> 5);
}

static inline int16_t
ao_stride_bytes(int16_t width)
{
	return (int16_t) ((width + 7) >> 3);
}

static inline void
ao_damage_set_empty(struct ao_bitmap *dst)
{
	ao_box_set_empty(&dst->damage);
}

struct ao_pattern {
	uint8_t		pattern[8];
};

struct ao_glyph_metrics {
	int8_t	width;
	int8_t	height;
	int8_t	x_off;
	int8_t	y_off;
	int8_t	advance;
};

struct ao_font {
	const uint8_t	*bytes;
	const uint16_t	*pos;
	const struct ao_glyph_metrics *metrics;
	int16_t	max_width;
	int16_t	max_height;
	int16_t	ascent;
};

void
ao_copy(struct ao_bitmap	*dst,
	int16_t			dst_x,
	int16_t			dst_y,
	int16_t			width,
	int16_t			height,
	const struct ao_bitmap	*src,
	int16_t			src_x,
	int16_t			src_y,
	uint8_t			rop);

void
ao_rect(struct ao_bitmap	*dst,
	int16_t			x,
	int16_t			y,
	int16_t			width,
	int16_t			height,
	uint32_t		fill,
	uint8_t			rop);

void
ao_pattern(struct ao_bitmap		*dst,
	   int16_t			x,
	   int16_t			y,
	   int16_t			width,
	   int16_t			height,
	   const struct ao_pattern	*pattern,
	   int16_t			pat_x,
	   int16_t			pat_y,
	   uint8_t			rop);

void
ao_line(struct ao_bitmap	*dst,
	int16_t			x1,
	int16_t			y1,
	int16_t			x2,
	int16_t			y2,
	uint32_t		fill,
	uint8_t			rop);

void
ao_poly(struct ao_bitmap		*dst,
	const struct ao_coord		*coords,
	uint16_t			ncoords,
	const struct ao_transform	*transform,
	uint32_t			fill,
	uint8_t				rop);

void
ao_text(struct ao_bitmap	*dst,
	const struct ao_font	*font,
	int16_t			x,
	int16_t			y,
	char			*string,
	uint32_t		fill,
	uint8_t			rop);

void
ao_logo(struct ao_bitmap		*dst,
	const struct ao_transform	*transform,
	const struct ao_font		*font,
	uint32_t			fill,
	uint8_t				rop);

extern const struct ao_transform ao_identity;

#define AO_SHIFT	5
#define AO_UNIT		(1 << AO_SHIFT)
#define AO_MASK		(AO_UNIT - 1)
#define AO_ALLONES	((uint32_t) -1)

/*
 *	    dst
 *	   0   1
 *
 *	0  a   b
 *  src
 *	1  c   d
 *
 *	ROP = abcd
 */

#define AO_CLEAR         0x0	/* 0 */
#define AO_AND           0x1	/* src AND dst */
#define AO_AND_REVERSE   0x2	/* src AND NOT dst */
#define AO_COPY          0x3	/* src */
#define AO_AND_INVERTED  0x4	/* NOT src AND dst */
#define AO_NOOP          0x5	/* dst */
#define AO_XOR           0x6	/* src XOR dst */
#define AO_OR            0x7	/* src OR dst */
#define AO_NOR           0x8	/* NOT src AND NOT dst */
#define AO_EQUIV         0x9	/* NOT src XOR dst */
#define AO_INVERT        0xa	/* NOT dst */
#define AO_OR_REVERSE    0xb	/* src OR NOT dst */
#define AO_COPY_INVERTED 0xc	/* NOT src */
#define AO_OR_INVERTED   0xd	/* NOT src OR dst */
#define AO_NAND          0xe	/* NOT src OR NOT dst */
#define AO_SET           0xf	/* 1 */

#endif /* _AO_DRAW_H_ */
