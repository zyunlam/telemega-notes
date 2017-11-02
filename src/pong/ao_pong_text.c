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
#include "ao_pong_text.h"

static const uint16_t numbers[] = {
#include "ao_pong_font.h"
};

#define GLYPH_WIDTH	16
#define GLYPH_SPACING	20
#define GLYPH_HEIGHT	24
#define GLYPH_ASCENT	24

const struct ao_font ao_pong_font = {
	.width = GLYPH_SPACING,
	.height = GLYPH_HEIGHT,
	.ascent = GLYPH_ASCENT,
	.descent = GLYPH_HEIGHT - GLYPH_ASCENT,
};

void
ao_pong_text(const struct ao_bitmap	*dst,
	     int16_t			x,
	     int16_t			y,
	     char			*string)
{
	static uint32_t	src[GLYPH_HEIGHT];
	char		c;
	int		h;

       	struct ao_bitmap	src_bitmap = {
		.base = src,
		.stride = 1,
		.width = GLYPH_WIDTH,
		.height = GLYPH_HEIGHT
	};

	y -= GLYPH_ASCENT;

	while ((c = *string++)) {
		if (c == ' ') {
			ao_rect(dst, x, y, GLYPH_WIDTH, GLYPH_HEIGHT, 0, AO_COPY);
		} else {
			const uint16_t	*n = &numbers[(c - '0') * GLYPH_HEIGHT];

			for (h = 0; h < GLYPH_HEIGHT; h++)
				src[h] = n[h];

			ao_copy(dst,
				x, y, GLYPH_WIDTH, GLYPH_HEIGHT,
				&src_bitmap,
				0, 0, AO_COPY);
		}
		x += GLYPH_SPACING;
	}
}
