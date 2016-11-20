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
#include "ao_font.h"

void
ao_text(char		*string,
	uint32_t	*dst_line,
	int16_t		dst_stride,
	int16_t		dst_x)
{
	char		c;
	uint32_t	src[GLYPH_HEIGHT];

	while ((c = *string++)) {
		uint8_t	*bytes = &glyph_bytes[glyph_pos[(uint8_t) c]];
		int	h;

		for (h = 0; h < GLYPH_HEIGHT; h++)
			src[h] = bytes[h];

		ao_blt(src, 1, 0,
		       dst_line, dst_stride,
		       dst_x,
		       GLYPH_WIDTH,
		       GLYPH_HEIGHT,
		       AO_AND_INVERTED,
		       0, 0);
		dst_x += GLYPH_WIDTH;
	}
}
