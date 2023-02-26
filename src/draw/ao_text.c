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

#include "ao_draw.h"
#include "ao_draw_int.h"
#include "ao_font.h"
#include <string.h>
#include <stdio.h>

void
ao_text(const struct ao_bitmap	*dst,
	const struct ao_font	*font,
	int16_t			x,
	int16_t			y,
	char			*string,
	uint32_t		fill,
	uint8_t			rop)
{
	int		glyph_stride = (font->max_width + 31) / 32;
	uint32_t	src[glyph_stride * font->max_height];
	char		c;
	int		h;
	int8_t		x_off, y_off, advance;
	int		byte_width;

	struct ao_bitmap	src_bitmap = {
		.base = src,
	};

	rop = (rop & 3) | 0x4;

	if ((fill&1) == 0)
		rop ^= 3;

	if (!font->metrics) {
		src_bitmap.width = font->max_width;
		src_bitmap.height = font->max_width;
		src_bitmap.stride = glyph_stride;
		x_off = 0;
		y_off = font->ascent;
		advance = font->max_width;
	}
	while ((c = *string++)) {
		const uint8_t	*bytes = &font->bytes[font->pos[(uint8_t) c]];

		if (font->metrics) {
			const struct ao_glyph_metrics *m = &font->metrics[(uint8_t) c];
			src_bitmap.width = m->width;
			src_bitmap.height = m->height;
			src_bitmap.stride = (m->width + 31) / 32;
			x_off = m->x_off;
			y_off = m->y_off;
			advance = m->advance;
			byte_width = ((src_bitmap.width + 7) / 8);
		}

		for (h = 0; h < src_bitmap.height; h++)
			memcpy(&src[h * src_bitmap.stride], &bytes[h * byte_width], byte_width);

		ao_copy(dst,
			x + x_off, y - y_off, src_bitmap.width, src_bitmap.height,
			&src_bitmap,
			0, 0, rop);
		x += advance;
	}
}
