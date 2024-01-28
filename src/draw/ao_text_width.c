/*
 * Copyright © 2024 Keith Packard <keithp@keithp.com>
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
#include "ao_font.h"
#include <string.h>
#include <stdio.h>

int16_t
ao_text_width(const struct ao_font	*font,
	      const char		*string)
{
	char		c;
	int16_t		x = 0;

	while ((c = *string++)) {
		if (font->metrics) {
			x += font->metrics[(uint8_t) c].advance;
		} else {
			x += font->max_width;
		}
	}
	return x;
}
