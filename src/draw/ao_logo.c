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
#include "ao_logo.h"

#define ARRAYSIZE(a)	(sizeof(a) / sizeof((a)[0]))

void
ao_logo(struct ao_bitmap		*dst,
	const struct ao_transform	*transform,
	const struct ao_font		*font,
	uint32_t			fill,
	uint8_t				rop)
{
	if (!transform)
		transform = &ao_identity;
	int16_t name_x = ao_t_xi(ao_logo_width, 0.0f, transform);
	int16_t name_y1 = ao_t_yi(ao_logo_width, 0.5f, transform);
	int16_t name_y2 = ao_t_yi(ao_logo_width, 0.98f, transform);
	ao_poly(dst, ao_logo_top, ARRAYSIZE(ao_logo_top), transform, fill, rop);
	ao_poly(dst, ao_logo_bottom, ARRAYSIZE(ao_logo_bottom), transform, fill, rop);
	ao_text(dst, font, name_x, name_y1, "Altus", fill, rop);
	ao_text(dst, font, name_x, name_y2, "Metrum", fill, rop);
}
