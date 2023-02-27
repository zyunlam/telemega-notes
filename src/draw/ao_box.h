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

#ifndef _AO_BOX_H_
#define _AO_BOX_H_

#include <stdint.h>
#include <stdbool.h>

struct ao_box {
	int16_t		x1, y1;
	int16_t		x2, y2;
};

static inline bool
ao_box_is_empty(struct ao_box *box)
{
	return box->x1 == INT16_MAX;
}

static inline void
ao_box_set_empty(struct ao_box *box)
{
	box->x1 = INT16_MAX;
	box->y1 = INT16_MAX;
	box->x2 = INT16_MIN;
	box->y2 = INT16_MIN;
}

#define AO_BOX_INIT	{ .x1 = INT16_MAX, .y1 = INT16_MAX, .x2 = INT16_MIN, .y2 = INT16_MIN }

void
ao_box_union(struct ao_box *dst, int16_t x1, int16_t y1, int16_t x2, int16_t y2);

#endif /* _AO_BOX_H_ */
