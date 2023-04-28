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

#include <ao_box.h>

void
ao_box_union(struct ao_box *dst, int16_t x1, int16_t y1, int16_t x2, int16_t y2)
{
	if (x1 < dst->x1) dst->x1 = x1;
	if (x2 > dst->x2) dst->x2 = x2;
	if (y1 < dst->y1) dst->y1 = y1;
	if (y2 > dst->y2) dst->y2 = y2;
}
