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

#define TRACK_POINTER

#include "test-frame.c"

static int16_t	x1 = 32, my_y1 = 10, x2 = 32, my_y2 = 100;

void HandleExpose(Display *dpy, Window win, GC gc)
{
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, 0xffffffff, AO_COPY);

	ao_line(&fb, x1, my_y1, x2, my_y2, 0x00000000, AO_COPY);

	DoDisplay(dpy, win, gc);
	DisplayPositions(dpy, win, gc, current_positions);
}

void
Draw(Display *dpy, Window win, GC gc, PositionRec positions[5])
{
	x1 = (int16_t) positions[0].cur_x / IMAGE_SCALE;
	my_y1 = (int16_t) positions[0].cur_y / IMAGE_SCALE;
	x2 = (int16_t) positions[2].cur_x / IMAGE_SCALE;
	my_y2 = (int16_t) positions[2].cur_y / IMAGE_SCALE;
	HandleExpose(dpy, win, gc);
}

void
Undraw(Display *dpy, Window win, GC gc, PositionRec positions[5])
{
	(void) dpy;
	(void) win;
	(void) gc;
	(void) positions;
}
