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

#define WIDTH	128
#define HEIGHT	64

#define DEFAULT_WIDTH	WIDTH
#define DEFAULT_HEIGHT	HEIGHT

#define PASS_KEYS
#include "frame.c"
#include "ao_draw.h"

#define STRIDE	((WIDTH + 31) / 32)

static uint32_t bits[STRIDE * HEIGHT];

static struct ao_bitmap fb = {
	.base = bits,
	.stride = STRIDE,
	.width = WIDTH,
	.height = HEIGHT
};

#define BIG_FONT FrutigerLT_Roman_64_font
#define SMALL_FONT FrutigerLT_Roman_12_font
#define LOGO_FONT FrutigerLT_Roman_24_font

#define VALUE_Y		BIG_FONT.ascent
#define LABEL_Y		BIG_FONT.ascent + SMALL_FONT.ascent + 2
#define BOX_X		2
#define PAD_X		90
#define BOX_LABEL_X	30
#define PAD_LABEL_X	95
#define SEP_X		(PAD_X - 8)

static int	box_number = 1;
static int	pad_number = 1;
static bool	do_polys = false;

static const struct ao_coord trek[] = {
	{ .x = 90, .y = 0 },
	{ .x = 60, .y = 40 },
	{ .x = 90, .y = 20 },
	{ .x = 120, .y = 40 },
};

#define NCOORD_TREK (sizeof(trek)/sizeof(trek[0]))

static const struct ao_coord donut[] = {
	{ .x = 30, .y = 0 },
	{ .x = 0, .y = 30 },
	{ .x = 30, .y = 60 },
	{ .x = 60, .y = 30 },
	{ .x = 30, .y = 0 },
	{ .x = 30, .y = 10 },
	{ .x = 50, .y = 30 },
	{ .x = 30, .y = 50 },
	{ .x = 10, .y = 30 },
	{ .x = 30, .y = 10 },
};

#define NCOORD_DONUT (sizeof(donut)/sizeof(donut[0]))

void HandleExpose(Display *dpy, Window win, GC gc)
{
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, 0xffffffff, AO_COPY);

	if (do_polys) {
		ao_logo(&fb, &LOGO_FONT, 0x00000000, AO_COPY);
//		ao_poly(&fb, trek, NCOORD_TREK, 0x00000000, AO_COPY);
//		ao_poly(&fb, donut, NCOORD_DONUT, 0x00000000, AO_COPY);
	} else {
		char	str[64];

		sprintf(str, "%02d", box_number);
		ao_text(&fb, &BIG_FONT, BOX_X, VALUE_Y, str, 0x00000000, AO_COPY);
		ao_text(&fb, &SMALL_FONT, BOX_LABEL_X, LABEL_Y, "box", 0x00000000, AO_COPY);

		sprintf(str, "%d", pad_number);
		ao_text(&fb, &BIG_FONT, PAD_X, VALUE_Y, str, 0x00000000, AO_COPY);
		ao_text(&fb, &SMALL_FONT, PAD_LABEL_X, LABEL_Y, "pad", 0x00000000, AO_COPY);

		ao_line(&fb, SEP_X, 0, SEP_X, HEIGHT, 0x00000000, AO_COPY);
	}

	XImage *image = XCreateImage(dpy, visual, 1, XYBitmap, 0, (char *) bits, WIDTH, HEIGHT, 32, STRIDE*4);
	XSetForeground(dpy, gc, WhitePixel(dpy, screen));
	XSetBackground(dpy, gc, BlackPixel(dpy, screen));
	XPutImage(dpy, win, gc, image, 0, 0, 0, 0, WIDTH, HEIGHT);
	free(image);
}

void
HandleKeyPress(Display *dpy, Window win, GC gc, XEvent *ev)
{
	char	string[10];
	if (XLookupString ((XKeyEvent *) ev, string, sizeof (string), 0, 0) >= 1) {
		switch (string[0]) {
		case 'q':
			exit (0);
		case 'p':
			pad_number++;
			if (pad_number > 8)
				pad_number = 1;
			break;
		case 'P':
			pad_number--;
			if (pad_number < 1)
				pad_number = 8;
			break;
		case 'b':
			box_number++;
			if (box_number > 99)
				box_number = 1;
			break;
		case 'B':
			box_number--;
			if (box_number < 1)
				box_number = 99;
			break;
		case 's':
			do_polys = !do_polys;
			break;
		case 'c':
			break;
		}
		HandleExpose(dpy, win, gc);
	}
}

void
HandleKeyRelease(Display *dpy, Window win, GC gc, XEvent *ev)
{
}
