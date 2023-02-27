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

#define TIMEOUT	50

#define PASS_KEYS
#include "test-frame.c"

#define BIG_FONT BitstreamVeraSans_Roman_58_font
#define VOLT_FONT BitstreamVeraSans_Roman_58_font
#define SMALL_FONT BitstreamVeraSans_Roman_12_font
#define TINY_FONT BitstreamVeraSans_Roman_12_font
#define LOGO_FONT BenguiatGothicStd_Bold_26_font

#define LABEL_Y		(int16_t) (SMALL_FONT.ascent)
#define VALUE_Y		(int16_t) (LABEL_Y + BIG_FONT.ascent + 3)
#define BOX_X		2
#define PAD_X		90
#define BOX_LABEL_X	30
#define VOLT_LABEL_X	25
#define RSSI_LABEL_X	15
#define PAD_LABEL_X	95
#define SEP_X		(PAD_X - 10)
#define SCAN_X		(WIDTH - 100) / 2
#define SCAN_Y		49
#define SCAN_HEIGHT	4
#define FOUND_Y		63
#define FOUND_WIDTH	21
#define MAX_VALID	(WIDTH / FOUND_WIDTH)

static int	box_number = 1;
static int	pad_number = 1;
static int	do_polys = 1;
static int	scan_number = 0;

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

static const struct ao_coord bowtie[] = {
	{ .x = 0, .y = 0 },
	{ .x = 32, .y = 32 },
	{ .x = 0, .y = 32 },
	{ .x = 32, .y = 0 },
};

#define NCOORD_BOWTIE (sizeof(bowtie)/sizeof(bowtie[0]))

static const struct ao_transform logo_transform = {
	.x_scale = 48, .x_off = 0,
	.y_scale = 48, .y_off = 0,
};

static const struct ao_transform bowtie_transform = {
	.x_scale = 1, .x_off = 50,
	.y_scale = 1, .y_off = 20,
};

static const float pad_volts = 12.3f;
static const float lco_volts = 4.1f;
static const int rssi = -30;

static int	boxes[] = { 1, 2, 3, 5, 8, 11, 13, 17, 19, 23, 29, 31, 37, 62, 97 };

static int	max_box = 97;

#define ARRAYSIZE(a)	(sizeof(a) / sizeof((a)[0]))

static bool
valid_box(int box)
{
	size_t i;
	if (box == 0)
		return true;
	for (i = 0; i < ARRAYSIZE(boxes); i++)
		if (boxes[i] == box)
			return true;
	return false;
}

static void
next_box(void)
{
	for (int n = box_number + 1; n <= max_box; n++)
		if (valid_box(n)) {
			box_number = n;
			return;
		}
	box_number = 0;
}

static void
prev_box(void)
{
	for (int n = box_number - 1; n >= 0; n--)
		if (valid_box(n)) {
			box_number = n;
			return;
		}
	box_number = max_box;
}

static bool redraw_all = true;

void HandleExpose(Display *dpy, Window win, GC gc)
{
	char	str[64];
	int 	i;
	int	v;
	int	last_box;
	int16_t	b;

	if (redraw_all)
		ao_rect(&fb, 0, 0, WIDTH, HEIGHT, 0xffffffff, AO_COPY);

	if (do_polys == 1)
		current_timeout = TIMEOUT;
	else
		current_timeout = 0;
	switch (do_polys) {
	case 1:
		if (redraw_all)
			ao_logo(&fb, &logo_transform, &LOGO_FONT, 0x00000000, AO_COPY);
		else
			ao_rect(&fb, 0, SCAN_Y, WIDTH, HEIGHT-SCAN_Y, 0xffffffff, AO_COPY);
		if (scan_number) {
			ao_rect(&fb, SCAN_X, SCAN_Y, (int16_t) scan_number, SCAN_HEIGHT, 0x00000000, AO_COPY);
			b = 0;
			v = 0;
			last_box = 0;
			for (i = scan_number; i > 1; i--) {
				if (valid_box(i)) {
					if (!last_box)
						last_box = i;
					v++;
					if (v == MAX_VALID)
						break;
				}
			}
			for (; i <= scan_number; i++) {
				if (valid_box(i)) {
					sprintf(str, "%02d%s", i, i == last_box ? "" : ",");
					ao_text(&fb, &TINY_FONT, 0 + FOUND_WIDTH * b, FOUND_Y, str, 0x00000000, AO_COPY);
					b++;
				}
			}
			redraw_all = false;
		}
		break;
	case 2:
		ao_poly(&fb, trek, NCOORD_TREK, NULL, 0x00000000, AO_COPY);
		ao_poly(&fb, donut, NCOORD_DONUT, NULL, 0x00000000, AO_COPY);
		break;
	case 3:
		ao_poly(&fb, bowtie, NCOORD_BOWTIE, &bowtie_transform, 0x00000000, AO_COPY);
		break;
	default:
	case 0:
		switch (box_number) {
		case 0:
			sprintf(str, "%4.1f", lco_volts);
			ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, 0x00000000, AO_COPY);
			ao_text(&fb, &SMALL_FONT, VOLT_LABEL_X, LABEL_Y, "LCO Battery", 0x00000000, AO_COPY);
			break;
		default:
			switch (pad_number) {
			case -1:
				sprintf(str, "%4.1f", pad_volts);
				ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, 0x00000000, AO_COPY);
				ao_text(&fb, &SMALL_FONT, VOLT_LABEL_X, LABEL_Y, "Pad Battery", 0x00000000, AO_COPY);
				break;
			case 0:
				sprintf(str, "%4d", rssi);
				ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, 0x00000000, AO_COPY);
				ao_text(&fb, &SMALL_FONT, RSSI_LABEL_X, LABEL_Y, "Signal Strength", 0x00000000, AO_COPY);
				break;
			default:
				sprintf(str, "%02d", box_number);
				ao_text(&fb, &BIG_FONT, BOX_X, VALUE_Y, str, 0x00000000, AO_COPY);
				ao_text(&fb, &SMALL_FONT, BOX_LABEL_X, LABEL_Y, "Box", 0x00000000, AO_COPY);

				sprintf(str, "%d", pad_number);
				ao_text(&fb, &BIG_FONT, PAD_X, VALUE_Y, str, 0x00000000, AO_COPY);
				ao_text(&fb, &SMALL_FONT, PAD_LABEL_X, LABEL_Y, "Pad", 0x00000000, AO_COPY);

				ao_rect(&fb, SEP_X, 0, 2, HEIGHT, 0x00000000, AO_COPY);
			}
			break;
		}
		break;
	}

	DoDisplay(dpy, win, gc);
}

void
HandleKeyPress(Display *dpy, Window win, GC gc, XEvent *ev)
{
	char	string[10];

	redraw_all = true;
	if (XLookupString ((XKeyEvent *) ev, string, sizeof (string), 0, 0) >= 1) {
		switch (string[0]) {
		case 'q':
			exit (0);
		case 'p':
			if (box_number != 0) {
				pad_number++;
				if (pad_number > 8)
					pad_number = -1;
			}
			break;
		case 'P':
			if (box_number != 0) {
				pad_number--;
				if (pad_number < -1)
					pad_number = 8;
			}
			break;
		case 'b':
			next_box();
			break;
		case 'B':
			prev_box();
			break;
		case 'i':
			do_polys++;
			if (do_polys == 4)
				do_polys = 0;
			if (do_polys == 1)
				scan_number = 0;
			break;
		case 'I':
			do_polys--;
			if (do_polys < 0)
				do_polys = 4;
			if (do_polys == 1)
				scan_number = 0;
			break;
		case 's':
			if (scan_number < 99)
				scan_number++;
			break;
		case 'S':
			if (scan_number > 0)
				scan_number--;
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
	(void) dpy;
	(void) win;
	(void) gc;
	(void) ev;
}

void
HandleTimeout(Display *dpy, Window win, GC gc)
{
	if (do_polys == 1) {
		if (scan_number < 99)
			scan_number++;
		else {
			redraw_all = true;
			box_number = boxes[0];
			pad_number = 1;
			do_polys = 0;
		}
		HandleExpose(dpy, win, gc);
	}
}
