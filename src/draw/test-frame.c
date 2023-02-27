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

#define IMAGE_SCALE	8
#define WIDTH	128
#define HEIGHT	64

#define IMAGE_WIDTH	(WIDTH * IMAGE_SCALE)
#define IMAGE_HEIGHT	(HEIGHT * IMAGE_SCALE)
#define IMAGE_STRIDE	((IMAGE_WIDTH + 31) / 32)

#define DEFAULT_WIDTH	IMAGE_WIDTH
#define DEFAULT_HEIGHT	IMAGE_HEIGHT

#define PASS_SETUP

#include "frame.c"
#include <ao_draw.h>

#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

#define STRIDE	((WIDTH + 31) / 32)

static uint32_t bits[STRIDE * HEIGHT];

static struct ao_bitmap fb = {
	.base = bits,
	.stride = STRIDE,
	.width = WIDTH,
	.height = HEIGHT,
	.damage = AO_BOX_INIT,
};

static XImage *shm_image;
static XImage *nonshm_image;
static XShmSegmentInfo shm_info;

void
Setup(Display *dpy, Window win)
{
	(void) win;
	if (XShmQueryExtension(dpy)) {
		shm_image = XShmCreateImage(dpy, visual, (unsigned) depth, ZPixmap, NULL, &shm_info, IMAGE_WIDTH, IMAGE_HEIGHT);
		shm_info.shmid = shmget(IPC_PRIVATE, (size_t) (shm_image->bytes_per_line * shm_image->height), IPC_CREAT|0777);
		shm_info.shmaddr = shm_image->data = shmat(shm_info.shmid, 0, 0);
		shm_info.readOnly = True;
		XShmAttach(dpy, &shm_info);
	} else {
		nonshm_image = XCreateImage(dpy, visual, (unsigned) depth, ZPixmap, 0, NULL,
				     IMAGE_WIDTH, IMAGE_HEIGHT, 32, IMAGE_STRIDE * 4);
		nonshm_image->data = calloc((size_t) nonshm_image->bytes_per_line, (size_t) nonshm_image->height);
	}
}

static void
DoDisplay(Display *dpy, Window win, GC gc)
{
	int 		ix, iy, ib;
	int 		dx, dy;
	XImage 		*image;
	uint32_t	*w, *scan, d;
	unsigned long	white = WhitePixel(dpy, screen);
	unsigned long	black = BlackPixel(dpy, screen);

	if (shm_image)
		image = shm_image;
	else
		image = nonshm_image;

	scan = bits + STRIDE * fb.damage.y1;
	for (iy = fb.damage.y1; iy < fb.damage.y2; iy++) {
		w = scan;
		scan += STRIDE;
		for (ix = fb.damage.x1 & ~31; ix < fb.damage.x2; ix += 32) {
			d = *w++;
			for (ib = 0; ib < 32 && ix + ib < WIDTH; ib++) {
				unsigned long p = d & 1 ? white : black;
				d >>= 1;
				for (dy = 0; dy < IMAGE_SCALE; dy++) {

					for (dx = 0; dx < IMAGE_SCALE; dx++) {
						XPutPixel(image, (ix + ib) * IMAGE_SCALE + dx, iy * IMAGE_SCALE + dy, p);
					}
				}
			}
		}
	}
	if (shm_image)
		XShmPutImage(dpy, win, gc, image, 0, 0, 0, 0, IMAGE_WIDTH, IMAGE_HEIGHT, False);
	else
		XPutImage(dpy, win, gc, image, 0, 0, 0, 0, IMAGE_WIDTH, IMAGE_HEIGHT);
	ao_damage_set_empty(&fb);
}
