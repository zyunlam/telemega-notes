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

#ifndef _AO_VGA_H_
#define _AO_VGA_H_

#include "ao_draw.h"

void
ao_vga_init(void);

void
ao_vga_enable(int active);

#define AO_VGA_WIDTH		320
#define AO_VGA_HEIGHT		240
#define AO_VGA_PAD		64
#define AO_VGA_STRIDE		((AO_VGA_WIDTH + AO_VGA_PAD) >> AO_SHIFT)

extern uint32_t	ao_vga_fb[AO_VGA_STRIDE * AO_VGA_HEIGHT];

#endif /* _AO_VGA_H_ */
