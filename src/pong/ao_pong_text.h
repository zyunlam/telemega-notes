/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_PONG_TEXT_
#define _AO_PONG_TEXT_

void
ao_pong_text(const struct ao_bitmap	*dst,
	     int16_t			x,
	     int16_t			y,
	     char			*string);

extern const struct ao_font ao_pong_font;

#endif /* _AO_PONG_TEXT_H_ */
