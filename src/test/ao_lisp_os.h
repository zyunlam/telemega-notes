/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
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

#ifndef _AO_LISP_OS_H_
#define _AO_LISP_OS_H_

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define AO_LISP_POOL_TOTAL	3072
#define AO_LISP_SAVE

extern int ao_lisp_getc(void);

static inline void
ao_lisp_os_flush() {
	fflush(stdout);
}

static inline void
ao_lisp_abort(void)
{
	abort();
}

static inline void
ao_lisp_os_led(int led)
{
	printf("leds set to 0x%x\n", led);
}

static inline void
ao_lisp_os_delay(int delay)
{
	struct timespec ts = {
		.tv_sec = delay / 1000,
		.tv_nsec = (delay % 1000) * 1000000,
	};
	nanosleep(&ts, NULL);
}
#endif
