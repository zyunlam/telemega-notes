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

#ifndef _AO_SCHEME_OS_H_
#define _AO_SCHEME_OS_H_

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define AO_SCHEME_POOL_TOTAL	4096

static inline void
ao_scheme_abort(void)
{
	abort();
}

#define AO_SCHEME_JIFFIES_PER_SECOND	100

static inline void
ao_scheme_os_delay(int jiffies)
{
	struct timespec ts = {
		.tv_sec = jiffies / AO_SCHEME_JIFFIES_PER_SECOND,
		.tv_nsec = (jiffies % AO_SCHEME_JIFFIES_PER_SECOND) * (1000000000L / AO_SCHEME_JIFFIES_PER_SECOND)
	};
	nanosleep(&ts, NULL);
}

static inline int
ao_scheme_os_jiffy(void)
{
	struct timespec tp;
	clock_gettime(CLOCK_MONOTONIC, &tp);
	return tp.tv_sec * AO_SCHEME_JIFFIES_PER_SECOND + (tp.tv_nsec / (1000000000L / AO_SCHEME_JIFFIES_PER_SECOND));
}

#endif
