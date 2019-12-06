/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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

#include <ao.h>

/*
 * The provided 'calibration' value is
 * that needed to tune the radio to precisely 434550kHz.
 * The relation between value and freq is linear, so
 * to get the value for an arbitrary frequency:
 *
 *	target_value   target_freq
 *	------------ = ------------
 *	 cal_value       cal_freq
 *
 *                     cal_value * target_freq
 *	target_value = -----------------------
 *                             cal_freq
 */

int32_t ao_freq_to_set(int32_t target_freq, int32_t cal_value)
{
	int64_t	prod = (int64_t) target_freq * (int64_t) cal_value;

	/* Round to nearest */
	int32_t target_value = (prod + (434550 / 2)) / 434550;

	return target_value;
}
