/*
 * Copyright © 2010 Keith Packard <keithp@keithp.com>
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

#include "ao.h"

#ifdef AVR
__code struct ao_romconfig ao_romconfig __attribute__ ((section("romconfig")))
#else
__code __at (0x00a0) struct ao_romconfig ao_romconfig
#endif
= {
	.version = AO_ROMCONFIG_VERSION,
	.check = ~AO_ROMCONFIG_VERSION,
	.serial_number = 0,
	/*
	 * For 434.550MHz, the frequency value is:
	 *
	 * 434.550e6 / (24e6 / 2**16) = 1186611.2
	 *
	 * This value is stored in a const variable so that
	 * ao-load can change it during programming for
	 * devices that have no eeprom for config data.
	 */
	.radio_cal = 1186611
};
