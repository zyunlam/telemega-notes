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
#include "ao_pins.h"

static void
ao_pins_enable(void)
{
#if defined(TELEDONGLE_V_0_2)
	P0DIR = 0xff;
	P1DIR = 0x3f;
	P2DIR = 0x1f;
#elif defined(TELEDONGLE_V_0_1)
	P0DIR = 0xff;
	P1DIR = 0x3f;
	P2DIR = 0x1f;
#else
#error "ao_pins not configured for this device"
#endif
}

static void
ao_pins_set(void)
{
	uint8_t	reg, val;

	ao_pins_enable();
	for (;;) {
		ao_cmd_white();
		if (ao_cmd_lex_c == '\n')
			break;
		ao_cmd_hex();
		reg = ao_cmd_lex_i;
		ao_cmd_hex();
		val = ao_cmd_lex_i;
		if (ao_cmd_status != ao_cmd_success)
			break;
		switch (reg) {
		case 0:
			P0 = val;
			break;
		case 1:
			P1 = val;
			break;
		case 2:
			P2 = val;
			break;
		}
	}
}

__code struct ao_cmds ao_pins_cmds[] = {
	{ 'o',	ao_pins_set,	"o <0|1|2> <hex value> ...          Set GPIO output" },
	{ 0,    ao_pins_set,    0 },
};

void
ao_pins_init(void)
{
	ao_cmd_register(&ao_pins_cmds[0]);
}
