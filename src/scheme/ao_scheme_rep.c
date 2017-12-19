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

#include "ao_scheme.h"

ao_poly
ao_scheme_read_eval_print(void)
{
	ao_poly	in, out = AO_SCHEME_NIL;

	ao_scheme_exception = 0;
	for(;;) {
		in = ao_scheme_read();
		if (in == _ao_scheme_atom_eof)
			break;
		out = ao_scheme_eval(in);
		if (ao_scheme_exception) {
			if (ao_scheme_exception & AO_SCHEME_EXIT)
				break;
			ao_scheme_exception = 0;
		} else {
			ao_scheme_poly_write(out, true);
			putchar ('\n');
		}
	}
	return out;
}
