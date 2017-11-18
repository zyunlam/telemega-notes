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

#include "ao_lisp.h"

ao_poly
ao_lisp_read_eval_print(void)
{
	ao_poly	in, out = AO_LISP_NIL;
	for(;;) {
		in = ao_lisp_read();
		if (in == _ao_lisp_atom_eof)
			break;
		out = ao_lisp_eval(in);
		if (ao_lisp_exception) {
			if (ao_lisp_exception & AO_LISP_EXIT)
				break;
			ao_lisp_exception = 0;
		} else {
			ao_lisp_poly_write(out);
			putchar ('\n');
		}
	}
	return out;
}
