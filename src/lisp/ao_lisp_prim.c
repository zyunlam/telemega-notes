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

ao_lisp_poly
ao_lisp_poly_print(ao_lisp_poly p)
{
	switch (ao_lisp_poly_type(p)) {
	case AO_LISP_CONS:
		ao_lisp_cons_print(ao_lisp_poly_cons(p));
		break;
	case AO_LISP_STRING:
		ao_lisp_string_print(ao_lisp_poly_string(p));
		break;
	case AO_LISP_INT:
		ao_lisp_int_print(ao_lisp_poly_int(p));
		break;
	case AO_LISP_ATOM:
		ao_lisp_atom_print(ao_lisp_poly_atom(p));
		break;
	case AO_LISP_BUILTIN:
		ao_lisp_builtin_print(ao_lisp_poly_builtin(p));
		break;
	}
	return AO_LISP_NIL;
}

void
ao_lisp_poly_mark(ao_lisp_poly p)
{
	switch (ao_lisp_poly_type(p)) {
	case AO_LISP_CONS:
		ao_lisp_mark(&ao_lisp_cons_type, ao_lisp_poly_cons(p));
		break;
	case AO_LISP_STRING:
		ao_lisp_mark(&ao_lisp_string_type, ao_lisp_poly_string(p));
		break;
	case AO_LISP_ATOM:
		ao_lisp_mark(&ao_lisp_atom_type, ao_lisp_poly_atom(p));
		break;
	}
}

ao_lisp_poly
ao_lisp_poly_move(ao_lisp_poly p)
{
	switch (ao_lisp_poly_type(p)) {
	case AO_LISP_CONS:
		p = ao_lisp_cons_poly(ao_lisp_move(&ao_lisp_cons_type, ao_lisp_poly_cons(p)));
		break;
	case AO_LISP_STRING:
		p = ao_lisp_string_poly(ao_lisp_move(&ao_lisp_string_type, ao_lisp_poly_string(p)));
		break;
	case AO_LISP_ATOM:
		p = ao_lisp_atom_poly(ao_lisp_move(&ao_lisp_atom_type, ao_lisp_poly_atom(p)));
		break;
	}
	return p;
}
