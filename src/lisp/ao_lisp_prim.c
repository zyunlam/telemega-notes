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

static void (*const ao_lisp_print_funcs[AO_LISP_NUM_TYPE])(ao_poly) = {
	[AO_LISP_CONS] = ao_lisp_cons_print,
	[AO_LISP_STRING] = ao_lisp_string_print,
	[AO_LISP_INT] = ao_lisp_int_print,
	[AO_LISP_ATOM] = ao_lisp_atom_print,
	[AO_LISP_BUILTIN] = ao_lisp_builtin_print
};

ao_poly
ao_lisp_poly_print(ao_poly p)
{
	void (*print)(ao_poly) = ao_lisp_print_funcs[ao_lisp_poly_type(p)];
	if (print)
		print(p);
	return p;
}

static const struct ao_lisp_type const *ao_lisp_types[AO_LISP_NUM_TYPE] = {
	[AO_LISP_CONS] = &ao_lisp_cons_type,
	[AO_LISP_STRING] = &ao_lisp_string_type,
	[AO_LISP_ATOM] = &ao_lisp_atom_type,
	[AO_LISP_BUILTIN] = &ao_lisp_builtin_type,
};

void
ao_lisp_poly_mark(ao_poly p)
{
	const struct ao_lisp_type *lisp_type = ao_lisp_types[ao_lisp_poly_type(p)];
	if (lisp_type)
		ao_lisp_mark(lisp_type, ao_lisp_ref(p));
}

ao_poly
ao_lisp_poly_move(ao_poly p)
{
	uint8_t				type = p & AO_LISP_TYPE_MASK;
	const struct ao_lisp_type	*lisp_type;

	if (type == AO_LISP_OTHER)
		type = ao_lisp_other_type(ao_lisp_move_map(ao_lisp_poly_other(p)));

	lisp_type = ao_lisp_types[type];
	if (lisp_type)
		p = ao_lisp_poly(ao_lisp_move(lisp_type, ao_lisp_ref(p)), p & AO_LISP_TYPE_MASK);
	return p;
}
