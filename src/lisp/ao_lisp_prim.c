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

#if 0
#define DBG(...) printf (__VA_ARGS__)
#else
#define DBG(...)
#endif

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
	[AO_LISP_INT] = NULL,
	[AO_LISP_STRING] = &ao_lisp_string_type,
	[AO_LISP_OTHER] = (void *) 0x1,
	[AO_LISP_ATOM] = &ao_lisp_atom_type,
	[AO_LISP_BUILTIN] = &ao_lisp_builtin_type,
	[AO_LISP_FRAME] = &ao_lisp_frame_type,
};

int
ao_lisp_poly_mark(ao_poly p)
{
	const struct ao_lisp_type *lisp_type = ao_lisp_types[ao_lisp_poly_type(p)];
	if (lisp_type)
		return ao_lisp_mark(lisp_type, ao_lisp_ref(p));
	return 1;
}

int
ao_lisp_poly_move(ao_poly *ref)
{
	uint8_t				type;
	ao_poly				p = *ref;
	const struct ao_lisp_type	*lisp_type;
	int				ret;
	void				*addr;

	if (!p)
		return 1;

	type = p & AO_LISP_TYPE_MASK;
	if (type == AO_LISP_OTHER)
		type = ao_lisp_other_type(ao_lisp_move_map(ao_lisp_poly_other(p)));

	if (type >= AO_LISP_NUM_TYPE)
		abort();

	lisp_type = ao_lisp_types[type];
	if (!lisp_type)
		return 1;
	addr = ao_lisp_ref(p);
	ret = ao_lisp_move(lisp_type, &addr);
	if (addr != ao_lisp_ref(p)) {
		ao_poly np = ao_lisp_poly(addr, p & AO_LISP_TYPE_MASK);
		DBG("poly %d moved %04x -> %04x\n",
		    type, p, np);
		*ref = np;
	}
	return ret;
}
