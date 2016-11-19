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

struct ao_lisp_funcs {
	void (*print)(ao_poly);
	void (*patom)(ao_poly);
};

static const struct ao_lisp_funcs ao_lisp_funcs[AO_LISP_NUM_TYPE] = {
	[AO_LISP_CONS] = {
		.print = ao_lisp_cons_print,
		.patom = ao_lisp_cons_patom,
	},
	[AO_LISP_STRING] = {
		.print = ao_lisp_string_print,
		.patom = ao_lisp_string_patom,
	},
	[AO_LISP_INT] = {
		.print = ao_lisp_int_print,
		.patom = ao_lisp_int_print,
	},
	[AO_LISP_ATOM] = {
		.print = ao_lisp_atom_print,
		.patom = ao_lisp_atom_print,
	},
	[AO_LISP_BUILTIN] = {
		.print = ao_lisp_builtin_print,
		.patom = ao_lisp_builtin_print,
	},
	[AO_LISP_FRAME] = {
		.print = ao_lisp_frame_print,
		.patom = ao_lisp_frame_print,
	},
	[AO_LISP_LAMBDA] = {
		.print = ao_lisp_lambda_print,
		.patom = ao_lisp_lambda_print,
	},
	[AO_LISP_STACK] = {
		.print = ao_lisp_stack_print,
		.patom = ao_lisp_stack_print,
	},
};

static const struct ao_lisp_funcs *
funcs(ao_poly p)
{
	uint8_t	type = ao_lisp_poly_type(p);

	if (type < AO_LISP_NUM_TYPE)
		return &ao_lisp_funcs[type];
	return NULL;
}

void
ao_lisp_poly_print(ao_poly p)
{
	const struct ao_lisp_funcs *f = funcs(p);

	if (f && f->print)
		f->print(p);
}

void
ao_lisp_poly_patom(ao_poly p)
{
	const struct ao_lisp_funcs *f = funcs(p);

	if (f && f->patom)
		f->patom(p);
}

void *
ao_lisp_ref(ao_poly poly) {
	if (poly == AO_LISP_NIL)
		return NULL;
	if (poly & AO_LISP_CONST)
		return (void *) (ao_lisp_const + (poly & AO_LISP_REF_MASK) - 4);
	return (void *) (ao_lisp_pool + (poly & AO_LISP_REF_MASK) - 4);
}

ao_poly
ao_lisp_poly(const void *addr, ao_poly type) {
	const uint8_t	*a = addr;
	if (a == NULL)
		return AO_LISP_NIL;
	if (AO_LISP_IS_CONST(a))
		return AO_LISP_CONST | (a - ao_lisp_const + 4) | type;
	return (a - ao_lisp_pool + 4) | type;
}
