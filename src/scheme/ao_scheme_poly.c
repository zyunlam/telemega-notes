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

static void ao_scheme_invalid_write(ao_poly p, bool write) {
	printf("??? type %d poly 0x%04x ???", ao_scheme_poly_type (p), p);
	(void) write;
	ao_scheme_abort();
}

static void (*const ao_scheme_write_funcs[AO_SCHEME_NUM_TYPE]) (ao_poly p, bool write) = {
	[AO_SCHEME_CONS] = ao_scheme_cons_write,
#ifdef AO_SCHEME_FEATURE_BIGINT
	[AO_SCHEME_BIGINT] = ao_scheme_bigint_write,
#endif
	[AO_SCHEME_INT] = ao_scheme_int_write,
	[AO_SCHEME_ATOM] = ao_scheme_atom_write,
	[AO_SCHEME_BUILTIN] = ao_scheme_builtin_write,
	[AO_SCHEME_FRAME] = ao_scheme_frame_write,
	[AO_SCHEME_FRAME_VALS] = ao_scheme_invalid_write,
	[AO_SCHEME_LAMBDA] = ao_scheme_lambda_write,
	[AO_SCHEME_STACK] = ao_scheme_stack_write,
	[AO_SCHEME_BOOL] = ao_scheme_bool_write,
	[AO_SCHEME_STRING] = ao_scheme_string_write,
#ifdef AO_SCHEME_FEATURE_FLOAT
	[AO_SCHEME_FLOAT] = ao_scheme_float_write,
#endif
#ifdef AO_SCHEME_FEATURE_VECTOR
	[AO_SCHEME_VECTOR] = ao_scheme_vector_write,
#endif
};

void (*ao_scheme_poly_write_func(ao_poly p))(ao_poly p, bool write)
{
	uint8_t	type = ao_scheme_poly_type(p);

	if (type < AO_SCHEME_NUM_TYPE)
		return ao_scheme_write_funcs[type];
	return ao_scheme_invalid_write;
}

void *
ao_scheme_ref(ao_poly poly) {
	if (poly == AO_SCHEME_NIL)
		return NULL;
	if (poly & AO_SCHEME_CONST)
		return (void *) (ao_scheme_const + (poly & AO_SCHEME_REF_MASK) - 4);
	return (void *) (ao_scheme_pool + (poly & AO_SCHEME_REF_MASK) - 4);
}

ao_poly
ao_scheme_poly(const void *addr, ao_poly type) {
	const uint8_t	*a = addr;
	if (a == NULL)
		return AO_SCHEME_NIL;
	if (ao_scheme_is_const_addr(a))
		return AO_SCHEME_CONST | (a - ao_scheme_const + 4) | type;
	return (a - ao_scheme_pool + 4) | type;
}
