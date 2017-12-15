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

struct ao_scheme_funcs {
	void (*write)(ao_poly);
	void (*display)(ao_poly);
};

static const struct ao_scheme_funcs ao_scheme_funcs[AO_SCHEME_NUM_TYPE] = {
	[AO_SCHEME_CONS] = {
		.write = ao_scheme_cons_write,
		.display = ao_scheme_cons_display,
	},
#ifdef AO_SCHEME_FEATURE_BIGINT
	[AO_SCHEME_BIGINT] = {
		.write = ao_scheme_bigint_write,
		.display = ao_scheme_bigint_write,
	},
#endif
	[AO_SCHEME_INT] = {
		.write = ao_scheme_int_write,
		.display = ao_scheme_int_write,
	},
	[AO_SCHEME_ATOM] = {
		.write = ao_scheme_atom_write,
		.display = ao_scheme_atom_write,
	},
	[AO_SCHEME_BUILTIN] = {
		.write = ao_scheme_builtin_write,
		.display = ao_scheme_builtin_write,
	},
	[AO_SCHEME_FRAME] = {
		.write = ao_scheme_frame_write,
		.display = ao_scheme_frame_write,
	},
	[AO_SCHEME_FRAME_VALS] = {
		.write = NULL,
		.display = NULL,
	},
	[AO_SCHEME_LAMBDA] = {
		.write = ao_scheme_lambda_write,
		.display = ao_scheme_lambda_write,
	},
	[AO_SCHEME_STACK] = {
		.write = ao_scheme_stack_write,
		.display = ao_scheme_stack_write,
	},
	[AO_SCHEME_BOOL] = {
		.write = ao_scheme_bool_write,
		.display = ao_scheme_bool_write,
	},
	[AO_SCHEME_STRING] = {
		.write = ao_scheme_string_write,
		.display = ao_scheme_string_display,
	},
#ifdef AO_SCHEME_FEATURE_FLOAT
	[AO_SCHEME_FLOAT] = {
		.write = ao_scheme_float_write,
		.display = ao_scheme_float_write,
	},
#endif
#ifdef AO_SCHEME_FEATURE_VECTOR
	[AO_SCHEME_VECTOR] = {
		.write = ao_scheme_vector_write,
		.display = ao_scheme_vector_display
	},
#endif
};

static void ao_scheme_invalid_write(ao_poly p) {
	printf("??? 0x%04x ???", p);
}

static const struct ao_scheme_funcs ao_scheme_invalid_funcs = {
	.write = ao_scheme_invalid_write,
	.display = ao_scheme_invalid_write,
};

static const struct ao_scheme_funcs *
funcs(ao_poly p)
{
	uint8_t	type = ao_scheme_poly_type(p);

	if (type < AO_SCHEME_NUM_TYPE)
		return &ao_scheme_funcs[type];
	return &ao_scheme_invalid_funcs;
}

void (*ao_scheme_poly_write_func(ao_poly p))(ao_poly p)
{
	return funcs(p)->write;
}

void (*ao_scheme_poly_display_func(ao_poly p))(ao_poly p)
{
	return funcs(p)->display;
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
	if (AO_SCHEME_IS_CONST(a))
		return AO_SCHEME_CONST | (a - ao_scheme_const + 4) | type;
	return (a - ao_scheme_pool + 4) | type;
}
