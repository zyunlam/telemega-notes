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

void
ao_scheme_int_write(FILE *out, ao_poly p, bool write)
{
	int i = ao_scheme_poly_int(p);
	(void) write;
	fprintf(out, "%d", i);
}

ao_poly
ao_scheme_do_integerp(struct ao_scheme_cons *cons)
{
#ifdef AO_SCHEME_FEATURE_BIGINT
	ao_poly val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_pair3f, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(val)) {
	case AO_SCHEME_INT:
	case AO_SCHEME_BIGINT:
		return _ao_scheme_bool_true;
	default:
		return _ao_scheme_bool_false;
	}
#else
	return ao_scheme_do_typep(_ao_scheme_atom_integer3f, AO_SCHEME_INT, cons);
#endif
}

ao_poly
ao_scheme_do_numberp(struct ao_scheme_cons *cons)
{
#if defined(AO_SCHEME_FEATURE_BIGINT) || defined(AO_SCHEME_FEATURE_FLOAT)
	ao_poly val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_pair3f, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(val)) {
	case AO_SCHEME_INT:
#ifdef AO_SCHEME_FEATURE_BIGINT
	case AO_SCHEME_BIGINT:
#endif
#ifdef AO_SCHEME_FEATURE_FLOAT
	case AO_SCHEME_FLOAT:
#endif
		return _ao_scheme_bool_true;
	default:
		return _ao_scheme_bool_false;
	}
#else
	return ao_scheme_do_integerp(cons);
#endif
}

#ifdef AO_SCHEME_FEATURE_BIGINT

int32_t
ao_scheme_poly_integer(ao_poly p)
{
	switch (ao_scheme_poly_base_type(p)) {
	case AO_SCHEME_INT:
		return ao_scheme_poly_int(p);
	case AO_SCHEME_BIGINT:
		return ao_scheme_poly_bigint(p)->value;
	}
	return 0;
}

ao_poly
ao_scheme_integer_poly(int32_t p)
{
	struct ao_scheme_bigint	*bi;

	if (AO_SCHEME_MIN_INT <= p && p <= AO_SCHEME_MAX_INT)
		return ao_scheme_int_poly(p);
	bi = ao_scheme_alloc(sizeof (struct ao_scheme_bigint));
	bi->value = p;
	return ao_scheme_bigint_poly(bi);
}

static void bigint_mark(void *addr)
{
	(void) addr;
}

static int bigint_size(void *addr)
{
	if (!addr)
		return 0;
	return sizeof (struct ao_scheme_bigint);
}

static void bigint_move(void *addr)
{
	(void) addr;
}

const struct ao_scheme_type ao_scheme_bigint_type = {
	.mark = bigint_mark,
	.size = bigint_size,
	.move = bigint_move,
	.name = "bigint",
};

void
ao_scheme_bigint_write(FILE *out, ao_poly p, bool write)
{
	struct ao_scheme_bigint	*bi = ao_scheme_poly_bigint(p);

	(void) write;
	fprintf(out, "%d", bi->value);
}
#endif /* AO_SCHEME_FEATURE_BIGINT */
