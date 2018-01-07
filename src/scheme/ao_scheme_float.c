/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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
#include <math.h>

#ifdef AO_SCHEME_FEATURE_FLOAT

static void float_mark(void *addr)
{
	(void) addr;
}

static int float_size(void *addr)
{
	if (!addr)
		return 0;
	return sizeof (struct ao_scheme_float);
}

static void float_move(void *addr)
{
	(void) addr;
}

const struct ao_scheme_type ao_scheme_float_type = {
	.mark = float_mark,
	.size = float_size,
	.move = float_move,
	.name = "float",
};

#ifndef FLOAT_FORMAT
#define FLOAT_FORMAT "%g"
#endif

void
ao_scheme_float_write(FILE *out, ao_poly p, bool write)
{
	struct ao_scheme_float *f = ao_scheme_poly_float(p);
	float	v = f->value;

	(void) write;
	if (isnanf(v))
		fputs("+nan.0", out);
	else if (isinff(v)) {
		if (v < 0)
			putc('-', out);
		else
			putc('+', out);
		fputs("inf.0", out);
	} else
		fprintf(out, FLOAT_FORMAT, v);
}

float
ao_scheme_poly_number(ao_poly p)
{
	switch (ao_scheme_poly_base_type(p)) {
	case AO_SCHEME_INT:
		return ao_scheme_poly_int(p);
	case AO_SCHEME_BIGINT:
		return ao_scheme_poly_bigint(p)->value;
	case AO_SCHEME_OTHER:
		switch (ao_scheme_other_type(ao_scheme_poly_other(p))) {
		case AO_SCHEME_FLOAT:
			return ao_scheme_poly_float(p)->value;
		}
	}
	return NAN;
}

ao_poly
ao_scheme_float_get(float value)
{
	struct ao_scheme_float	*f;

	f = ao_scheme_alloc(sizeof (struct ao_scheme_float));
	f->type = AO_SCHEME_FLOAT;
	f->value = value;
	return ao_scheme_float_poly(f);
}

ao_poly
ao_scheme_do_inexactp(struct ao_scheme_cons *cons)
{
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_inexact3f, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (ao_scheme_poly_type(val) == AO_SCHEME_FLOAT)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_finitep(struct ao_scheme_cons *cons)
{
	ao_poly	val;
	float	f;

	if (!ao_scheme_parse_args(_ao_scheme_atom_inexact3f, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(val)) {
	case AO_SCHEME_INT:
	case AO_SCHEME_BIGINT:
		return _ao_scheme_bool_true;
	case AO_SCHEME_FLOAT:
		f = ao_scheme_poly_float(val)->value;
		if (!isnan(f) && !isinf(f))
			return _ao_scheme_bool_true;
	}
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_infinitep(struct ao_scheme_cons *cons)
{
	ao_poly	val;
	float	f;

	if (!ao_scheme_parse_args(_ao_scheme_atom_inexact3f, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(val)) {
	case AO_SCHEME_FLOAT:
		f = ao_scheme_poly_float(val)->value;
		if (isinf(f))
			return _ao_scheme_bool_true;
	}
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_sqrt(struct ao_scheme_cons *cons)
{
	float	f;

	if (!ao_scheme_parse_args(_ao_scheme_atom_sqrt, cons,
				  AO_SCHEME_FLOAT, &f,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_float_get(sqrtf(f));
}
#endif
