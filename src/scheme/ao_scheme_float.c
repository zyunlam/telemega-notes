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

void
ao_scheme_float_write(ao_poly p)
{
	struct ao_scheme_float *f = ao_scheme_poly_float(p);
	float	v = f->value;

	if (isnanf(v))
		printf("+nan.0");
	else if (isinff(v)) {
		if (v < 0)
			printf("-");
		else
			printf("+");
		printf("inf.0");
	} else
		printf ("%g", f->value);
}

float
ao_scheme_poly_number(ao_poly p)
{
	switch (ao_scheme_poly_base_type(p)) {
	case AO_SCHEME_INT:
		return ao_scheme_poly_int(p);
	case AO_SCHEME_OTHER:
		switch (ao_scheme_other_type(ao_scheme_poly_other(p))) {
		case AO_SCHEME_BIGINT:
			return ao_scheme_bigint_int(ao_scheme_poly_bigint(p)->value);
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
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (ao_scheme_poly_type(ao_scheme_arg(cons, 0)) == AO_SCHEME_FLOAT)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_finitep(struct ao_scheme_cons *cons)
{
	ao_poly	value;
	float	f;

	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	value = ao_scheme_arg(cons, 0);
	switch (ao_scheme_poly_type(value)) {
	case AO_SCHEME_INT:
	case AO_SCHEME_BIGINT:
		return _ao_scheme_bool_true;
	case AO_SCHEME_FLOAT:
		f = ao_scheme_poly_float(value)->value;
		if (!isnan(f) && !isinf(f))
			return _ao_scheme_bool_true;
	}
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_infinitep(struct ao_scheme_cons *cons)
{
	ao_poly	value;
	float	f;

	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	value = ao_scheme_arg(cons, 0);
	switch (ao_scheme_poly_type(value)) {
	case AO_SCHEME_FLOAT:
		f = ao_scheme_poly_float(value)->value;
		if (isinf(f))
			return _ao_scheme_bool_true;
	}
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_sqrt(struct ao_scheme_cons *cons)
{
	ao_poly	value;

	if (!ao_scheme_check_argc(_ao_scheme_atom_sqrt, cons, 1, 1))
		return AO_SCHEME_NIL;
	value = ao_scheme_arg(cons, 0);
	if (!ao_scheme_number_typep(ao_scheme_poly_type(value)))
		return ao_scheme_error(AO_SCHEME_INVALID, "%s: non-numeric", ao_scheme_poly_atom(_ao_scheme_atom_sqrt)->name);
	return ao_scheme_float_get(sqrtf(ao_scheme_poly_number(value)));
}
