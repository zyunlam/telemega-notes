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

#include "ao_lisp.h"
#include <math.h>

static void float_mark(void *addr)
{
	(void) addr;
}

static int float_size(void *addr)
{
	if (!addr)
		return 0;
	return sizeof (struct ao_lisp_float);
}

static void float_move(void *addr)
{
	(void) addr;
}

const struct ao_lisp_type ao_lisp_float_type = {
	.mark = float_mark,
	.size = float_size,
	.move = float_move,
	.name = "float",
};

void
ao_lisp_float_write(ao_poly p)
{
	struct ao_lisp_float *f = ao_lisp_poly_float(p);
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
ao_lisp_poly_number(ao_poly p)
{
	switch (ao_lisp_poly_base_type(p)) {
	case AO_LISP_INT:
		return ao_lisp_poly_int(p);
	case AO_LISP_OTHER:
		switch (ao_lisp_other_type(ao_lisp_poly_other(p))) {
		case AO_LISP_BIGINT:
			return ao_lisp_bigint_int(ao_lisp_poly_bigint(p)->value);
		case AO_LISP_FLOAT:
			return ao_lisp_poly_float(p)->value;
		}
	}
	return NAN;
}

ao_poly
ao_lisp_float_get(float value)
{
	struct ao_lisp_float	*f;

	f = ao_lisp_alloc(sizeof (struct ao_lisp_float));
	f->type = AO_LISP_FLOAT;
	f->value = value;
	return ao_lisp_float_poly(f);
}

ao_poly
ao_lisp_do_inexactp(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (ao_lisp_poly_type(ao_lisp_arg(cons, 0)) == AO_LISP_FLOAT)
		return _ao_lisp_bool_true;
	return _ao_lisp_bool_false;
}

ao_poly
ao_lisp_do_finitep(struct ao_lisp_cons *cons)
{
	ao_poly	value;
	float	f;

	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	value = ao_lisp_arg(cons, 0);
	switch (ao_lisp_poly_type(value)) {
	case AO_LISP_INT:
	case AO_LISP_BIGINT:
		return _ao_lisp_bool_true;
	case AO_LISP_FLOAT:
		f = ao_lisp_poly_float(value)->value;
		if (!isnan(f) && !isinf(f))
			return _ao_lisp_bool_true;
	}
	return _ao_lisp_bool_false;
}

ao_poly
ao_lisp_do_infinitep(struct ao_lisp_cons *cons)
{
	ao_poly	value;
	float	f;

	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	value = ao_lisp_arg(cons, 0);
	switch (ao_lisp_poly_type(value)) {
	case AO_LISP_FLOAT:
		f = ao_lisp_poly_float(value)->value;
		if (isinf(f))
			return _ao_lisp_bool_true;
	}
	return _ao_lisp_bool_false;
}

ao_poly
ao_lisp_do_sqrt(struct ao_lisp_cons *cons)
{
	ao_poly	value;

	if (!ao_lisp_check_argc(_ao_lisp_atom_sqrt, cons, 1, 1))
		return AO_LISP_NIL;
	value = ao_lisp_arg(cons, 0);
	if (!ao_lisp_number_typep(ao_lisp_poly_type(value)))
		return ao_lisp_error(AO_LISP_INVALID, "%s: non-numeric", ao_lisp_poly_atom(_ao_lisp_atom_sqrt)->name);
	return ao_lisp_float_get(sqrtf(ao_lisp_poly_number(value)));
}
