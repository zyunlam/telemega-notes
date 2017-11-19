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

void
ao_lisp_int_write(ao_poly p)
{
	int i = ao_lisp_poly_int(p);
	printf("%d", i);
}

int32_t
ao_lisp_poly_integer(ao_poly p)
{
	switch (ao_lisp_poly_base_type(p)) {
	case AO_LISP_INT:
		return ao_lisp_poly_int(p);
	case AO_LISP_OTHER:
		if (ao_lisp_other_type(ao_lisp_poly_other(p)) == AO_LISP_BIGINT)
			return ao_lisp_bigint_int(ao_lisp_poly_bigint(p)->value);
	}
	return AO_LISP_NOT_INTEGER;
}

ao_poly
ao_lisp_integer_poly(int32_t p)
{
	struct ao_lisp_bigint	*bi;

	if (AO_LISP_MIN_INT <= p && p <= AO_LISP_MAX_INT)
		return ao_lisp_int_poly(p);
	bi = ao_lisp_alloc(sizeof (struct ao_lisp_bigint));
	bi->value = ao_lisp_int_bigint(p);
	return ao_lisp_bigint_poly(bi);
}

static void bigint_mark(void *addr)
{
	(void) addr;
}

static int bigint_size(void *addr)
{
	if (!addr)
		return 0;
	return sizeof (struct ao_lisp_bigint);
}

static void bigint_move(void *addr)
{
	(void) addr;
}

const struct ao_lisp_type ao_lisp_bigint_type = {
	.mark = bigint_mark,
	.size = bigint_size,
	.move = bigint_move,
	.name = "bigint",
};

void
ao_lisp_bigint_write(ao_poly p)
{
	struct ao_lisp_bigint	*bi = ao_lisp_poly_bigint(p);

	printf("%d", ao_lisp_bigint_int(bi->value));
}
