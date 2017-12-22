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

static void bool_mark(void *addr)
{
	(void) addr;
}

static int bool_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_scheme_bool);
}

static void bool_move(void *addr)
{
	(void) addr;
}

const struct ao_scheme_type ao_scheme_bool_type = {
	.mark = bool_mark,
	.size = bool_size,
	.move = bool_move,
	.name = "bool"
};

void
ao_scheme_bool_write(ao_poly v, bool write)
{
	struct ao_scheme_bool	*b = ao_scheme_poly_bool(v);

	(void) write;
	if (b->value)
		printf("#t");
	else
		printf("#f");
}

#ifdef AO_SCHEME_MAKE_CONST

struct ao_scheme_bool	*ao_scheme_true, *ao_scheme_false;

struct ao_scheme_bool *
ao_scheme_bool_get(uint8_t value)
{
	struct ao_scheme_bool	**b;

	if (value)
		b = &ao_scheme_true;
	else
		b = &ao_scheme_false;

	if (!*b) {
		*b = ao_scheme_alloc(sizeof (struct ao_scheme_bool));
		(*b)->type = AO_SCHEME_BOOL;
		(*b)->value = value;
	}
	return *b;
}

#endif
