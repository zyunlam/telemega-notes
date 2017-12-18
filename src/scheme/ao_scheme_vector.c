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

#ifdef AO_SCHEME_FEATURE_VECTOR

static void vector_mark(void *addr)
{
	struct ao_scheme_vector	*vector = addr;
	unsigned int	i;

	for (i = 0; i < vector->length; i++) {
		ao_poly v = vector->vals[i];

		ao_scheme_poly_mark(v, 1);
	}
}

static int vector_len_size(uint16_t length)
{
	return sizeof (struct ao_scheme_vector) + length * sizeof (ao_poly);
}

static int vector_size(void *addr)
{
	struct ao_scheme_vector *vector = addr;

	return vector_len_size(vector->length);
}

static void vector_move(void *addr)
{
	struct ao_scheme_vector	*vector = addr;
	unsigned int	i;

	for (i = 0; i < vector->length; i++)
		(void) ao_scheme_poly_move(&vector->vals[i], 1);
}

const struct ao_scheme_type ao_scheme_vector_type = {
	.mark = vector_mark,
	.size = vector_size,
	.move = vector_move,
	.name = "vector",
};

struct ao_scheme_vector *
ao_scheme_vector_alloc(uint16_t length, ao_poly fill)
{
	struct ao_scheme_vector	*vector;
	unsigned int i;

	vector = ao_scheme_alloc(vector_len_size(length));
	if (!vector)
		return NULL;
	vector->type = AO_SCHEME_VECTOR;
	vector->length = length;
	for (i = 0; i < length; i++)
		vector->vals[i] = fill;
	return vector;
}

void
ao_scheme_vector_write(ao_poly v)
{
	struct ao_scheme_vector	*vector = ao_scheme_poly_vector(v);
	unsigned int i;

	ao_scheme_print_start();
	if (ao_scheme_print_mark_addr(vector))
		printf ("...");
	else {
		printf("#(");
		for (i = 0; i < vector->length; i++) {
			if (i != 0)
				printf(" ");
			ao_scheme_poly_write(vector->vals[i]);
		}
		printf(")");
	}
	ao_scheme_print_stop();
}

void
ao_scheme_vector_display(ao_poly v)
{
	struct ao_scheme_vector *vector = ao_scheme_poly_vector(v);
	unsigned int i;

	ao_scheme_print_start();
	if (ao_scheme_print_mark_addr(vector))
		printf ("...");
	else {
		for (i = 0; i < vector->length; i++)
			ao_scheme_poly_display(vector->vals[i]);
	}
}

static int32_t
ao_scheme_vector_offset(struct ao_scheme_vector *vector, ao_poly i)
{
	bool	fail;
	int32_t	offset = ao_scheme_poly_integer(i, &fail);

	if (fail)
		ao_scheme_error(AO_SCHEME_INVALID, "vector index %v not integer", i);
	if (offset < 0 || vector->length <= offset) {
		ao_scheme_error(AO_SCHEME_INVALID, "vector index %v out of range (max %d)",
				i, vector->length);
		offset = -1;
	}
	return offset;
}

ao_poly
ao_scheme_vector_get(ao_poly v, ao_poly i)
{
	struct ao_scheme_vector	*vector = ao_scheme_poly_vector(v);
	int32_t			offset = ao_scheme_vector_offset(vector, i);

	if (offset < 0)
		return AO_SCHEME_NIL;
	return vector->vals[offset];
}

ao_poly
ao_scheme_vector_set(ao_poly v, ao_poly i, ao_poly p)
{
	struct ao_scheme_vector	*vector = ao_scheme_poly_vector(v);
	int32_t			offset = ao_scheme_vector_offset(vector, i);

	if (offset < 0)
		return AO_SCHEME_NIL;
	return vector->vals[offset] = p;
}

struct ao_scheme_vector *
ao_scheme_list_to_vector(struct ao_scheme_cons *cons)
{
	uint16_t		length;
	uint16_t		i;
	struct ao_scheme_vector	*vector;

	length = (uint16_t) ao_scheme_cons_length (cons);
	if (ao_scheme_exception)
		return NULL;

	ao_scheme_cons_stash(0, cons);
	vector = ao_scheme_vector_alloc(length, AO_SCHEME_NIL);
	cons = ao_scheme_cons_fetch(0);
	if (!vector)
		return NULL;
	i = 0;
	while (cons) {
		vector->vals[i++] = cons->car;
		cons = ao_scheme_cons_cdr(cons);
	}
	return vector;
}

struct ao_scheme_cons *
ao_scheme_vector_to_list(struct ao_scheme_vector *vector)
{
	unsigned int		i;
	uint16_t		length = vector->length;
	struct ao_scheme_cons	*cons = NULL;

	for (i = length; i-- > 0;) {
		ao_scheme_poly_stash(2, ao_scheme_vector_poly(vector));
		cons = ao_scheme_cons_cons(vector->vals[i], ao_scheme_cons_poly(cons));
		vector = ao_scheme_poly_vector(ao_scheme_poly_fetch(2));
		if (!cons)
			return NULL;
	}
	return cons;
}

#endif /* AO_SCHEME_FEATURE_VECTOR */
