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

struct vl {
	struct ao_scheme_vector	*vector;
	struct vl *prev;
};

static struct vl *vl;
static unsigned int vd;

void
ao_scheme_vector_write(FILE *out, ao_poly v, bool write)
{
	struct ao_scheme_vector	*vector = ao_scheme_poly_vector(v);
	unsigned int i;
	int was_marked = 0;
	struct vl *ve;

	++vd;
	for (ve = vl; ve; ve = ve->prev)
		if (ve->vector == vector)
			abort();

	ve = malloc(sizeof (struct vl));
	ve->prev = vl;
	ve->vector = vector;
	vl = ve;

	ao_scheme_print_start();
	was_marked = ao_scheme_print_mark_addr(vector);
	if (was_marked) {
		fputs("...", out);
	} else {
		fputs("#(", out);
		for (i = 0; i < vector->length; i++) {
			if (i != 0)
				putc(' ', out);
			ao_scheme_poly_write(out, vector->vals[i], write);
		}
		printf(")");
	}
	if (ao_scheme_print_stop() && !was_marked)
		ao_scheme_print_clear_addr(vector);
	if (vl != ve)
		abort();
	vl = ve->prev;
	free(ve);
	--vd;
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

	ao_scheme_cons_stash(cons);
	vector = ao_scheme_vector_alloc(length, AO_SCHEME_NIL);
	cons = ao_scheme_cons_fetch();
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
ao_scheme_vector_to_list(struct ao_scheme_vector *vector, int start, int end)
{
	int			i;
	uint16_t		length = vector->length;
	struct ao_scheme_cons	*cons = NULL;

	if (end == -1)
		end = length;
	if (start < 0)
		start = 0;
	if (end > length)
		end = length;
	for (i = end; i-- > start;) {
		ao_scheme_vector_stash(vector);
		cons = ao_scheme_cons_cons(vector->vals[i], ao_scheme_cons_poly(cons));
		vector = ao_scheme_vector_fetch();
		if (!cons)
			return NULL;
	}
	return cons;
}

ao_poly
ao_scheme_do_vector(struct ao_scheme_cons *cons)
{
	return ao_scheme_vector_poly(ao_scheme_list_to_vector(cons));
}

ao_poly
ao_scheme_do_make_vector(struct ao_scheme_cons *cons)
{
	int32_t	len;
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_make2dvector, cons,
				  AO_SCHEME_INT, &len,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, _ao_scheme_bool_false, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_vector_poly(ao_scheme_vector_alloc(len, val));
}

static bool
ao_scheme_check_vector(ao_poly proc, struct ao_scheme_vector *vector, int32_t offset)
{
	if (offset < 0 || vector->length <= offset) {
		(void) ao_scheme_error(AO_SCHEME_INVALID, "%v: vector index %d out of range (max %d)",
				       proc,
				       offset, vector->length);
		return false;
	}
	return true;
}

ao_poly
ao_scheme_do_vector_ref(struct ao_scheme_cons *cons)
{
	struct ao_scheme_vector	*vector;
	int32_t			offset;

	if (!ao_scheme_parse_args(_ao_scheme_atom_vector2dref, cons,
				  AO_SCHEME_VECTOR, &vector,
				  AO_SCHEME_INT, &offset,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_vector(_ao_scheme_atom_vector2dref, vector, offset))
		return AO_SCHEME_NIL;
	return vector->vals[offset];
}

ao_poly
ao_scheme_do_vector_set(struct ao_scheme_cons *cons)
{
	struct ao_scheme_vector	*vector;
	int32_t			offset;
	ao_poly			val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_vector2dset21, cons,
				  AO_SCHEME_VECTOR, &vector,
				  AO_SCHEME_INT, &offset,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_vector(_ao_scheme_atom_vector2dset21, vector, offset))
		return AO_SCHEME_NIL;
	vector->vals[offset] = val;
	return val;
}

ao_poly
ao_scheme_do_list_to_vector(struct ao_scheme_cons *cons)
{
	struct ao_scheme_cons	*pair;

	if (!ao_scheme_parse_args(_ao_scheme_atom_list2d3evector, cons,
				  AO_SCHEME_CONS|AO_SCHEME_ARG_NIL_OK, &pair,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_vector_poly(ao_scheme_list_to_vector(pair));
}

ao_poly
ao_scheme_do_vector_to_list(struct ao_scheme_cons *cons)
{
	struct ao_scheme_vector	*vector;
	int32_t			start, end;

	if (!ao_scheme_parse_args(_ao_scheme_atom_vector2d3elist, cons,
				  AO_SCHEME_VECTOR, &vector,
				  AO_SCHEME_INT|AO_SCHEME_ARG_OPTIONAL, ao_scheme_int_poly(0), &start,
				  AO_SCHEME_INT|AO_SCHEME_ARG_OPTIONAL, ao_scheme_int_poly(-1), &end,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (end == -1)
		end = vector->length;
	return ao_scheme_cons_poly(ao_scheme_vector_to_list(vector, start, end));
}

ao_poly
ao_scheme_do_vector_length(struct ao_scheme_cons *cons)
{
	struct ao_scheme_vector	*vector;

	if (!ao_scheme_parse_args(_ao_scheme_atom_vector2d3elist, cons,
				  AO_SCHEME_VECTOR, &vector,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_integer_poly(vector->length);
}

ao_poly
ao_scheme_do_vectorp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(_ao_scheme_atom_vector3f, AO_SCHEME_VECTOR, cons);
}

#endif /* AO_SCHEME_FEATURE_VECTOR */
