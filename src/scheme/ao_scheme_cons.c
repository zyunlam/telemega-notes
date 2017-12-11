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

static void cons_mark(void *addr)
{
	struct ao_scheme_cons	*cons = addr;

	for (;;) {
		ao_poly cdr = cons->cdr;

		ao_scheme_poly_mark(cons->car, 1);
		if (!cdr)
			break;
		if (ao_scheme_poly_type(cdr) != AO_SCHEME_CONS) {
			ao_scheme_poly_mark(cdr, 1);
			break;
		}
		cons = ao_scheme_poly_cons(cdr);
		if (ao_scheme_mark_memory(&ao_scheme_cons_type, cons))
			break;
	}
}

static int cons_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_scheme_cons);
}

static void cons_move(void *addr)
{
	struct ao_scheme_cons	*cons = addr;

	if (!cons)
		return;

	for (;;) {
		ao_poly			cdr;
		struct ao_scheme_cons	*c;
		int	ret;

		MDBG_MOVE("cons_move start %d (%d, %d)\n",
			  MDBG_OFFSET(cons), MDBG_OFFSET(ao_scheme_ref(cons->car)), MDBG_OFFSET(ao_scheme_ref(cons->cdr)));
		(void) ao_scheme_poly_move(&cons->car, 1);
		cdr = cons->cdr;
		if (!cdr)
			break;
		if (ao_scheme_poly_base_type(cdr) != AO_SCHEME_CONS) {
			(void) ao_scheme_poly_move(&cons->cdr, 0);
			break;
		}
		c = ao_scheme_poly_cons(cdr);
		ret = ao_scheme_move_memory(&ao_scheme_cons_type, (void **) &c);
		if (c != ao_scheme_poly_cons(cons->cdr))
			cons->cdr = ao_scheme_cons_poly(c);
		MDBG_MOVE("cons_move end %d (%d, %d)\n",
			  MDBG_OFFSET(cons), MDBG_OFFSET(ao_scheme_ref(cons->car)), MDBG_OFFSET(ao_scheme_ref(cons->cdr)));
		if (ret)
			break;
		cons = c;
	}
}

const struct ao_scheme_type ao_scheme_cons_type = {
	.mark = cons_mark,
	.size = cons_size,
	.move = cons_move,
	.name = "cons",
};

struct ao_scheme_cons *ao_scheme_cons_free_list;

struct ao_scheme_cons *
ao_scheme_cons_cons(ao_poly car, ao_poly cdr)
{
	struct ao_scheme_cons	*cons;

	if (ao_scheme_cons_free_list) {
		cons = ao_scheme_cons_free_list;
		ao_scheme_cons_free_list = ao_scheme_poly_cons(cons->cdr);
	} else {
		ao_scheme_poly_stash(0, car);
		ao_scheme_poly_stash(1, cdr);
		cons = ao_scheme_alloc(sizeof (struct ao_scheme_cons));
		cdr = ao_scheme_poly_fetch(1);
		car = ao_scheme_poly_fetch(0);
		if (!cons)
			return NULL;
	}
	cons->car = car;
	cons->cdr = cdr;
	return cons;
}

struct ao_scheme_cons *
ao_scheme_cons_cdr(struct ao_scheme_cons *cons)
{
	ao_poly	cdr = cons->cdr;
	if (cdr == AO_SCHEME_NIL)
		return NULL;
	if (ao_scheme_poly_type(cdr) != AO_SCHEME_CONS) {
		(void) ao_scheme_error(AO_SCHEME_INVALID, "improper list");
		return NULL;
	}
	return ao_scheme_poly_cons(cdr);
}

ao_poly
ao_scheme__cons(ao_poly car, ao_poly cdr)
{
	return ao_scheme_cons_poly(ao_scheme_cons_cons(car, cdr));
}

void
ao_scheme_cons_free(struct ao_scheme_cons *cons)
{
#if DBG_FREE_CONS
	ao_scheme_cons_check(cons);
#endif
	while (cons) {
		ao_poly cdr = cons->cdr;
		cons->cdr = ao_scheme_cons_poly(ao_scheme_cons_free_list);
		ao_scheme_cons_free_list = cons;
		cons = ao_scheme_poly_cons(cdr);
	}
}

void
ao_scheme_cons_write(ao_poly c)
{
	struct ao_scheme_cons	*cons = ao_scheme_poly_cons(c);
	ao_poly			cdr;
	int			first = 1;

	printf("(");
	while (cons) {
		if (!first)
			printf(" ");
		ao_scheme_poly_write(cons->car);
		cdr = cons->cdr;
		if (cdr == c) {
			printf(" ...");
			break;
		}
		if (ao_scheme_poly_type(cdr) == AO_SCHEME_CONS) {
			cons = ao_scheme_poly_cons(cdr);
			first = 0;
		} else {
			printf(" . ");
			ao_scheme_poly_write(cdr);
			cons = NULL;
		}
	}
	printf(")");
}

void
ao_scheme_cons_display(ao_poly c)
{
	struct ao_scheme_cons	*cons = ao_scheme_poly_cons(c);
	ao_poly			cdr;

	while (cons) {
		ao_scheme_poly_display(cons->car);
		cdr = cons->cdr;
		if (cdr == c) {
			printf("...");
			break;
		}
		if (ao_scheme_poly_type(cdr) == AO_SCHEME_CONS)
			cons = ao_scheme_poly_cons(cdr);
		else {
			ao_scheme_poly_display(cdr);
			cons = NULL;
		}
	}
}

int
ao_scheme_cons_length(struct ao_scheme_cons *cons)
{
	int	len = 0;
	while (cons) {
		len++;
		cons = ao_scheme_cons_cdr(cons);
	}
	return len;
}
