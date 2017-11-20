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

static void cons_mark(void *addr)
{
	struct ao_lisp_cons	*cons = addr;

	for (;;) {
		ao_poly cdr = cons->cdr;

		ao_lisp_poly_mark(cons->car, 1);
		if (!cdr)
			break;
		if (ao_lisp_poly_type(cdr) != AO_LISP_CONS) {
			ao_lisp_poly_mark(cdr, 1);
			break;
		}
		cons = ao_lisp_poly_cons(cdr);
		if (ao_lisp_mark_memory(&ao_lisp_cons_type, cons))
			break;
	}
}

static int cons_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_lisp_cons);
}

static void cons_move(void *addr)
{
	struct ao_lisp_cons	*cons = addr;

	if (!cons)
		return;

	for (;;) {
		ao_poly			cdr;
		struct ao_lisp_cons	*c;
		int	ret;

		MDBG_MOVE("cons_move start %d (%d, %d)\n",
			  MDBG_OFFSET(cons), MDBG_OFFSET(ao_lisp_ref(cons->car)), MDBG_OFFSET(ao_lisp_ref(cons->cdr)));
		(void) ao_lisp_poly_move(&cons->car, 1);
		cdr = cons->cdr;
		if (!cdr)
			break;
		if (ao_lisp_poly_type(cdr) != AO_LISP_CONS) {
			(void) ao_lisp_poly_move(&cons->cdr, 1);
			break;
		}
		c = ao_lisp_poly_cons(cdr);
		ret = ao_lisp_move_memory(&ao_lisp_cons_type, (void **) &c);
		if (c != ao_lisp_poly_cons(cons->cdr))
			cons->cdr = ao_lisp_cons_poly(c);
		MDBG_MOVE("cons_move end %d (%d, %d)\n",
			  MDBG_OFFSET(cons), MDBG_OFFSET(ao_lisp_ref(cons->car)), MDBG_OFFSET(ao_lisp_ref(cons->cdr)));
		if (ret)
			break;
		cons = c;
	}
}

const struct ao_lisp_type ao_lisp_cons_type = {
	.mark = cons_mark,
	.size = cons_size,
	.move = cons_move,
	.name = "cons",
};

struct ao_lisp_cons *ao_lisp_cons_free_list;

struct ao_lisp_cons *
ao_lisp_cons_cons(ao_poly car, ao_poly cdr)
{
	struct ao_lisp_cons	*cons;

	if (ao_lisp_cons_free_list) {
		cons = ao_lisp_cons_free_list;
		ao_lisp_cons_free_list = ao_lisp_poly_cons(cons->cdr);
	} else {
		ao_lisp_poly_stash(0, car);
		ao_lisp_poly_stash(1, cdr);
		cons = ao_lisp_alloc(sizeof (struct ao_lisp_cons));
		car = ao_lisp_poly_fetch(0);
		cdr = ao_lisp_poly_fetch(1);
		if (!cons)
			return NULL;
	}
	cons->car = car;
	cons->cdr = cdr;
	return cons;
}

struct ao_lisp_cons *
ao_lisp_cons_cdr(struct ao_lisp_cons *cons)
{
	ao_poly	cdr = cons->cdr;
	if (cdr == AO_LISP_NIL)
		return NULL;
	if (ao_lisp_poly_type(cdr) != AO_LISP_CONS) {
		(void) ao_lisp_error(AO_LISP_INVALID, "improper list");
		return NULL;
	}
	return ao_lisp_poly_cons(cdr);
}

ao_poly
ao_lisp__cons(ao_poly car, ao_poly cdr)
{
	return ao_lisp_cons_poly(ao_lisp_cons_cons(car, cdr));
}

void
ao_lisp_cons_free(struct ao_lisp_cons *cons)
{
	while (cons) {
		ao_poly cdr = cons->cdr;
		cons->cdr = ao_lisp_cons_poly(ao_lisp_cons_free_list);
		ao_lisp_cons_free_list = cons;
		cons = ao_lisp_poly_cons(cdr);
	}
}

void
ao_lisp_cons_write(ao_poly c)
{
	struct ao_lisp_cons *cons = ao_lisp_poly_cons(c);
	int	first = 1;
	printf("(");
	while (cons) {
		if (!first)
			printf(" ");
		ao_lisp_poly_write(cons->car);
		c = cons->cdr;
		if (ao_lisp_poly_type(c) == AO_LISP_CONS) {
			cons = ao_lisp_poly_cons(c);
			first = 0;
		} else {
			printf(" . ");
			ao_lisp_poly_write(c);
			cons = NULL;
		}
	}
	printf(")");
}

void
ao_lisp_cons_display(ao_poly c)
{
	struct ao_lisp_cons *cons = ao_lisp_poly_cons(c);

	while (cons) {
		ao_lisp_poly_display(cons->car);
		cons = ao_lisp_poly_cons(cons->cdr);
	}
}

int
ao_lisp_cons_length(struct ao_lisp_cons *cons)
{
	int	len = 0;
	while (cons) {
		len++;
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	return len;
}
