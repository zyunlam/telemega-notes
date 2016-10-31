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
		ao_lisp_poly_mark(cons->car);
		cons = cons->cdr;
		if (!cons)
			break;
		if (ao_lisp_mark_memory(cons, sizeof (struct ao_lisp_cons)))
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

	for (;;) {
		struct ao_lisp_cons	*cdr;

		cons->car = ao_lisp_poly_move(cons->car);
		cdr = ao_lisp_move_memory(cons->cdr, sizeof (struct ao_lisp_cons));
		if (!cdr)
			break;
		cons->cdr = cdr;
		cons = cdr;
	}
}

const struct ao_lisp_mem_type ao_lisp_cons_type = {
	.mark = cons_mark,
	.size = cons_size,
	.move = cons_move,
};

struct ao_lisp_cons *
ao_lisp_cons(ao_lisp_poly car, struct ao_lisp_cons *cdr)
{
	struct ao_lisp_cons	*cons = ao_lisp_alloc(sizeof (struct ao_lisp_cons));
	if (!cons)
		return NULL;
	cons->car = car;
	cons->cdr = cdr;
	return cons;
}

void
ao_lisp_cons_print(struct ao_lisp_cons *cons)
{
	int	first = 1;
	printf("(");
	while (cons) {
		if (!first)
			printf(" ");
		fflush(stdout);
		ao_lisp_poly_print(cons->car);
		cons = cons->cdr;
		first = 0;
	}
	printf(")");
}
