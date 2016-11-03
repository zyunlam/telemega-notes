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

#define OFFSET(a)	((int) ((uint8_t *) (a) - ao_lisp_const))

#if 0
static int cons_depth;
#define DBG(...)	do { int d; for (d = 0; d < cons_depth; d++) printf ("  "); printf(__VA_ARGS__); } while(0)
#define DBG_IN()	(cons_depth++)
#define DBG_OUT()	(cons_depth--)
#define DBG_PR(c)	ao_lisp_cons_print(ao_lisp_cons_poly(c))
#define DBG_PRP(p)	ao_lisp_poly_print(p)
#else
#define DBG(...)
#define DBG_IN()
#define DBG_OUT()
#define DBG_PR(c)
#define DBG_PRP(p)
#endif

static void cons_mark(void *addr)
{
	struct ao_lisp_cons	*cons = addr;

	for (;;) {
		ao_lisp_poly_mark(cons->car);
		cons = ao_lisp_poly_cons(cons->cdr);
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

	DBG_IN();
	DBG("move cons start %d\n", OFFSET(cons));
	for (;;) {
		struct ao_lisp_cons	*cdr;
		ao_poly			car;

		car = ao_lisp_poly_move(cons->car);
		DBG(" moved car %d -> %d\n", OFFSET(ao_lisp_ref(cons->car)), OFFSET(ao_lisp_ref(car)));
		cons->car = car;
		cdr = ao_lisp_poly_cons(cons->cdr);
		cdr = ao_lisp_move_memory(cdr, sizeof (struct ao_lisp_cons));
		if (!cdr)
			break;
		DBG(" moved cdr %d -> %d\n", OFFSET(ao_lisp_poly_cons(cons->cdr)), OFFSET(cdr));
		cons->cdr = ao_lisp_cons_poly(cdr);
		cons = cdr;
	}
	DBG("move cons end\n");
	DBG_OUT();
}

const struct ao_lisp_type ao_lisp_cons_type = {
	.mark = cons_mark,
	.size = cons_size,
	.move = cons_move,
};

struct ao_lisp_cons *
ao_lisp_cons_cons(ao_poly car, struct ao_lisp_cons *cdr)
{
	struct ao_lisp_cons	*cons = ao_lisp_alloc(sizeof (struct ao_lisp_cons));
	if (!cons)
		return NULL;
	cons->car = car;
	cons->cdr = ao_lisp_cons_poly(cdr);
	return cons;
}

void
ao_lisp_cons_print(ao_poly c)
{
	struct ao_lisp_cons *cons = ao_lisp_poly_cons(c);
	int	first = 1;
	printf("(");
	while (cons) {
		if (!first)
			printf(" ");
		ao_lisp_poly_print(cons->car);
		cons = ao_lisp_poly_cons(cons->cdr);
		first = 0;
	}
	printf(")");
}
