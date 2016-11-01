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

/*
 * Non-recursive eval
 *
 * Plan: walk actuals, construct formals
 *
 * stack >  save  > actuals > actual_1
 *           v         v
 *	   formals     .    > actual_2
 */

static struct ao_lisp_cons	*stack;
static struct ao_lisp_cons	*actuals;
static struct ao_lisp_cons	*formals;
static struct ao_lisp_cons	*formals_tail;
static uint8_t been_here;

#if 0
#define DBG(...) printf(__VA_ARGS__)
#define DBG_CONS(a)	ao_lisp_cons_print(a)
#define DBG_POLY(a)	ao_lisp_poly_print(a)
#else
#define DBG(...)
#define DBG_CONS(a)
#define DBG_POLY(a)
#endif

ao_lisp_poly
ao_lisp_eval(ao_lisp_poly v)
{
	struct ao_lisp_cons	*formal;
	int			cons = 0;

	if (!been_here) {
		been_here = 1;
		ao_lisp_root_add(&ao_lisp_cons_type, &stack);
		ao_lisp_root_add(&ao_lisp_cons_type, &actuals);
		ao_lisp_root_add(&ao_lisp_cons_type, &formals);
		ao_lisp_root_add(&ao_lisp_cons_type, &formals_tail);
	}
	stack = 0;
	actuals = 0;
	formals = 0;
	formals_tail = 0;
	for (;;) {

		/* Build stack frames for each list */
		while (ao_lisp_poly_type(v) == AO_LISP_CONS) {
			if (v == AO_LISP_NIL)
				break;

			/* Push existing frame on the stack */
			if (cons++) {
				struct ao_lisp_cons *frame;

				frame = ao_lisp_cons(ao_lisp_cons_poly(actuals), formals);
				stack = ao_lisp_cons(ao_lisp_cons_poly(frame), stack);
			}
			actuals = ao_lisp_poly_cons(v);
			formals = NULL;
			formals_tail = NULL;
			v = actuals->car;

			DBG("start: stack"); DBG_CONS(stack); DBG("\n");
			DBG("start: actuals"); DBG_CONS(actuals); DBG("\n");
			DBG("start: formals"); DBG_CONS(formals); DBG("\n");
		}

		/* Evaluate primitive types */

		switch (ao_lisp_poly_type(v)) {
		case AO_LISP_INT:
		case AO_LISP_STRING:
			break;
		case AO_LISP_ATOM:
			v = ao_lisp_poly_atom(v)->val;
			break;
		}

		for (;;) {
			DBG("add formal: "); DBG_POLY(v); DBG("\n");

			formal = ao_lisp_cons(v, NULL);
			if (formals_tail)
				formals_tail->cdr = formal;
			else
				formals = formal;
			formals_tail = formal;
			actuals = actuals->cdr;

			DBG("formals: ");
			DBG_CONS(formals);
			DBG("\n");
			DBG("actuals: ");
			DBG_CONS(actuals);
			DBG("\n");

			/* Process all of the arguments */
			if (actuals) {
				v = actuals->car;
				DBG ("actual: "); DBG_POLY(v); DBG("\n");
				break;
			}

			v = formals->car;

			/* Evaluate the resulting list */
			if (ao_lisp_poly_type(v) == AO_LISP_BUILTIN) {
				struct ao_lisp_builtin *b = ao_lisp_poly_builtin(v);

				v = b->func(formals->cdr);

				DBG ("eval: ");
				DBG_CONS(formals);
				DBG(" -> ");
				DBG_POLY(v);
				DBG ("\n");
			} else {
				DBG ("invalid eval\n");
			}

			if (--cons) {
				struct ao_lisp_cons	*frame;

				/* Pop the previous frame off the stack */
				frame = ao_lisp_poly_cons(stack->car);
				actuals = ao_lisp_poly_cons(frame->car);
				formals = frame->cdr;

				/* Recompute the tail of the formals list */
				for (formal = formals; formal->cdr != NULL; formal = formal->cdr);
				formals_tail = formal;

				stack = stack->cdr;
				DBG("stack pop: stack"); DBG_CONS(stack); DBG("\n");
				DBG("stack pop: actuals"); DBG_CONS(actuals); DBG("\n");
				DBG("stack pop: formals"); DBG_CONS(formals); DBG("\n");
			} else {
				DBG("done func\n");
				break;
			}
		}
		if (!cons)
			break;
	}
	return v;
}
