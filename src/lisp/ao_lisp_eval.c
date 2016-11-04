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

#if 0
#define DBG(...) printf(__VA_ARGS__)
#define DBG_CONS(a)	ao_lisp_cons_print(ao_lisp_cons_poly(a))
#define DBG_POLY(a)	ao_lisp_poly_print(a)
#define OFFSET(a)	((a) ? (int) ((uint8_t *) a - ao_lisp_pool) : -1)
#else
#define DBG(...)
#define DBG_CONS(a)
#define DBG_POLY(a)
#endif

struct ao_lisp_stack {
	ao_poly			next;
	ao_poly			actuals;
	ao_poly			formals;
	ao_poly			frame;
	ao_poly			cond;
};

static struct ao_lisp_stack *
ao_lisp_poly_stack(ao_poly p)
{
	return ao_lisp_ref(p);
}

static ao_poly
ao_lisp_stack_poly(struct ao_lisp_stack *stack)
{
	return ao_lisp_poly(stack, AO_LISP_OTHER);
}

static int
stack_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_lisp_stack);
}

static void
stack_mark(void *addr)
{
	struct ao_lisp_stack	*stack = addr;
	for (;;) {
		ao_lisp_poly_mark(stack->actuals);
		ao_lisp_poly_mark(stack->formals);
		ao_lisp_poly_mark(stack->frame);
		ao_lisp_poly_mark(stack->cond);
		stack = ao_lisp_poly_stack(stack->next);
		if (ao_lisp_mark_memory(stack, sizeof (struct ao_lisp_stack)))
			break;
	}
}

static void
stack_move(void *addr)
{
	struct ao_lisp_stack	*stack = addr;

	for (;;) {
		struct ao_lisp_stack *next;
		stack->actuals = ao_lisp_poly_move(stack->actuals);
		stack->formals = ao_lisp_poly_move(stack->formals);
		stack->frame = ao_lisp_poly_move(stack->frame);
		stack->cond = ao_lisp_poly_move(stack->cond);
		next = ao_lisp_ref(stack->next);
		next = ao_lisp_move_memory(next, sizeof (struct ao_lisp_stack));
		stack->next = ao_lisp_stack_poly(next);
		stack = next;
	}
}

static const struct ao_lisp_type ao_lisp_stack_type = {
	.size = stack_size,
	.mark = stack_mark,
	.move = stack_move
};


static struct ao_lisp_stack	*stack;
static struct ao_lisp_cons	*actuals;
static struct ao_lisp_cons	*formals;
static struct ao_lisp_cons	*formals_tail;
static struct ao_lisp_cons	*cond;
struct ao_lisp_frame		*next_frame;
static uint8_t been_here;

ao_poly
ao_lisp_set_cond(struct ao_lisp_cons *c)
{
	cond = c;
	return AO_LISP_NIL;
}

static int
ao_lisp_stack_push(void)
{
	struct ao_lisp_stack	*n = ao_lisp_alloc(sizeof (struct ao_lisp_stack));
	if (!n)
		return 0;
	n->next = ao_lisp_stack_poly(stack);
	n->actuals = ao_lisp_cons_poly(actuals);
	n->formals = ao_lisp_cons_poly(formals);
	n->cond = ao_lisp_cons_poly(cond);
	n->frame = ao_lisp_frame_poly(ao_lisp_frame_current);
	DBG("push frame %d\n", OFFSET(ao_lisp_frame_current));
	stack = n;
	return 1;
}

static void
ao_lisp_stack_pop(void)
{
	actuals = ao_lisp_poly_cons(stack->actuals);
	formals = ao_lisp_poly_cons(stack->formals);
	cond = ao_lisp_poly_cons(stack->cond);
	ao_lisp_frame_current = ao_lisp_poly_frame(stack->frame);
	DBG("pop frame %d\n", OFFSET(ao_lisp_frame_current));
	formals_tail = 0;

	/* Recompute the tail of the formals list */
	if (formals) {
		struct ao_lisp_cons *formal;
		for (formal = formals; formal->cdr != AO_LISP_NIL; formal = ao_lisp_poly_cons(formal->cdr));
		formals_tail = formal;
	}
	stack = ao_lisp_poly_stack(stack->next);
}

static void
ao_lisp_stack_clear(void)
{
	stack = 0;
	actuals = formals = formals_tail = 0;
	cond = 0;
	ao_lisp_frame_current = 0;
}


static ao_poly
func_type(ao_poly func)
{
	struct ao_lisp_cons	*cons;
	struct ao_lisp_cons	*args;
	int			f;

	DBG("func type "); DBG_POLY(func); DBG("\n");
	if (func == AO_LISP_NIL)
		return ao_lisp_error(AO_LISP_INVALID, "func is nil");
	if (ao_lisp_poly_type(func) != AO_LISP_CONS)
		return ao_lisp_error(AO_LISP_INVALID, "func is not list");
	cons = ao_lisp_poly_cons(func);
	if (!ao_lisp_check_argc(_ao_lisp_atom_lambda, cons, 3, 3))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_lambda, cons, 0, AO_LISP_ATOM, 0))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_lambda, cons, 1, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	args = ao_lisp_poly_cons(ao_lisp_arg(cons, 1));
	f = 0;
	while (args) {
		if (ao_lisp_poly_type(args->car) != AO_LISP_ATOM) {
			return ao_lisp_error(ao_lisp_arg(cons, 0), "formal %d is not an atom", f);
		}
		args = ao_lisp_poly_cons(args->cdr);
		f++;
	}
	return ao_lisp_arg(cons, 0);
}

static int
ao_lisp_cons_length(struct ao_lisp_cons *cons)
{
	int	len = 0;
	while (cons) {
		len++;
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	return len;
}

static ao_poly
ao_lisp_lambda(struct ao_lisp_cons *cons)
{
	ao_poly			type;
	struct ao_lisp_cons	*lambda;
	struct ao_lisp_cons	*args;
	int			args_wanted;
	int			args_provided;

	lambda = ao_lisp_poly_cons(ao_lisp_arg(cons, 0));
	DBG("lambda "); DBG_CONS(lambda); DBG("\n");
	type = ao_lisp_arg(lambda, 0);
	args = ao_lisp_poly_cons(ao_lisp_arg(lambda, 1));

	args_wanted = ao_lisp_cons_length(args);

	/* Create a frame to hold the variables
	 */
	if (type == _ao_lisp_atom_lambda)
		args_provided = ao_lisp_cons_length(cons) - 1;
	else
		args_provided = 1;
	if (args_wanted != args_provided)
		return ao_lisp_error(AO_LISP_INVALID, "need %d args, not %d", args_wanted, args_provided);
	next_frame = ao_lisp_frame_new(args_wanted, 0);
	DBG("new frame %d\n", OFFSET(next_frame));
	switch (type) {
	case _ao_lisp_atom_lambda: {
		int			f;
		struct ao_lisp_cons	*vals = ao_lisp_poly_cons(cons->cdr);

		for (f = 0; f < args_wanted; f++) {
			next_frame->vals[f].atom = args->car;
			next_frame->vals[f].val = vals->car;
			args = ao_lisp_poly_cons(args->cdr);
			vals = ao_lisp_poly_cons(vals->cdr);
		}
		break;
	}
	case _ao_lisp_atom_lexpr:
	case _ao_lisp_atom_nlambda:
		next_frame->vals[0].atom = args->car;
		next_frame->vals[0].val = cons->cdr;
		break;
	case _ao_lisp_atom_macro:
		next_frame->vals[0].atom = args->car;
		next_frame->vals[0].val = ao_lisp_cons_poly(cons);
		break;
	}
	return ao_lisp_arg(lambda, 2);
}

ao_poly
ao_lisp_eval(ao_poly v)
{
	struct ao_lisp_cons	*formal;
	int			cons = 0;

	if (!been_here) {
		been_here = 1;
		ao_lisp_root_add(&ao_lisp_stack_type, &stack);
		ao_lisp_root_add(&ao_lisp_cons_type, &actuals);
		ao_lisp_root_add(&ao_lisp_cons_type, &formals);
		ao_lisp_root_add(&ao_lisp_cons_type, &formals_tail);
	}
	stack = 0;
	actuals = 0;
	formals = 0;
	formals_tail = 0;
	cond = 0;
	for (;;) {

	restart:
		if (cond) {
			if (cond->car == AO_LISP_NIL) {
				cond = AO_LISP_NIL;
				v = AO_LISP_NIL;
			} else {
				if (ao_lisp_poly_type(cond->car) != AO_LISP_CONS) {
					ao_lisp_error(AO_LISP_INVALID, "malformed cond");
					goto bail;
				}
				v = ao_lisp_poly_cons(cond->car)->car;
			}
		}

		/* Build stack frames for each list */
		while (ao_lisp_poly_type(v) == AO_LISP_CONS) {
			if (v == AO_LISP_NIL)
				break;

			/* Push existing bits on the stack */
			if (cons++)
				if (!ao_lisp_stack_push())
					goto bail;

			actuals = ao_lisp_poly_cons(v);
			formals = NULL;
			formals_tail = NULL;
			cond = NULL;

			v = actuals->car;

//			DBG("start: stack"); DBG_CONS(stack); DBG("\n");
//			DBG("start: actuals"); DBG_CONS(actuals); DBG("\n");
//			DBG("start: formals"); DBG_CONS(formals); DBG("\n");
		}

		/* Evaluate primitive types */

		DBG ("actual: "); DBG_POLY(v); DBG("\n");

		switch (ao_lisp_poly_type(v)) {
		case AO_LISP_INT:
		case AO_LISP_STRING:
			break;
		case AO_LISP_ATOM:
			v = ao_lisp_atom_get(v);
			break;
		}

		while (cons) {
			DBG("add formal: "); DBG_POLY(v); DBG("\n");

			/* We've processed the first element of the list, go check
			 * what kind of function we've got
			 */
			if (formals == NULL) {
				if (ao_lisp_poly_type(v) == AO_LISP_BUILTIN) {
					struct ao_lisp_builtin *b = ao_lisp_poly_builtin(v);
					switch (b->args) {
					case AO_LISP_NLAMBDA:
						formals = actuals;
						goto eval;

					case AO_LISP_MACRO:
						v = ao_lisp_func(b)(ao_lisp_poly_cons(actuals->cdr));
						DBG("macro "); DBG_POLY(ao_lisp_cons_poly(actuals));
						DBG(" -> "); DBG_POLY(v);
						DBG("\n");
						if (ao_lisp_poly_type(v) != AO_LISP_CONS) {
							ao_lisp_error(AO_LISP_INVALID, "macro didn't return list");
							goto bail;
						}
						/* Reset frame to the new list */
						actuals = ao_lisp_poly_cons(v);
						v = actuals->car;
						goto restart;
					}
				} else {
					switch (func_type(v)) {
					case _ao_lisp_atom_lambda:
					case _ao_lisp_atom_lexpr:
						break;
					case _ao_lisp_atom_nlambda:
						formals = actuals;
						goto eval;
					case _ao_lisp_atom_macro:
						break;
					default:
						ao_lisp_error(AO_LISP_INVALID, "operator is not a function");
						goto bail;
					}
				}
			}

			formal = ao_lisp_cons_cons(v, NULL);
			if (formals_tail)
				formals_tail->cdr = ao_lisp_cons_poly(formal);
			else
				formals = formal;
			formals_tail = formal;
			actuals = ao_lisp_poly_cons(actuals->cdr);

			DBG("formals: ");
			DBG_CONS(formals);
			DBG("\n");
			DBG("actuals: ");
			DBG_CONS(actuals);
			DBG("\n");

			/* Process all of the arguments */
			if (actuals) {
				v = actuals->car;
				break;
			}

			v = formals->car;

		eval:

			/* Evaluate the resulting list */
			if (ao_lisp_poly_type(v) == AO_LISP_BUILTIN) {
				struct ao_lisp_builtin *b = ao_lisp_poly_builtin(v);

				v = ao_lisp_func(b) (ao_lisp_poly_cons(formals->cdr));

				DBG ("eval: ");
				DBG_CONS(formals);
				DBG(" -> ");
				DBG_POLY(v);
				DBG ("\n");
				if (ao_lisp_exception)
					goto bail;

				if (cond)
					goto restart;
			} else {
				v = ao_lisp_lambda(formals);
				if (ao_lisp_exception)
					goto bail;
			}

			--cons;
			if (cons) {
				ao_lisp_stack_pop();
//				DBG("stack pop: stack"); DBG_CONS(stack); DBG("\n");
//				DBG("stack pop: actuals"); DBG_CONS(actuals); DBG("\n");
//				DBG("stack pop: formals"); DBG_CONS(formals); DBG("\n");
			} else {
				actuals = 0;
				formals = 0;
				formals_tail = 0;
				ao_lisp_frame_current = 0;
			}
			if (next_frame) {
				ao_lisp_frame_current = next_frame;
				DBG("next frame %d\n", OFFSET(next_frame));
				next_frame = 0;
				goto restart;
			}
			if (cond) {
				if (v) {
					v = ao_lisp_poly_cons(cond->car)->cdr;
					if (v != AO_LISP_NIL) {
						v = ao_lisp_poly_cons(v)->car;
						goto restart;
					}
				} else {
					cond = ao_lisp_poly_cons(cond->cdr);
					goto restart;
				}
			}
		}
		if (!cons)
			break;
	}
	DBG("leaving frame at %d\n", OFFSET(ao_lisp_frame_current));
	return v;
bail:
	ao_lisp_stack_clear();
	return AO_LISP_NIL;
}
