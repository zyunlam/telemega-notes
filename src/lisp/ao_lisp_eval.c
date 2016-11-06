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
#define DBG_CODE	1
static int stack_depth;
#define DBG_INDENT()	do { int _s; for(_s = 0; _s < stack_depth; _s++) printf("  "); } while(0)
#define DBG_IN()	(++stack_depth)
#define DBG_OUT()	(--stack_depth)
#define DBG(...) 	printf(__VA_ARGS__)
#define DBGI(...)	do { DBG_INDENT(); DBG("%4d: ", __LINE__); DBG(__VA_ARGS__); } while (0)
#define DBG_CONS(a)	ao_lisp_cons_print(ao_lisp_cons_poly(a))
#define DBG_POLY(a)	ao_lisp_poly_print(a)
#define OFFSET(a)	((a) ? (int) ((uint8_t *) a - ao_lisp_pool) : -1)
#else
#define DBG_INDENT()
#define DBG_IN()
#define DBG_OUT()
#define DBG(...)
#define DBGI(...)
#define DBG_CONS(a)
#define DBG_POLY(a)
#endif

enum eval_state {
	eval_sexpr,
	eval_val,
	eval_formal,
	eval_exec,
	eval_exec_direct,
	eval_cond,
	eval_cond_test
};

struct ao_lisp_stack {
	ao_poly			prev;
	uint8_t			state;
	uint8_t			macro;
	ao_poly			actuals;
	ao_poly			formals;
	ao_poly			formals_tail;
	ao_poly			frame;
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
		ao_lisp_poly_mark(stack->actuals, 0);
		ao_lisp_poly_mark(stack->formals, 0);
		/* no need to mark formals_tail */
		ao_lisp_poly_mark(stack->frame, 0);
		stack = ao_lisp_poly_stack(stack->prev);
		if (ao_lisp_mark_memory(stack, sizeof (struct ao_lisp_stack)))
			break;
	}
}

static const struct ao_lisp_type ao_lisp_stack_type;

#if DBG_CODE
static void
stack_validate_tail(struct ao_lisp_stack *stack)
{
	struct ao_lisp_cons *head = ao_lisp_poly_cons(stack->formals);
	struct ao_lisp_cons *tail = ao_lisp_poly_cons(stack->formals_tail);
	struct ao_lisp_cons *cons;
	for (cons = head; cons && cons->cdr && cons != tail; cons = ao_lisp_poly_cons(cons->cdr))
		;
	if (cons != tail || (tail && tail->cdr)) {
		if (!tail) {
			printf("tail null\n");
		} else {
			printf("tail validate fail head %d actual %d recorded %d\n",
			       OFFSET(head), OFFSET(cons), OFFSET(tail));
			abort();
		}
	}
}
#else
#define stack_validate_tail(s)
#endif

static void
stack_move(void *addr)
{
	struct ao_lisp_stack	*stack = addr;

	while (stack) {
		void	*prev;
		int	ret;
		(void) ao_lisp_poly_move(&stack->actuals, 0);
		(void) ao_lisp_poly_move(&stack->formals, 0);
		(void) ao_lisp_poly_move(&stack->formals_tail, 0);
		(void) ao_lisp_poly_move(&stack->frame, 0);
		prev = ao_lisp_poly_stack(stack->prev);
		ret = ao_lisp_move(&ao_lisp_stack_type, &prev);
		if (prev != ao_lisp_poly_stack(stack->prev))
			stack->prev = ao_lisp_stack_poly(prev);
		stack_validate_tail(stack);
		if (ret)
			break;
		stack = ao_lisp_poly_stack(stack->prev);
	}
}

static const struct ao_lisp_type ao_lisp_stack_type = {
	.size = stack_size,
	.mark = stack_mark,
	.move = stack_move
};

static struct ao_lisp_stack	*ao_lisp_stack;
static ao_poly			ao_lisp_v;
static uint8_t been_here;

#if DBG_CODE
static void
stack_validate_tails(void)
{
	struct ao_lisp_stack	*stack;

	for (stack = ao_lisp_stack; stack; stack = ao_lisp_poly_stack(stack->prev))
		stack_validate_tail(stack);
}
#else
#define stack_validate_tails(s)
#endif

ao_poly
ao_lisp_set_cond(struct ao_lisp_cons *c)
{
	ao_lisp_stack->state = eval_cond;
	ao_lisp_stack->actuals = ao_lisp_cons_poly(c);
	return AO_LISP_NIL;
}

void
ao_lisp_stack_reset(struct ao_lisp_stack *stack)
{
	stack->state = eval_sexpr;
	stack->macro = 0;
	stack->actuals = AO_LISP_NIL;
	stack->formals = AO_LISP_NIL;
	stack->formals_tail = AO_LISP_NIL;
	stack->frame = ao_lisp_frame_poly(ao_lisp_frame_current);
	stack_validate_tails();
}

int
ao_lisp_stack_push(void)
{
	stack_validate_tails();
	if (ao_lisp_stack) {
		DBGI("formals "); DBG_POLY(ao_lisp_stack->formals); DBG("\n");
		DBGI("actuals "); DBG_POLY(ao_lisp_stack->actuals); DBG("\n");
	}
	DBGI("stack push\n");
	DBG_IN();
	struct ao_lisp_stack	*stack = ao_lisp_alloc(sizeof (struct ao_lisp_stack));
	if (!stack)
		return 0;
	stack->prev = ao_lisp_stack_poly(ao_lisp_stack);
	ao_lisp_stack = stack;
	ao_lisp_stack_reset(stack);
	stack_validate_tails();
	return 1;
}

void
ao_lisp_stack_pop(void)
{
	if (!ao_lisp_stack)
		return;
	stack_validate_tails();
	DBG_OUT();
	DBGI("stack pop\n");
	ao_lisp_stack = ao_lisp_poly_stack(ao_lisp_stack->prev);
	if (ao_lisp_stack)
		ao_lisp_frame_current = ao_lisp_poly_frame(ao_lisp_stack->frame);
	else
		ao_lisp_frame_current = NULL;
	if (ao_lisp_stack) {
		DBGI("formals "); DBG_POLY(ao_lisp_stack->formals); DBG("\n");
		DBGI("actuals "); DBG_POLY(ao_lisp_stack->actuals); DBG("\n");
	}
}

static void
ao_lisp_stack_clear(void)
{
	stack_validate_tails();
	ao_lisp_stack = NULL;
	ao_lisp_frame_current = NULL;
}

static ao_poly
func_type(ao_poly func)
{
	struct ao_lisp_cons	*cons;
	struct ao_lisp_cons	*args;
	int			f;

	DBGI("func type "); DBG_POLY(func); DBG("\n");
	if (func == AO_LISP_NIL)
		return ao_lisp_error(AO_LISP_INVALID, "func is nil");
	if (ao_lisp_poly_type(func) == AO_LISP_BUILTIN) {
		struct ao_lisp_builtin *b = ao_lisp_poly_builtin(func);
		return b->args;
	} else if (ao_lisp_poly_type(func) == AO_LISP_CONS) {
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
	} else {
		ao_lisp_error(AO_LISP_INVALID, "not a func");
		abort();
		return AO_LISP_NIL;
	}
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
	struct ao_lisp_frame	*next_frame;
	int			args_wanted;
	int			args_provided;

	lambda = ao_lisp_poly_cons(ao_lisp_arg(cons, 0));
	DBGI("lambda "); DBG_CONS(lambda); DBG("\n");
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
	next_frame = ao_lisp_frame_new(args_wanted);
//	DBGI("new frame %d\n", OFFSET(next_frame));
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
	next_frame->next = ao_lisp_frame_poly(ao_lisp_frame_current);
	ao_lisp_frame_current = next_frame;
	ao_lisp_stack->frame = ao_lisp_frame_poly(next_frame);
	return ao_lisp_arg(lambda, 2);
}

ao_poly
ao_lisp_eval(ao_poly _v)
{
	ao_poly			formal;

	ao_lisp_v = _v;
	if (!been_here) {
		been_here = 1;
		ao_lisp_root_add(&ao_lisp_stack_type, &ao_lisp_stack);
		ao_lisp_root_poly_add(&ao_lisp_v);
	}

	if (!ao_lisp_stack_push())
		goto bail;

	for (;;) {
		if (ao_lisp_exception)
			goto bail;
		switch (ao_lisp_stack->state) {
		case eval_sexpr:
			DBGI("sexpr: "); DBG_POLY(ao_lisp_v); DBG("\n");
			switch (ao_lisp_poly_type(ao_lisp_v)) {
			case AO_LISP_CONS:
				if (ao_lisp_v == AO_LISP_NIL) {
					ao_lisp_stack->state = eval_exec;
					break;
				}
				ao_lisp_stack->actuals = ao_lisp_v;
				DBGI("actuals now "); DBG_POLY(ao_lisp_v); DBG("\n");
				ao_lisp_stack->state = eval_formal;
				if (!ao_lisp_stack_push())
					goto bail;
				ao_lisp_v = ao_lisp_poly_cons(ao_lisp_v)->car;
				stack_validate_tails();
				break;
			case AO_LISP_ATOM:
				ao_lisp_v = ao_lisp_atom_get(ao_lisp_v);
				/* fall through */
			case AO_LISP_INT:
			case AO_LISP_STRING:
			case AO_LISP_BUILTIN:
				ao_lisp_stack->state = eval_val;
				break;
			}
			break;
		case eval_val:
			DBGI("val: "); DBG_POLY(ao_lisp_v); DBG("\n");
			ao_lisp_stack_pop();
			if (!ao_lisp_stack)
				return ao_lisp_v;
			DBGI("..state %d\n", ao_lisp_stack->state);
			break;

		case eval_formal:
			/* Check what kind of function we've got */
			if (!ao_lisp_stack->formals) {
				switch (func_type(ao_lisp_v)) {
				case AO_LISP_LAMBDA:
				case _ao_lisp_atom_lambda:
				case AO_LISP_LEXPR:
				case _ao_lisp_atom_lexpr:
					DBGI(".. lambda or lexpr\n");
					break;
				case AO_LISP_MACRO:
				case _ao_lisp_atom_macro:
					ao_lisp_stack->macro = 1;
				case AO_LISP_NLAMBDA:
				case _ao_lisp_atom_nlambda:
					DBGI(".. nlambda or macro\n");
					ao_lisp_stack->formals = ao_lisp_stack->actuals;
					ao_lisp_stack->formals_tail = AO_LISP_NIL;
					ao_lisp_stack->state = eval_exec_direct;
					stack_validate_tails();
					break;
				}
				if (ao_lisp_stack->state == eval_exec_direct)
					break;
			}

			DBGI("add formal "); DBG_POLY(ao_lisp_v); DBG("\n");
			stack_validate_tails();
			formal = ao_lisp_cons_poly(ao_lisp_cons_cons(ao_lisp_v, NULL));
			stack_validate_tails();
			if (!formal)
				goto bail;

			if (ao_lisp_stack->formals_tail)
				ao_lisp_poly_cons(ao_lisp_stack->formals_tail)->cdr = formal;
			else
				ao_lisp_stack->formals = formal;
			ao_lisp_stack->formals_tail = formal;

			DBGI("formals now "); DBG_POLY(ao_lisp_stack->formals); DBG("\n");

			ao_lisp_v = ao_lisp_poly_cons(ao_lisp_stack->actuals)->cdr;

			stack_validate_tails();
			ao_lisp_stack->state = eval_sexpr;

			break;
		case eval_exec:
			if (!ao_lisp_stack->formals) {
				ao_lisp_v = AO_LISP_NIL;
				ao_lisp_stack->state = eval_val;
				break;
			}
			ao_lisp_v = ao_lisp_poly_cons(ao_lisp_stack->formals)->car;
		case eval_exec_direct:
			DBGI("exec: macro %d ", ao_lisp_stack->macro); DBG_POLY(ao_lisp_v); DBG(" formals "); DBG_POLY(ao_lisp_stack->formals); DBG ("\n");
			if (ao_lisp_poly_type(ao_lisp_v) == AO_LISP_BUILTIN) {
				stack_validate_tails();
				struct ao_lisp_builtin	*b = ao_lisp_poly_builtin(ao_lisp_v);
				stack_validate_tails();
				struct ao_lisp_cons	*f = ao_lisp_poly_cons(ao_lisp_poly_cons(ao_lisp_stack->formals)->cdr);

				DBGI(".. builtin formals "); DBG_CONS(f); DBG("\n");
				stack_validate_tails();
				if (ao_lisp_stack->macro)
					ao_lisp_stack->state = eval_sexpr;
				else
					ao_lisp_stack->state = eval_val;
				ao_lisp_stack->macro = 0;
				ao_lisp_stack->actuals = ao_lisp_stack->formals = ao_lisp_stack->formals_tail = AO_LISP_NIL;
				ao_lisp_v = ao_lisp_func(b) (f);
				DBGI("builtin result:"); DBG_POLY(ao_lisp_v); DBG ("\n");
				if (ao_lisp_exception)
					goto bail;
				break;
			} else {
				ao_lisp_v = ao_lisp_lambda(ao_lisp_poly_cons(ao_lisp_stack->formals));
				ao_lisp_stack_reset(ao_lisp_stack);
			}
			break;
		case eval_cond:
			DBGI("cond: "); DBG_POLY(ao_lisp_stack->actuals); DBG("\n");
			if (!ao_lisp_stack->actuals) {
				ao_lisp_v = AO_LISP_NIL;
				ao_lisp_stack->state = eval_val;
			} else {
				ao_lisp_v = ao_lisp_poly_cons(ao_lisp_stack->actuals)->car;
				if (!ao_lisp_v || ao_lisp_poly_type(ao_lisp_v) != AO_LISP_CONS) {
					ao_lisp_error(AO_LISP_INVALID, "invalid cond clause");
					goto bail;
				}
				ao_lisp_v = ao_lisp_poly_cons(ao_lisp_v)->car;
				ao_lisp_stack->state = eval_cond_test;
				stack_validate_tails();
				ao_lisp_stack_push();
				stack_validate_tails();
				ao_lisp_stack->state = eval_sexpr;
			}
			break;
		case eval_cond_test:
			DBGI("cond_test: "); DBG_POLY(ao_lisp_v); DBG(" actuals "); DBG_POLY(ao_lisp_stack->actuals); DBG("\n");
			if (ao_lisp_v) {
				struct ao_lisp_cons *car = ao_lisp_poly_cons(ao_lisp_poly_cons(ao_lisp_stack->actuals)->car);
				struct ao_lisp_cons *c = ao_lisp_poly_cons(car->cdr);
				if (c) {
					ao_lisp_v = c->car;
					ao_lisp_stack->state = eval_sexpr;
				} else {
					ao_lisp_stack->state = eval_val;
				}
			} else {
				ao_lisp_stack->actuals = ao_lisp_poly_cons(ao_lisp_stack->actuals)->cdr;
				DBGI("actuals now "); DBG_POLY(ao_lisp_stack->actuals); DBG("\n");
				ao_lisp_stack->state = eval_cond;
			}
			break;
		}
	}
bail:
	ao_lisp_stack_clear();
	return AO_LISP_NIL;
}
