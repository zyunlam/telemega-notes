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

#if 1
static int stack_depth;
#define DBG_INDENT()	do { int _s; for(_s = 0; _s < stack_depth; _s++) printf("  "); } while(0)
#define DBG_IN()	(++stack_depth)
#define DBG_OUT()	(--stack_depth)
#define DBG(...) 	printf(__VA_ARGS__)
#define DBGI(...)	do { DBG_INDENT(); DBG(__VA_ARGS__); } while (0)
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
	eval_exec,
	eval_exec_direct
};

struct ao_lisp_stack {
	ao_poly			prev;
	uint8_t			state;
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
		ao_lisp_poly_mark(stack->actuals);
		ao_lisp_poly_mark(stack->formals);
		ao_lisp_poly_mark(stack->frame);
		stack = ao_lisp_poly_stack(stack->prev);
		if (ao_lisp_mark_memory(stack, sizeof (struct ao_lisp_stack)))
			break;
	}
}

static void
stack_move(void *addr)
{
	struct ao_lisp_stack	*stack = addr;

	for (;;) {
		struct ao_lisp_stack *prev;
		stack->actuals = ao_lisp_poly_move(stack->actuals);
		stack->formals = ao_lisp_poly_move(stack->formals);
		stack->frame = ao_lisp_poly_move(stack->frame);
		prev = ao_lisp_ref(stack->prev);
		prev = ao_lisp_move_memory(prev, sizeof (struct ao_lisp_stack));
		stack->prev = ao_lisp_stack_poly(prev);
		stack = prev;
	}
}

static const struct ao_lisp_type ao_lisp_stack_type = {
	.size = stack_size,
	.mark = stack_mark,
	.move = stack_move
};


static struct ao_lisp_stack	*ao_lisp_stack;
static uint8_t been_here;

ao_poly
ao_lisp_set_cond(struct ao_lisp_cons *c)
{
	return AO_LISP_NIL;
}

static void
ao_lisp_stack_reset(struct ao_lisp_stack *stack)
{
	stack->state = eval_sexpr;
	stack->actuals = AO_LISP_NIL;
	stack->formals = AO_LISP_NIL;
	stack->formals_tail = AO_LISP_NIL;
	stack->frame = ao_lisp_frame_poly(ao_lisp_frame_current);
}

static struct ao_lisp_stack *
ao_lisp_stack_push(void)
{
	struct ao_lisp_stack	*stack = ao_lisp_alloc(sizeof (struct ao_lisp_stack));
	if (!stack)
		return NULL;
	stack->prev = ao_lisp_stack_poly(ao_lisp_stack);
	ao_lisp_stack_reset(stack);
	ao_lisp_stack = stack;
	DBGI("stack push\n");
	DBG_IN();
	return stack;
}

static struct ao_lisp_stack *
ao_lisp_stack_pop(void)
{
	if (!ao_lisp_stack)
		return NULL;
	DBG_OUT();
	DBGI("stack pop\n");
	ao_lisp_stack = ao_lisp_poly_stack(ao_lisp_stack->prev);
	if (ao_lisp_stack)
		ao_lisp_frame_current = ao_lisp_poly_frame(ao_lisp_stack->frame);
	else
		ao_lisp_frame_current = NULL;
	return ao_lisp_stack;
}

static void
ao_lisp_stack_clear(void)
{
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
	} else
		return ao_lisp_error(AO_LISP_INVALID, "not a func");
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
	next_frame = ao_lisp_frame_new(args_wanted, 0);
	DBGI("new frame %d\n", OFFSET(next_frame));
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
ao_lisp_eval(ao_poly v)
{
	struct ao_lisp_stack	*stack;
	ao_poly			formal;

	if (!been_here) {
		been_here = 1;
		ao_lisp_root_add(&ao_lisp_stack_type, &stack);
	}

	stack = ao_lisp_stack_push();

	for (;;) {
		if (ao_lisp_exception)
			return AO_LISP_NIL;
		switch (stack->state) {
		case eval_sexpr:
			DBGI("sexpr: "); DBG_POLY(v); DBG("\n");
			switch (ao_lisp_poly_type(v)) {
			case AO_LISP_CONS:
				if (v == AO_LISP_NIL) {
					stack->state = eval_exec;
					break;
				}
				stack->actuals = v;
				stack = ao_lisp_stack_push();
				v = ao_lisp_poly_cons(v)->car;
				break;
			case AO_LISP_ATOM:
				v = ao_lisp_atom_get(v);
				/* fall through */
			case AO_LISP_INT:
			case AO_LISP_STRING:
				stack->state = eval_val;
				break;
			}
			break;
		case eval_val:
			DBGI("val: "); DBG_POLY(v); DBG("\n");
			stack = ao_lisp_stack_pop();
			if (!stack)
				return v;

			stack->state = eval_sexpr;
			/* Check what kind of function we've got */
			if (!stack->formals) {
				switch (func_type(v)) {
				case AO_LISP_LAMBDA:
				case _ao_lisp_atom_lambda:
				case AO_LISP_LEXPR:
				case _ao_lisp_atom_lexpr:
					DBGI(".. lambda or lexpr\n");
					break;
				case AO_LISP_NLAMBDA:
				case _ao_lisp_atom_nlambda:
				case AO_LISP_MACRO:
				case _ao_lisp_atom_macro:
					DBGI(".. nlambda or macro\n");
					stack->formals = stack->actuals;
					stack->state = eval_exec_direct;
					break;
				}
				if (stack->state == eval_exec_direct)
					break;
			}

			formal = ao_lisp_cons_poly(ao_lisp_cons_cons(v, NULL));
			if (!formal) {
				ao_lisp_stack_clear();
				return AO_LISP_NIL;
			}

			if (stack->formals_tail)
				ao_lisp_poly_cons(stack->formals_tail)->cdr = formal;
			else
				stack->formals = formal;
			stack->formals_tail = formal;

			DBGI("formals now "); DBG_POLY(stack->formals); DBG("\n");

			v = ao_lisp_poly_cons(stack->actuals)->cdr;

			break;
		case eval_exec:
			v = ao_lisp_poly_cons(stack->formals)->car;
		case eval_exec_direct:
			DBGI("exec: "); DBG_POLY(v); DBG(" formals "); DBG_POLY(stack->formals); DBG ("\n");
			if (ao_lisp_poly_type(v) == AO_LISP_BUILTIN) {
				struct ao_lisp_builtin *b = ao_lisp_poly_builtin(v);

				v = ao_lisp_func(b) (ao_lisp_poly_cons(ao_lisp_poly_cons(stack->formals)->cdr));
				DBGI("builtin result:"); DBG_POLY(v); DBG ("\n");
				if (ao_lisp_exception) {
					ao_lisp_stack_clear();
					return AO_LISP_NIL;
				}
				stack->state = eval_val;
				break;
			} else {
				v = ao_lisp_lambda(ao_lisp_poly_cons(stack->formals));
				ao_lisp_stack_reset(stack);
			}
			break;
		}
	}
}
#if 0


	restart:
		if (cond) {
			DBGI("cond is now "); DBG_CONS(cond); DBG("\n");
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
			save_cond = cond;
			cond = NULL;

			v = actuals->car;

//			DBG("start: stack"); DBG_CONS(stack); DBG("\n");
//			DBG("start: actuals"); DBG_CONS(actuals); DBG("\n");
//			DBG("start: formals"); DBG_CONS(formals); DBG("\n");
		}

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
				struct ao_lisp_cons *old_cond = cond;
				struct ao_lisp_builtin *b = ao_lisp_poly_builtin(v);

				v = ao_lisp_func(b) (ao_lisp_poly_cons(formals->cdr));

				DBG ("eval: ");
				DBG_CONS(formals);
				DBG(" -> ");
				DBG_POLY(v);
				DBG ("\n");
				if (ao_lisp_exception)
					goto bail;

				if (cond != old_cond) {
					DBG("cond changed from "); DBG_CONS(old_cond); DBG(" to "); DBG_CONS(cond); DBG("\n");
					actuals = NULL;
					formals = 0;
					formals_tail = 0;
					save_cons = cons;
					cons = 0;
					goto restart;
				}
			} else {
				v = ao_lisp_lambda(formals);
				if (ao_lisp_exception)
					goto bail;
			}

		cond_done:
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
		}
		if (cond) {
			DBG("next cond cons is %d\n", cons);
			if (v) {
				v = ao_lisp_poly_cons(cond->car)->cdr;
				cond = 0;
				cons = save_cons;
				if (v != AO_LISP_NIL) {
					v = ao_lisp_poly_cons(v)->car;
					DBG("cond complete, sexpr is "); DBG_POLY(v); DBG("\n");
				}
				goto cond_done;
			} else {
				cond = ao_lisp_poly_cons(cond->cdr);
				DBG("next cond is "); DBG_CONS(cond); DBG("\n");
				goto restart;
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
#endif

