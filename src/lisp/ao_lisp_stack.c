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

const struct ao_lisp_type ao_lisp_stack_type;

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
		ao_lisp_poly_mark(stack->sexprs, 0);
		ao_lisp_poly_mark(stack->values, 0);
		/* no need to mark values_tail */
		ao_lisp_poly_mark(stack->frame, 0);
		ao_lisp_poly_mark(stack->list, 0);
		stack = ao_lisp_poly_stack(stack->prev);
		if (ao_lisp_mark_memory(&ao_lisp_stack_type, stack))
			break;
	}
}

static void
stack_move(void *addr)
{
	struct ao_lisp_stack	*stack = addr;

	while (stack) {
		struct ao_lisp_stack	*prev;
		int			ret;
		(void) ao_lisp_poly_move(&stack->sexprs, 0);
		(void) ao_lisp_poly_move(&stack->values, 0);
		(void) ao_lisp_poly_move(&stack->values_tail, 0);
		(void) ao_lisp_poly_move(&stack->frame, 0);
		(void) ao_lisp_poly_move(&stack->list, 0);
		prev = ao_lisp_poly_stack(stack->prev);
		if (!prev)
			break;
		ret = ao_lisp_move_memory(&ao_lisp_stack_type, (void **) &prev);
		if (prev != ao_lisp_poly_stack(stack->prev))
			stack->prev = ao_lisp_stack_poly(prev);
		if (ret)
			break;
		stack = prev;
	}
}

const struct ao_lisp_type ao_lisp_stack_type = {
	.size = stack_size,
	.mark = stack_mark,
	.move = stack_move,
	.name = "stack"
};

struct ao_lisp_stack		*ao_lisp_stack_free_list;

void
ao_lisp_stack_reset(struct ao_lisp_stack *stack)
{
	stack->state = eval_sexpr;
	stack->sexprs = AO_LISP_NIL;
	stack->values = AO_LISP_NIL;
	stack->values_tail = AO_LISP_NIL;
}

static struct ao_lisp_stack *
ao_lisp_stack_new(void)
{
	struct ao_lisp_stack *stack;

	if (ao_lisp_stack_free_list) {
		stack = ao_lisp_stack_free_list;
		ao_lisp_stack_free_list = ao_lisp_poly_stack(stack->prev);
	} else {
		stack = ao_lisp_alloc(sizeof (struct ao_lisp_stack));
		if (!stack)
			return 0;
		stack->type = AO_LISP_STACK;
	}
	ao_lisp_stack_reset(stack);
	return stack;
}

int
ao_lisp_stack_push(void)
{
	struct ao_lisp_stack	*stack = ao_lisp_stack_new();

	if (!stack)
		return 0;

	stack->prev = ao_lisp_stack_poly(ao_lisp_stack);
	stack->frame = ao_lisp_frame_poly(ao_lisp_frame_current);
	stack->list = AO_LISP_NIL;

	ao_lisp_stack = stack;

	DBGI("stack push\n");
	DBG_FRAMES();
	DBG_IN();
	return 1;
}

void
ao_lisp_stack_pop(void)
{
	ao_poly			prev;
	struct ao_lisp_frame	*prev_frame;

	if (!ao_lisp_stack)
		return;
	prev = ao_lisp_stack->prev;
	if (!ao_lisp_stack_marked(ao_lisp_stack)) {
		ao_lisp_stack->prev = ao_lisp_stack_poly(ao_lisp_stack_free_list);
		ao_lisp_stack_free_list = ao_lisp_stack;
	}

	ao_lisp_stack = ao_lisp_poly_stack(prev);
	prev_frame = ao_lisp_frame_current;
	if (ao_lisp_stack)
		ao_lisp_frame_current = ao_lisp_poly_frame(ao_lisp_stack->frame);
	else
		ao_lisp_frame_current = NULL;
	if (ao_lisp_frame_current != prev_frame)
		ao_lisp_frame_free(prev_frame);
	DBG_OUT();
	DBGI("stack pop\n");
	DBG_FRAMES();
}

void
ao_lisp_stack_clear(void)
{
	ao_lisp_stack = NULL;
	ao_lisp_frame_current = NULL;
	ao_lisp_v = AO_LISP_NIL;
}

void
ao_lisp_stack_print(ao_poly poly)
{
	struct ao_lisp_stack *s = ao_lisp_poly_stack(poly);

	while (s) {
		if (s->type & AO_LISP_STACK_PRINT) {
			printf("[recurse...]");
			return;
		}
		s->type |= AO_LISP_STACK_PRINT;
		printf("\t[\n");
		printf("\t\texpr:   "); ao_lisp_poly_print(s->list); printf("\n");
		printf("\t\tstate:  %s\n", ao_lisp_state_names[s->state]);
		ao_lisp_error_poly ("values: ", s->values, s->values_tail);
		ao_lisp_error_poly ("sexprs: ", s->sexprs, AO_LISP_NIL);
		ao_lisp_error_frame(2, "frame:  ", ao_lisp_poly_frame(s->frame));
		printf("\t]\n");
		s->type &= ~AO_LISP_STACK_PRINT;
		s = ao_lisp_poly_stack(s->prev);
	}
}

/*
 * Copy a stack, being careful to keep everybody referenced
 */
static struct ao_lisp_stack *
ao_lisp_stack_copy(struct ao_lisp_stack *old)
{
	struct ao_lisp_stack *new = NULL;
	struct ao_lisp_stack *n, *prev = NULL;

	while (old) {
		ao_lisp_stack_stash(0, old);
		ao_lisp_stack_stash(1, new);
		ao_lisp_stack_stash(2, prev);
		n = ao_lisp_stack_new();
		prev = ao_lisp_stack_fetch(2);
		new = ao_lisp_stack_fetch(1);
		old = ao_lisp_stack_fetch(0);
		if (!n)
			return NULL;

		ao_lisp_stack_mark(old);
		ao_lisp_frame_mark(ao_lisp_poly_frame(old->frame));
		*n = *old;

		if (prev)
			prev->prev = ao_lisp_stack_poly(n);
		else
			new = n;
		prev = n;

		old = ao_lisp_poly_stack(old->prev);
	}
	return new;
}

/*
 * Evaluate a continuation invocation
 */
ao_poly
ao_lisp_stack_eval(void)
{
	struct ao_lisp_stack	*new = ao_lisp_stack_copy(ao_lisp_poly_stack(ao_lisp_v));
	if (!new)
		return AO_LISP_NIL;

	struct ao_lisp_cons	*cons = ao_lisp_poly_cons(ao_lisp_stack->values);

	if (!cons || !cons->cdr)
		return ao_lisp_error(AO_LISP_INVALID, "continuation requires a value");

	new->state = eval_val;

	ao_lisp_stack = new;
	ao_lisp_frame_current = ao_lisp_poly_frame(ao_lisp_stack->frame);

	return ao_lisp_poly_cons(cons->cdr)->car;
}

/*
 * Call with current continuation. This calls a lambda, passing
 * it a single argument which is the current continuation
 */
ao_poly
ao_lisp_call_cc(struct ao_lisp_cons *cons)
{
	struct ao_lisp_stack	*new;
	ao_poly			v;

	/* Make sure the single parameter is a lambda */
	if (!ao_lisp_check_argc(_ao_lisp_atom_call2fcc, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_call2fcc, cons, 0, AO_LISP_LAMBDA, 0))
		return AO_LISP_NIL;

	/* go get the lambda */
	ao_lisp_v = ao_lisp_arg(cons, 0);

	/* Note that the whole call chain now has
	 * a reference to it which may escape
	 */
	new = ao_lisp_stack_copy(ao_lisp_stack);
	if (!new)
		return AO_LISP_NIL;

	/* re-fetch cons after the allocation */
	cons = ao_lisp_poly_cons(ao_lisp_poly_cons(ao_lisp_stack->values)->cdr);

	/* Reset the arg list to the current stack,
	 * and call the lambda
	 */

	cons->car = ao_lisp_stack_poly(new);
	cons->cdr = AO_LISP_NIL;
	v = ao_lisp_lambda_eval();
	ao_lisp_stack->sexprs = v;
	ao_lisp_stack->state = eval_progn;
	return AO_LISP_NIL;
}
