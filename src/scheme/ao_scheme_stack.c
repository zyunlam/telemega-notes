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

const struct ao_scheme_type ao_scheme_stack_type;

static int
stack_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_scheme_stack);
}

static void
stack_mark(void *addr)
{
	struct ao_scheme_stack	*stack = addr;
	for (;;) {
		ao_scheme_poly_mark(stack->sexprs, 1);
		ao_scheme_poly_mark(stack->values, 1);
		/* no need to mark values_tail */
		ao_scheme_poly_mark(stack->frame, 0);
		ao_scheme_poly_mark(stack->list, 1);
		stack = ao_scheme_poly_stack(stack->prev);
		if (ao_scheme_mark_memory(&ao_scheme_stack_type, stack))
			break;
	}
}

static void
stack_move(void *addr)
{
	struct ao_scheme_stack	*stack = addr;

	while (stack) {
		struct ao_scheme_stack	*prev;
		int			ret;
		(void) ao_scheme_poly_move(&stack->sexprs, 1);
		(void) ao_scheme_poly_move(&stack->values, 1);
		(void) ao_scheme_poly_move(&stack->values_tail, 0);
		(void) ao_scheme_poly_move(&stack->frame, 0);
		(void) ao_scheme_poly_move(&stack->list, 1);
		prev = ao_scheme_poly_stack(stack->prev);
		if (!prev)
			break;
		ret = ao_scheme_move_memory(&ao_scheme_stack_type, (void **) &prev);
		if (prev != ao_scheme_poly_stack(stack->prev))
			stack->prev = ao_scheme_stack_poly(prev);
		if (ret)
			break;
		stack = prev;
	}
}

const struct ao_scheme_type ao_scheme_stack_type = {
	.size = stack_size,
	.mark = stack_mark,
	.move = stack_move,
	.name = "stack"
};

struct ao_scheme_stack		*ao_scheme_stack_free_list;

void
ao_scheme_stack_reset(struct ao_scheme_stack *stack)
{
	stack->state = eval_sexpr;
	stack->sexprs = AO_SCHEME_NIL;
	stack->values = AO_SCHEME_NIL;
	stack->values_tail = AO_SCHEME_NIL;
}

static struct ao_scheme_stack *
ao_scheme_stack_new(void)
{
	struct ao_scheme_stack *stack;

	if (ao_scheme_stack_free_list) {
		stack = ao_scheme_stack_free_list;
		ao_scheme_stack_free_list = ao_scheme_poly_stack(stack->prev);
	} else {
		stack = ao_scheme_alloc(sizeof (struct ao_scheme_stack));
		if (!stack)
			return 0;
		stack->type = AO_SCHEME_STACK;
	}
	ao_scheme_stack_reset(stack);
	return stack;
}

int
ao_scheme_stack_push(void)
{
	struct ao_scheme_stack	*stack;

	stack = ao_scheme_stack_new();

	if (!stack)
		return 0;

	stack->prev = ao_scheme_stack_poly(ao_scheme_stack);
	stack->frame = ao_scheme_frame_poly(ao_scheme_frame_current);
	stack->list = AO_SCHEME_NIL;

	ao_scheme_stack = stack;

	DBGI("stack push\n");
	DBG_FRAMES();
	DBG_IN();
	return 1;
}

void
ao_scheme_stack_pop(void)
{
	ao_poly			prev;
	struct ao_scheme_frame	*prev_frame;

	if (!ao_scheme_stack)
		return;
	prev = ao_scheme_stack->prev;
	if (!ao_scheme_stack_marked(ao_scheme_stack)) {
		ao_scheme_stack->prev = ao_scheme_stack_poly(ao_scheme_stack_free_list);
		ao_scheme_stack_free_list = ao_scheme_stack;
	}

	ao_scheme_stack = ao_scheme_poly_stack(prev);
	prev_frame = ao_scheme_frame_current;
	if (ao_scheme_stack)
		ao_scheme_frame_current = ao_scheme_poly_frame(ao_scheme_stack->frame);
	else
		ao_scheme_frame_current = NULL;
	if (ao_scheme_frame_current != prev_frame)
		ao_scheme_frame_free(prev_frame);
	DBG_OUT();
	DBGI("stack pop\n");
	DBG_FRAMES();
}

void
ao_scheme_stack_write(FILE *out, ao_poly poly, bool write)
{
	struct ao_scheme_stack 	*s = ao_scheme_poly_stack(poly);
	struct ao_scheme_stack	*clear = s;
	int			written = 0;

	(void) write;
	ao_scheme_print_start();
	ao_scheme_frame_print_indent += 2;
	while (s) {
		if (ao_scheme_print_mark_addr(s)) {
			fputs("[recurse...]", out);
			break;
		}
		written++;
		fputs("\t[\n", out);
		ao_scheme_fprintf(out, "\t\texpr:     %v\n", s->list);
		ao_scheme_fprintf(out, "\t\tvalues:   %v\n", s->values);
		ao_scheme_fprintf(out, "\t\tframe:    %v\n", s->frame);
		fputs("\t]\n", out);
		s = ao_scheme_poly_stack(s->prev);
	}
	ao_scheme_frame_print_indent -= 2;
	if (ao_scheme_print_stop()) {
		while (written--) {
			ao_scheme_print_clear_addr(clear);
			clear = ao_scheme_poly_stack(clear->prev);
		}
	}
}

/*
 * Copy a stack, being careful to keep everybody referenced
 */
static struct ao_scheme_stack *
ao_scheme_stack_copy(struct ao_scheme_stack *old)
{
	struct ao_scheme_stack *new = NULL;
	struct ao_scheme_stack *n, *prev = NULL;

	while (old) {
		ao_scheme_stack_stash(old);
		ao_scheme_stack_stash(new);
		ao_scheme_stack_stash(prev);
		n = ao_scheme_stack_new();
		prev = ao_scheme_stack_fetch();
		new = ao_scheme_stack_fetch();
		old = ao_scheme_stack_fetch();
		if (!n)
			return NULL;

		ao_scheme_stack_mark(old);
		ao_scheme_frame_mark(ao_scheme_poly_frame(old->frame));
		*n = *old;

		if (prev)
			prev->prev = ao_scheme_stack_poly(n);
		else
			new = n;
		prev = n;

		old = ao_scheme_poly_stack(old->prev);
	}
	return new;
}

/*
 * Evaluate a continuation invocation
 */
ao_poly
ao_scheme_stack_eval(void)
{
	struct ao_scheme_cons	*cons;
	struct ao_scheme_stack	*new = ao_scheme_stack_copy(ao_scheme_poly_stack(ao_scheme_v));
	if (!new)
		return AO_SCHEME_NIL;

	cons = ao_scheme_poly_cons(ao_scheme_stack->values);

	if (!cons || !cons->cdr)
		return ao_scheme_error(AO_SCHEME_INVALID, "continuation requires a value");

	new->state = eval_val;

	ao_scheme_stack = new;
	ao_scheme_frame_current = ao_scheme_poly_frame(ao_scheme_stack->frame);

	return ao_scheme_poly_cons(cons->cdr)->car;
}

/*
 * Call with current continuation. This calls a lambda, passing
 * it a single argument which is the current continuation
 */
ao_poly
ao_scheme_do_call_cc(struct ao_scheme_cons *cons)
{
	struct ao_scheme_stack	*new;
	ao_poly			v;

	if (!ao_scheme_parse_args(_ao_scheme_atom_call2fcc, cons,
				  AO_SCHEME_LAMBDA|AO_SCHEME_ARG_RET_POLY, &v,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;

	ao_scheme_poly_stash(v);
	/* Note that the whole call chain now has
	 * a reference to it which may escape
	 */
	new = ao_scheme_stack_copy(ao_scheme_stack);
	if (!new)
		return AO_SCHEME_NIL;
	v = ao_scheme_poly_fetch();

	/* re-fetch cons after the allocation */
	cons = ao_scheme_poly_cons(ao_scheme_poly_cons(ao_scheme_stack->values)->cdr);

	/* Reset the arg list to the current stack,
	 * and call the lambda
	 */

	cons->car = ao_scheme_stack_poly(new);
	cons->cdr = AO_SCHEME_NIL;

	ao_scheme_stack->state = eval_exec;
	return v;
}
