/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "ao_lisp.h"

int
lambda_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_lisp_lambda);
}

void
lambda_mark(void *addr)
{
	struct ao_lisp_lambda	*lambda = addr;

	ao_lisp_poly_mark(lambda->code, 0);
	ao_lisp_poly_mark(lambda->frame, 0);
}

void
lambda_move(void *addr)
{
	struct ao_lisp_lambda	*lambda = addr;

	ao_lisp_poly_move(&lambda->code, 0);
	ao_lisp_poly_move(&lambda->frame, 0);
}

const struct ao_lisp_type ao_lisp_lambda_type = {
	.size = lambda_size,
	.mark = lambda_mark,
	.move = lambda_move,
	.name = "lambda",
};

void
ao_lisp_lambda_print(ao_poly poly)
{
	struct ao_lisp_lambda	*lambda = ao_lisp_poly_lambda(poly);
	struct ao_lisp_cons	*cons = ao_lisp_poly_cons(lambda->code);

	printf("(");
	printf("%s", ao_lisp_args_name(lambda->args));
	while (cons) {
		printf(" ");
		ao_lisp_poly_print(cons->car);
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	printf(")");
}

ao_poly
ao_lisp_lambda_alloc(struct ao_lisp_cons *code, int args)
{
	ao_lisp_cons_stash(0, code);
	struct ao_lisp_lambda	*lambda = ao_lisp_alloc(sizeof (struct ao_lisp_lambda));
	code = ao_lisp_cons_fetch(0);
	struct ao_lisp_cons	*arg;
	int			f;

	if (!lambda)
		return AO_LISP_NIL;

	if (!ao_lisp_check_argt(_ao_lisp_atom_lambda, code, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	f = 0;
	arg = ao_lisp_poly_cons(ao_lisp_arg(code, 0));
	while (arg) {
		if (ao_lisp_poly_type(arg->car) != AO_LISP_ATOM)
			return ao_lisp_error(AO_LISP_INVALID, "formal %d is not an atom", f);
		arg = ao_lisp_poly_cons(arg->cdr);
		f++;
	}

	lambda->type = AO_LISP_LAMBDA;
	lambda->args = args;
	lambda->code = ao_lisp_cons_poly(code);
	lambda->frame = ao_lisp_frame_mark(ao_lisp_frame_current);
	DBGI("build frame: "); DBG_POLY(lambda->frame); DBG("\n");
	DBG_STACK();
	return ao_lisp_lambda_poly(lambda);
}

ao_poly
ao_lisp_lambda(struct ao_lisp_cons *cons)
{
	return ao_lisp_lambda_alloc(cons, AO_LISP_FUNC_LAMBDA);
}

ao_poly
ao_lisp_lexpr(struct ao_lisp_cons *cons)
{
	return ao_lisp_lambda_alloc(cons, AO_LISP_FUNC_LEXPR);
}

ao_poly
ao_lisp_nlambda(struct ao_lisp_cons *cons)
{
	return ao_lisp_lambda_alloc(cons, AO_LISP_FUNC_NLAMBDA);
}

ao_poly
ao_lisp_macro(struct ao_lisp_cons *cons)
{
	return ao_lisp_lambda_alloc(cons, AO_LISP_FUNC_MACRO);
}

ao_poly
ao_lisp_lambda_eval(void)
{
	struct ao_lisp_lambda	*lambda = ao_lisp_poly_lambda(ao_lisp_v);
	struct ao_lisp_cons	*cons = ao_lisp_poly_cons(ao_lisp_stack->values);
	struct ao_lisp_cons	*code = ao_lisp_poly_cons(lambda->code);
	struct ao_lisp_cons	*args = ao_lisp_poly_cons(ao_lisp_arg(code, 0));
	struct ao_lisp_frame	*next_frame;
	int			args_wanted;
	int			args_provided;
	int			f;
	struct ao_lisp_cons	*vals;

	DBGI("lambda "); DBG_POLY(ao_lisp_lambda_poly(lambda)); DBG("\n");

	args_wanted = ao_lisp_cons_length(args);

	/* Create a frame to hold the variables
	 */
	args_provided = ao_lisp_cons_length(cons) - 1;
	if (lambda->args == AO_LISP_FUNC_LAMBDA) {
		if (args_wanted != args_provided)
			return ao_lisp_error(AO_LISP_INVALID, "need %d args, got %d", args_wanted, args_provided);
	} else {
		if (args_provided < args_wanted - 1)
			return ao_lisp_error(AO_LISP_INVALID, "need at least %d args, got %d", args_wanted, args_provided);
	}

	next_frame = ao_lisp_frame_new(args_wanted);

	/* Re-fetch all of the values in case something moved */
	lambda = ao_lisp_poly_lambda(ao_lisp_v);
	cons = ao_lisp_poly_cons(ao_lisp_stack->values);
	code = ao_lisp_poly_cons(lambda->code);
	args = ao_lisp_poly_cons(ao_lisp_arg(code, 0));
	vals = ao_lisp_poly_cons(cons->cdr);

	next_frame->prev = lambda->frame;
	ao_lisp_frame_current = next_frame;
	ao_lisp_stack->frame = ao_lisp_frame_poly(ao_lisp_frame_current);

	switch (lambda->args) {
	case AO_LISP_FUNC_LAMBDA:
		for (f = 0; f < args_wanted; f++) {
			DBGI("bind "); DBG_POLY(args->car); DBG(" = "); DBG_POLY(vals->car); DBG("\n");
			next_frame->vals[f].atom = args->car;
			next_frame->vals[f].val = vals->car;
			args = ao_lisp_poly_cons(args->cdr);
			vals = ao_lisp_poly_cons(vals->cdr);
		}
		if (!ao_lisp_stack_marked(ao_lisp_stack))
			ao_lisp_cons_free(cons);
		cons = NULL;
		break;
	case AO_LISP_FUNC_LEXPR:
	case AO_LISP_FUNC_NLAMBDA:
	case AO_LISP_FUNC_MACRO:
		for (f = 0; f < args_wanted - 1; f++) {
			DBGI("bind "); DBG_POLY(args->car); DBG(" = "); DBG_POLY(vals->car); DBG("\n");
			next_frame->vals[f].atom = args->car;
			next_frame->vals[f].val = vals->car;
			args = ao_lisp_poly_cons(args->cdr);
			vals = ao_lisp_poly_cons(vals->cdr);
		}
		DBGI("bind "); DBG_POLY(args->car); DBG(" = "); DBG_POLY(ao_lisp_cons_poly(vals)); DBG("\n");
		next_frame->vals[f].atom = args->car;
		next_frame->vals[f].val = ao_lisp_cons_poly(vals);
		break;
	default:
		break;
	}
	DBGI("eval frame: "); DBG_POLY(ao_lisp_frame_poly(next_frame)); DBG("\n");
	DBG_STACK();
	DBGI("eval code: "); DBG_POLY(code->cdr); DBG("\n");
	return code->cdr;
}
