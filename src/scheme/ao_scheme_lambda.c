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

#include "ao_scheme.h"

static int
lambda_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_scheme_lambda);
}

static void
lambda_mark(void *addr)
{
	struct ao_scheme_lambda	*lambda = addr;

	ao_scheme_poly_mark(lambda->code, 0);
	ao_scheme_poly_mark(lambda->frame, 0);
}

static void
lambda_move(void *addr)
{
	struct ao_scheme_lambda	*lambda = addr;

	ao_scheme_poly_move(&lambda->code, 0);
	ao_scheme_poly_move(&lambda->frame, 0);
}

const struct ao_scheme_type ao_scheme_lambda_type = {
	.size = lambda_size,
	.mark = lambda_mark,
	.move = lambda_move,
	.name = "lambda",
};

void
ao_scheme_lambda_write(ao_poly poly, bool write)
{
	struct ao_scheme_lambda	*lambda = ao_scheme_poly_lambda(poly);
	struct ao_scheme_cons	*cons = ao_scheme_poly_cons(lambda->code);

	printf("(");
	printf("%s", ao_scheme_args_name(lambda->args));
	while (cons) {
		printf(" ");
		ao_scheme_poly_write(cons->car, write);
		cons = ao_scheme_poly_cons(cons->cdr);
	}
	printf(")");
}

static ao_poly
ao_scheme_lambda_alloc(struct ao_scheme_cons *code, int args)
{
	struct ao_scheme_lambda	*lambda;
	ao_poly			formal;
	struct ao_scheme_cons	*cons;

	formal = ao_scheme_arg(code, 0);
	while (formal != AO_SCHEME_NIL) {
		switch (ao_scheme_poly_type(formal)) {
		case AO_SCHEME_CONS:
			cons = ao_scheme_poly_cons(formal);
			if (ao_scheme_poly_type(cons->car) != AO_SCHEME_ATOM)
				return ao_scheme_error(AO_SCHEME_INVALID, "formal %p is not atom", cons->car);
			formal = cons->cdr;
			break;
		case AO_SCHEME_ATOM:
			formal = AO_SCHEME_NIL;
			break;
		default:
			return ao_scheme_error(AO_SCHEME_INVALID, "formal %p is not atom", formal);
		}
	}

	ao_scheme_cons_stash(code);
	lambda = ao_scheme_alloc(sizeof (struct ao_scheme_lambda));
	code = ao_scheme_cons_fetch();
	if (!lambda)
		return AO_SCHEME_NIL;

	lambda->type = AO_SCHEME_LAMBDA;
	lambda->args = args;
	lambda->code = ao_scheme_cons_poly(code);
	lambda->frame = ao_scheme_frame_mark(ao_scheme_frame_current);
	DBGI("build frame: "); DBG_POLY(lambda->frame); DBG("\n");
	DBG_STACK();
	return ao_scheme_lambda_poly(lambda);
}

ao_poly
ao_scheme_do_lambda(struct ao_scheme_cons *cons)
{
	return ao_scheme_lambda_alloc(cons, AO_SCHEME_FUNC_LAMBDA);
}

ao_poly
ao_scheme_do_nlambda(struct ao_scheme_cons *cons)
{
	return ao_scheme_lambda_alloc(cons, AO_SCHEME_FUNC_NLAMBDA);
}

ao_poly
ao_scheme_do_macro(struct ao_scheme_cons *cons)
{
	return ao_scheme_lambda_alloc(cons, AO_SCHEME_FUNC_MACRO);
}

ao_poly
ao_scheme_lambda_eval(void)
{
	struct ao_scheme_lambda	*lambda = ao_scheme_poly_lambda(ao_scheme_v);
	struct ao_scheme_cons	*cons = ao_scheme_poly_cons(ao_scheme_stack->values);
	struct ao_scheme_cons	*code = ao_scheme_poly_cons(lambda->code);
	ao_poly			formals;
	struct ao_scheme_frame	*next_frame;
	int			args_wanted;
	ao_poly			varargs = AO_SCHEME_NIL;
	int			args_provided;
	int			f;
	struct ao_scheme_cons	*vals;

	DBGI("lambda "); DBG_POLY(ao_scheme_lambda_poly(lambda)); DBG("\n");

	args_wanted = 0;
	for (formals = ao_scheme_arg(code, 0);
	     ao_scheme_is_pair(formals);
	     formals = ao_scheme_poly_cons(formals)->cdr)
		++args_wanted;
	if (formals != AO_SCHEME_NIL) {
		if (ao_scheme_poly_type(formals) != AO_SCHEME_ATOM)
			return ao_scheme_error(AO_SCHEME_INVALID, "bad lambda form");
		varargs = formals;
	}

	/* Create a frame to hold the variables
	 */
	args_provided = ao_scheme_cons_length(cons) - 1;
	if (varargs == AO_SCHEME_NIL) {
		if (args_wanted != args_provided)
			return ao_scheme_error(AO_SCHEME_INVALID, "need %d args, got %d", args_wanted, args_provided);
	} else {
		if (args_provided < args_wanted)
			return ao_scheme_error(AO_SCHEME_INVALID, "need at least %d args, got %d", args_wanted, args_provided);
	}

	ao_scheme_poly_stash(varargs);
	next_frame = ao_scheme_frame_new(args_wanted + (varargs != AO_SCHEME_NIL));
	varargs = ao_scheme_poly_fetch();
	if (!next_frame)
		return AO_SCHEME_NIL;

	/* Re-fetch all of the values in case something moved */
	lambda = ao_scheme_poly_lambda(ao_scheme_v);
	cons = ao_scheme_poly_cons(ao_scheme_stack->values);
	code = ao_scheme_poly_cons(lambda->code);
	formals = ao_scheme_arg(code, 0);
	vals = ao_scheme_poly_cons(cons->cdr);

	next_frame->prev = lambda->frame;
	ao_scheme_frame_current = next_frame;
	ao_scheme_stack->frame = ao_scheme_frame_poly(ao_scheme_frame_current);

	for (f = 0; f < args_wanted; f++) {
		struct ao_scheme_cons *arg = ao_scheme_poly_cons(formals);
		DBGI("bind "); DBG_POLY(arg->car); DBG(" = "); DBG_POLY(vals->car); DBG("\n");
		ao_scheme_frame_bind(next_frame, f, arg->car, vals->car);
		formals = arg->cdr;
		vals = ao_scheme_poly_cons(vals->cdr);
	}
	if (varargs) {
		DBGI("bind "); DBG_POLY(varargs); DBG(" = "); DBG_POLY(ao_scheme_cons_poly(vals)); DBG("\n");
		/*
		 * Bind the rest of the arguments to the final parameter
		 */
		ao_scheme_frame_bind(next_frame, f, varargs, ao_scheme_cons_poly(vals));
	} else {
		/*
		 * Mark the cons cells from the actuals as freed for immediate re-use, unless
		 * the actuals point into the source function (nlambdas and macros), or if the
		 * stack containing them was copied as a part of a continuation
		 */
		if (lambda->args == AO_SCHEME_FUNC_LAMBDA && !ao_scheme_stack_marked(ao_scheme_stack)) {
			ao_scheme_stack->values = AO_SCHEME_NIL;
			ao_scheme_cons_free(cons);
		}
	}
	DBGI("eval frame: "); DBG_POLY(ao_scheme_frame_poly(next_frame)); DBG("\n");
	DBG_STACK();
	DBGI("eval code: "); DBG_POLY(code->cdr); DBG("\n");
	return code->cdr;
}
