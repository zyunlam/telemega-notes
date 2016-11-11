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

static int
builtin_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_lisp_builtin);
}

static void
builtin_mark(void *addr)
{
	(void) addr;
}

static void
builtin_move(void *addr)
{
	(void) addr;
}

const struct ao_lisp_type ao_lisp_builtin_type = {
	.size = builtin_size,
	.mark = builtin_mark,
	.move = builtin_move
};

#ifdef AO_LISP_MAKE_CONST
char *ao_lisp_builtin_name(enum ao_lisp_builtin_id b) {
	return "???";
}
char *ao_lisp_args_name(uint8_t args) {
	return "???";
}
#else
static const ao_poly builtin_names[] = {
	[builtin_eval] = _ao_lisp_atom_eval,
	[builtin_read] = _ao_lisp_atom_read,
	[builtin_lambda] = _ao_lisp_atom_lambda,
	[builtin_lexpr] = _ao_lisp_atom_lexpr,
	[builtin_nlambda] = _ao_lisp_atom_nlambda,
	[builtin_macro] = _ao_lisp_atom_macro,
	[builtin_car] = _ao_lisp_atom_car,
	[builtin_cdr] = _ao_lisp_atom_cdr,
	[builtin_cons] = _ao_lisp_atom_cons,
	[builtin_last] = _ao_lisp_atom_last,
	[builtin_length] = _ao_lisp_atom_length,
	[builtin_quote] = _ao_lisp_atom_quote,
	[builtin_set] = _ao_lisp_atom_set,
	[builtin_setq] = _ao_lisp_atom_setq,
	[builtin_cond] = _ao_lisp_atom_cond,
	[builtin_progn] = _ao_lisp_atom_progn,
	[builtin_while] = _ao_lisp_atom_while,
	[builtin_print] = _ao_lisp_atom_print,
	[builtin_patom] = _ao_lisp_atom_patom,
	[builtin_plus] = _ao_lisp_atom_2b,
	[builtin_minus] = _ao_lisp_atom_2d,
	[builtin_times] = _ao_lisp_atom_2a,
	[builtin_divide] = _ao_lisp_atom_2f,
	[builtin_mod] = _ao_lisp_atom_25,
	[builtin_equal] = _ao_lisp_atom_3d,
	[builtin_less] = _ao_lisp_atom_3c,
	[builtin_greater] = _ao_lisp_atom_3e,
	[builtin_less_equal] = _ao_lisp_atom_3c3d,
	[builtin_greater_equal] = _ao_lisp_atom_3e3d,
	[builtin_pack] = _ao_lisp_atom_pack,
	[builtin_unpack] = _ao_lisp_atom_unpack,
	[builtin_flush] = _ao_lisp_atom_flush,
	[builtin_delay] = _ao_lisp_atom_delay,
	[builtin_led] = _ao_lisp_atom_led,
};

static char *
ao_lisp_builtin_name(enum ao_lisp_builtin_id b) {
	if (b < _builtin_last)
		return ao_lisp_poly_atom(builtin_names[b])->name;
	return "???";
}

static const ao_poly ao_lisp_args_atoms[] = {
	[AO_LISP_FUNC_LAMBDA] = _ao_lisp_atom_lambda,
	[AO_LISP_FUNC_LEXPR] = _ao_lisp_atom_lexpr,
	[AO_LISP_FUNC_NLAMBDA] = _ao_lisp_atom_nlambda,
	[AO_LISP_FUNC_MACRO] = _ao_lisp_atom_macro,
};

char *
ao_lisp_args_name(uint8_t args)
{
	if (args < sizeof ao_lisp_args_atoms / sizeof ao_lisp_args_atoms[0])
		return ao_lisp_poly_atom(ao_lisp_args_atoms[args])->name;
	return "(unknown)";
}
#endif

void
ao_lisp_builtin_print(ao_poly b)
{
	struct ao_lisp_builtin *builtin = ao_lisp_poly_builtin(b);
	printf("[builtin %s %s]",
	       ao_lisp_args_name(builtin->args),
	       ao_lisp_builtin_name(builtin->func));
}

ao_poly
ao_lisp_check_argc(ao_poly name, struct ao_lisp_cons *cons, int min, int max)
{
	int	argc = 0;

	while (cons && argc <= max) {
		argc++;
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	if (argc < min || argc > max)
		return ao_lisp_error(AO_LISP_INVALID, "%s: invalid arg count", ao_lisp_poly_atom(name)->name);
	return _ao_lisp_atom_t;
}

ao_poly
ao_lisp_arg(struct ao_lisp_cons *cons, int argc)
{
	if (!cons)
		return AO_LISP_NIL;
	while (argc--) {
		if (!cons)
			return AO_LISP_NIL;
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	return cons->car;
}

ao_poly
ao_lisp_check_argt(ao_poly name, struct ao_lisp_cons *cons, int argc, int type, int nil_ok)
{
	ao_poly car = ao_lisp_arg(cons, argc);

	if ((!car && !nil_ok) || ao_lisp_poly_type(car) != type)
		return ao_lisp_error(AO_LISP_INVALID, "%s: invalid type for arg %d", ao_lisp_poly_atom(name)->name, argc);
	return _ao_lisp_atom_t;
}

ao_poly
ao_lisp_car(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_car, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_car, cons, 0, AO_LISP_CONS, 0))
		return AO_LISP_NIL;
	return ao_lisp_poly_cons(cons->car)->car;
}

ao_poly
ao_lisp_cdr(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_cdr, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_cdr, cons, 0, AO_LISP_CONS, 0))
		return AO_LISP_NIL;
	return ao_lisp_poly_cons(cons->car)->cdr;
}

ao_poly
ao_lisp_cons(struct ao_lisp_cons *cons)
{
	ao_poly	car, cdr;
	if(!ao_lisp_check_argc(_ao_lisp_atom_cons, cons, 2, 2))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_cons, cons, 1, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	car = ao_lisp_arg(cons, 0);
	cdr = ao_lisp_arg(cons, 1);
	return ao_lisp_cons_poly(ao_lisp_cons_cons(car, ao_lisp_poly_cons(cdr)));
}

ao_poly
ao_lisp_last(struct ao_lisp_cons *cons)
{
	ao_poly	l;
	if (!ao_lisp_check_argc(_ao_lisp_atom_last, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_last, cons, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	l = ao_lisp_arg(cons, 0);
	while (l) {
		struct ao_lisp_cons *list = ao_lisp_poly_cons(l);
		if (!list->cdr)
			return list->car;
		l = list->cdr;
	}
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_length(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_last, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_last, cons, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	return ao_lisp_int_poly(ao_lisp_cons_length(ao_lisp_poly_cons(ao_lisp_arg(cons, 0))));
}

ao_poly
ao_lisp_quote(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_quote, cons, 1, 1))
		return AO_LISP_NIL;
	return ao_lisp_arg(cons, 0);
}

ao_poly
ao_lisp_set(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_set, cons, 2, 2))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_set, cons, 0, AO_LISP_ATOM, 0))
		return AO_LISP_NIL;

	return ao_lisp_atom_set(ao_lisp_arg(cons, 0), ao_lisp_arg(cons, 1));
}

ao_poly
ao_lisp_setq(struct ao_lisp_cons *cons)
{
	struct ao_lisp_cons	*expand = 0;
	if (!ao_lisp_check_argc(_ao_lisp_atom_setq, cons, 2, 2))
		return AO_LISP_NIL;
	expand = ao_lisp_cons_cons(_ao_lisp_atom_set,
				   ao_lisp_cons_cons(ao_lisp_cons_poly(ao_lisp_cons_cons(_ao_lisp_atom_quote,
								       ao_lisp_cons_cons(cons->car, NULL))),
						     ao_lisp_poly_cons(cons->cdr)));
	return ao_lisp_cons_poly(expand);
}

ao_poly
ao_lisp_cond(struct ao_lisp_cons *cons)
{
	ao_lisp_set_cond(cons);
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_progn(struct ao_lisp_cons *cons)
{
	ao_lisp_stack->state = eval_progn;
	ao_lisp_stack->sexprs = ao_lisp_cons_poly(cons);
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_while(struct ao_lisp_cons *cons)
{
	ao_lisp_stack->state = eval_while;
	ao_lisp_stack->sexprs = ao_lisp_cons_poly(cons);
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_print(struct ao_lisp_cons *cons)
{
	ao_poly	val = AO_LISP_NIL;
	while (cons) {
		val = cons->car;
		ao_lisp_poly_print(val);
		cons = ao_lisp_poly_cons(cons->cdr);
		if (cons)
			printf(" ");
	}
	printf("\n");
	return val;
}

ao_poly
ao_lisp_patom(struct ao_lisp_cons *cons)
{
	ao_poly	val = AO_LISP_NIL;
	while (cons) {
		val = cons->car;
		ao_lisp_poly_patom(val);
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	return val;
}

ao_poly
ao_lisp_math(struct ao_lisp_cons *cons, enum ao_lisp_builtin_id op)
{
	ao_poly	ret = AO_LISP_NIL;

	while (cons) {
		ao_poly		car = cons->car;
		uint8_t		rt = ao_lisp_poly_type(ret);
		uint8_t		ct = ao_lisp_poly_type(car);

		cons = ao_lisp_poly_cons(cons->cdr);

		if (rt == AO_LISP_NIL)
			ret = car;

		else if (rt == AO_LISP_INT && ct == AO_LISP_INT) {
			int	r = ao_lisp_poly_int(ret);
			int	c = ao_lisp_poly_int(car);

			switch(op) {
			case builtin_plus:
				r += c;
				break;
			case builtin_minus:
				r -= c;
				break;
			case builtin_times:
				r *= c;
				break;
			case builtin_divide:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "divide by zero");
				r /= c;
				break;
			case builtin_mod:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "mod by zero");
				r %= c;
				break;
			default:
				break;
			}
			ret = ao_lisp_int_poly(r);
		}

		else if (rt == AO_LISP_STRING && ct == AO_LISP_STRING && op == builtin_plus)
			ret = ao_lisp_string_poly(ao_lisp_string_cat(ao_lisp_poly_string(ret),
								     ao_lisp_poly_string(car)));
		else
			return ao_lisp_error(AO_LISP_INVALID, "invalid args");
	}
	return ret;
}

ao_poly
ao_lisp_plus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_plus);
}

ao_poly
ao_lisp_minus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_minus);
}

ao_poly
ao_lisp_times(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_times);
}

ao_poly
ao_lisp_divide(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_divide);
}

ao_poly
ao_lisp_mod(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_mod);
}

ao_poly
ao_lisp_compare(struct ao_lisp_cons *cons, enum ao_lisp_builtin_id op)
{
	ao_poly	left;

	if (!cons)
		return _ao_lisp_atom_t;

	left = cons->car;
	cons = ao_lisp_poly_cons(cons->cdr);
	while (cons) {
		ao_poly	right = cons->car;

		if (op == builtin_equal) {
			if (left != right)
				return AO_LISP_NIL;
		} else {
			uint8_t	lt = ao_lisp_poly_type(left);
			uint8_t	rt = ao_lisp_poly_type(right);
			if (lt == AO_LISP_INT && rt == AO_LISP_INT) {
				int l = ao_lisp_poly_int(left);
				int r = ao_lisp_poly_int(right);

				switch (op) {
				case builtin_less:
					if (!(l < r))
						return AO_LISP_NIL;
					break;
				case builtin_greater:
					if (!(l > r))
						return AO_LISP_NIL;
					break;
				case builtin_less_equal:
					if (!(l <= r))
						return AO_LISP_NIL;
					break;
				case builtin_greater_equal:
					if (!(l >= r))
						return AO_LISP_NIL;
					break;
				default:
					break;
				}
			} else if (lt == AO_LISP_STRING && rt == AO_LISP_STRING) {
				int c = strcmp(ao_lisp_poly_string(left),
					       ao_lisp_poly_string(right));
				switch (op) {
				case builtin_less:
					if (!(c < 0))
						return AO_LISP_NIL;
					break;
				case builtin_greater:
					if (!(c > 0))
						return AO_LISP_NIL;
					break;
				case builtin_less_equal:
					if (!(c <= 0))
						return AO_LISP_NIL;
					break;
				case builtin_greater_equal:
					if (!(c >= 0))
						return AO_LISP_NIL;
					break;
				default:
					break;
				}
			}
		}
		left = right;
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	return _ao_lisp_atom_t;
}

ao_poly
ao_lisp_equal(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_equal);
}

ao_poly
ao_lisp_less(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_less);
}

ao_poly
ao_lisp_greater(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_greater);
}

ao_poly
ao_lisp_less_equal(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_less_equal);
}

ao_poly
ao_lisp_greater_equal(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_greater_equal);
}

ao_poly
ao_lisp_pack(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_pack, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_pack, cons, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	return ao_lisp_string_pack(ao_lisp_poly_cons(ao_lisp_arg(cons, 0)));
}

ao_poly
ao_lisp_unpack(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_unpack, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_unpack, cons, 0, AO_LISP_STRING, 0))
		return AO_LISP_NIL;
	return ao_lisp_string_unpack(ao_lisp_poly_string(ao_lisp_arg(cons, 0)));
}

ao_poly
ao_lisp_flush(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_flush, cons, 0, 0))
		return AO_LISP_NIL;
	ao_lisp_os_flush();
	return _ao_lisp_atom_t;
}

ao_poly
ao_lisp_led(struct ao_lisp_cons *cons)
{
	ao_poly led;
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_INT, 0))
		return AO_LISP_NIL;
	led = ao_lisp_arg(cons, 0);
	ao_lisp_os_led(ao_lisp_poly_int(led));
	return led;
}

ao_poly
ao_lisp_delay(struct ao_lisp_cons *cons)
{
	ao_poly delay;
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_INT, 0))
		return AO_LISP_NIL;
	delay = ao_lisp_arg(cons, 0);
	ao_lisp_os_delay(ao_lisp_poly_int(delay));
	return delay;
}

ao_poly
ao_lisp_do_eval(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_eval, cons, 1, 1))
		return AO_LISP_NIL;
	ao_lisp_stack->state = eval_sexpr;
	return cons->car;
}

ao_poly
ao_lisp_do_read(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_read, cons, 0, 0))
		return AO_LISP_NIL;
	return ao_lisp_read();
}

const ao_lisp_func_t ao_lisp_builtins[] = {
	[builtin_eval] = ao_lisp_do_eval,
	[builtin_read] = ao_lisp_do_read,
	[builtin_lambda] = ao_lisp_lambda,
	[builtin_lexpr] = ao_lisp_lexpr,
	[builtin_nlambda] = ao_lisp_nlambda,
	[builtin_macro] = ao_lisp_macro,
	[builtin_car] = ao_lisp_car,
	[builtin_cdr] = ao_lisp_cdr,
	[builtin_cons] = ao_lisp_cons,
	[builtin_last] = ao_lisp_last,
	[builtin_length] = ao_lisp_length,
	[builtin_quote] = ao_lisp_quote,
	[builtin_set] = ao_lisp_set,
	[builtin_setq] = ao_lisp_setq,
	[builtin_cond] = ao_lisp_cond,
	[builtin_progn] = ao_lisp_progn,
	[builtin_while] = ao_lisp_while,
	[builtin_print] = ao_lisp_print,
	[builtin_patom] = ao_lisp_patom,
	[builtin_plus] = ao_lisp_plus,
	[builtin_minus] = ao_lisp_minus,
	[builtin_times] = ao_lisp_times,
	[builtin_divide] = ao_lisp_divide,
	[builtin_mod] = ao_lisp_mod,
	[builtin_equal] = ao_lisp_equal,
	[builtin_less] = ao_lisp_less,
	[builtin_greater] = ao_lisp_greater,
	[builtin_less_equal] = ao_lisp_less_equal,
	[builtin_greater_equal] = ao_lisp_greater_equal,
	[builtin_pack] = ao_lisp_pack,
	[builtin_unpack] = ao_lisp_unpack,
	[builtin_flush] = ao_lisp_flush,
	[builtin_led] = ao_lisp_led,
	[builtin_delay] = ao_lisp_delay,
};

