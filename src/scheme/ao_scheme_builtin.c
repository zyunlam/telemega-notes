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
#include <limits.h>
#include <math.h>

static int
builtin_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_scheme_builtin);
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

const struct ao_scheme_type ao_scheme_builtin_type = {
	.size = builtin_size,
	.mark = builtin_mark,
	.move = builtin_move
};

#ifdef AO_SCHEME_MAKE_CONST

#define AO_SCHEME_BUILTIN_CASENAME
#include "ao_scheme_builtin.h"

char *ao_scheme_args_name(uint8_t args) {
	args &= AO_SCHEME_FUNC_MASK;
	switch (args) {
	case AO_SCHEME_FUNC_LAMBDA: return ao_scheme_poly_atom(_ao_scheme_atom_lambda)->name;
	case AO_SCHEME_FUNC_NLAMBDA: return ao_scheme_poly_atom(_ao_scheme_atom_nlambda)->name;
	case AO_SCHEME_FUNC_MACRO: return ao_scheme_poly_atom(_ao_scheme_atom_macro)->name;
	default: return "???";
	}
}
#else

#define AO_SCHEME_BUILTIN_ARRAYNAME
#include "ao_scheme_builtin.h"

static char *
ao_scheme_builtin_name(enum ao_scheme_builtin_id b) {
	if (b < _builtin_last)
		return ao_scheme_poly_atom(builtin_names[b])->name;
	return "???";
}

static const ao_poly ao_scheme_args_atoms[] = {
	[AO_SCHEME_FUNC_LAMBDA] = _ao_scheme_atom_lambda,
	[AO_SCHEME_FUNC_NLAMBDA] = _ao_scheme_atom_nlambda,
	[AO_SCHEME_FUNC_MACRO] = _ao_scheme_atom_macro,
};

char *
ao_scheme_args_name(uint8_t args)
{
	args &= AO_SCHEME_FUNC_MASK;
	if (args < sizeof ao_scheme_args_atoms / sizeof ao_scheme_args_atoms[0])
		return ao_scheme_poly_atom(ao_scheme_args_atoms[args])->name;
	return "(unknown)";
}
#endif

void
ao_scheme_builtin_write(ao_poly b)
{
	struct ao_scheme_builtin *builtin = ao_scheme_poly_builtin(b);
	printf("%s", ao_scheme_builtin_name(builtin->func));
}

ao_poly
ao_scheme_check_argc(ao_poly name, struct ao_scheme_cons *cons, int min, int max)
{
	int	argc = 0;

	while (cons && argc <= max) {
		argc++;
		cons = ao_scheme_cons_cdr(cons);
	}
	if (argc < min || argc > max)
		return ao_scheme_error(AO_SCHEME_INVALID, "%s: invalid arg count", ao_scheme_poly_atom(name)->name);
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_arg(struct ao_scheme_cons *cons, int argc)
{
	if (!cons)
		return AO_SCHEME_NIL;
	while (argc--) {
		if (!cons)
			return AO_SCHEME_NIL;
		cons = ao_scheme_cons_cdr(cons);
	}
	return cons->car;
}

ao_poly
ao_scheme_check_argt(ao_poly name, struct ao_scheme_cons *cons, int argc, int type, int nil_ok)
{
	ao_poly car = ao_scheme_arg(cons, argc);

	if ((!car && !nil_ok) || ao_scheme_poly_type(car) != type)
		return ao_scheme_error(AO_SCHEME_INVALID, "%s: arg %d invalid type %v", ao_scheme_poly_atom(name)->name, argc, car);
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_car(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_car, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_car, cons, 0, AO_SCHEME_CONS, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_poly_cons(cons->car)->car;
}

ao_poly
ao_scheme_do_cdr(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_cdr, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_cdr, cons, 0, AO_SCHEME_CONS, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_poly_cons(cons->car)->cdr;
}

ao_poly
ao_scheme_do_cons(struct ao_scheme_cons *cons)
{
	ao_poly	car, cdr;
	if(!ao_scheme_check_argc(_ao_scheme_atom_cons, cons, 2, 2))
		return AO_SCHEME_NIL;
	car = ao_scheme_arg(cons, 0);
	cdr = ao_scheme_arg(cons, 1);
	return ao_scheme__cons(car, cdr);
}

ao_poly
ao_scheme_do_last(struct ao_scheme_cons *cons)
{
	struct ao_scheme_cons	*list;
	if (!ao_scheme_check_argc(_ao_scheme_atom_last, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_last, cons, 0, AO_SCHEME_CONS, 1))
		return AO_SCHEME_NIL;
	for (list = ao_scheme_poly_cons(ao_scheme_arg(cons, 0));
	     list;
	     list = ao_scheme_cons_cdr(list))
	{
		if (!list->cdr)
			return list->car;
	}
	return AO_SCHEME_NIL;
}

ao_poly
ao_scheme_do_length(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_length, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_length, cons, 0, AO_SCHEME_CONS, 1))
		return AO_SCHEME_NIL;
	return ao_scheme_int_poly(ao_scheme_cons_length(ao_scheme_poly_cons(ao_scheme_arg(cons, 0))));
}

ao_poly
ao_scheme_do_quote(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_quote, cons, 1, 1))
		return AO_SCHEME_NIL;
	return ao_scheme_arg(cons, 0);
}

ao_poly
ao_scheme_do_set(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_set, cons, 2, 2))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_set, cons, 0, AO_SCHEME_ATOM, 0))
		return AO_SCHEME_NIL;

	return ao_scheme_atom_set(ao_scheme_arg(cons, 0), ao_scheme_arg(cons, 1));
}

ao_poly
ao_scheme_do_def(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_def, cons, 2, 2))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_def, cons, 0, AO_SCHEME_ATOM, 0))
		return AO_SCHEME_NIL;

	return ao_scheme_atom_def(ao_scheme_arg(cons, 0), ao_scheme_arg(cons, 1));
}

ao_poly
ao_scheme_do_setq(struct ao_scheme_cons *cons)
{
	ao_poly	name;
	if (!ao_scheme_check_argc(_ao_scheme_atom_set21, cons, 2, 2))
		return AO_SCHEME_NIL;
	name = cons->car;
	if (ao_scheme_poly_type(name) != AO_SCHEME_ATOM)
		return ao_scheme_error(AO_SCHEME_INVALID, "set! of non-atom %v", name);
	if (!ao_scheme_atom_ref(name, NULL))
		return ao_scheme_error(AO_SCHEME_INVALID, "atom %v not defined", name);
	return ao_scheme__cons(_ao_scheme_atom_set,
			     ao_scheme__cons(ao_scheme__cons(_ao_scheme_atom_quote,
							 ao_scheme__cons(name, AO_SCHEME_NIL)),
					   cons->cdr));
}

ao_poly
ao_scheme_do_cond(struct ao_scheme_cons *cons)
{
	ao_scheme_set_cond(cons);
	return AO_SCHEME_NIL;
}

ao_poly
ao_scheme_do_begin(struct ao_scheme_cons *cons)
{
	ao_scheme_stack->state = eval_begin;
	ao_scheme_stack->sexprs = ao_scheme_cons_poly(cons);
	return AO_SCHEME_NIL;
}

ao_poly
ao_scheme_do_while(struct ao_scheme_cons *cons)
{
	ao_scheme_stack->state = eval_while;
	ao_scheme_stack->sexprs = ao_scheme_cons_poly(cons);
	return AO_SCHEME_NIL;
}

ao_poly
ao_scheme_do_write(struct ao_scheme_cons *cons)
{
	ao_poly	val = AO_SCHEME_NIL;
	while (cons) {
		val = cons->car;
		ao_scheme_poly_write(val);
		cons = ao_scheme_cons_cdr(cons);
		if (cons)
			printf(" ");
	}
	printf("\n");
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_display(struct ao_scheme_cons *cons)
{
	ao_poly	val = AO_SCHEME_NIL;
	while (cons) {
		val = cons->car;
		ao_scheme_poly_display(val);
		cons = ao_scheme_cons_cdr(cons);
	}
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_math(struct ao_scheme_cons *orig_cons, enum ao_scheme_builtin_id op)
{
	struct ao_scheme_cons *cons = cons;
	ao_poly	ret = AO_SCHEME_NIL;

	for (cons = orig_cons; cons; cons = ao_scheme_cons_cdr(cons)) {
		ao_poly		car = cons->car;
		uint8_t		rt = ao_scheme_poly_type(ret);
		uint8_t		ct = ao_scheme_poly_type(car);

		if (cons == orig_cons) {
			ret = car;
			if (cons->cdr == AO_SCHEME_NIL) {
				switch (op) {
				case builtin_minus:
					if (ao_scheme_integer_typep(ct))
						ret = ao_scheme_integer_poly(-ao_scheme_poly_integer(ret));
					else if (ct == AO_SCHEME_FLOAT)
						ret = ao_scheme_float_get(-ao_scheme_poly_number(ret));
					break;
				case builtin_divide:
					if (ao_scheme_integer_typep(ct) && ao_scheme_poly_integer(ret) == 1)
						;
					else if (ao_scheme_number_typep(ct)) {
						float	v = ao_scheme_poly_number(ret);
						ret = ao_scheme_float_get(1/v);
					}
					break;
				default:
					break;
				}
			}
		} else if (ao_scheme_integer_typep(rt) && ao_scheme_integer_typep(ct)) {
			int32_t	r = ao_scheme_poly_integer(ret);
			int32_t	c = ao_scheme_poly_integer(car);
			int64_t t;

			switch(op) {
			case builtin_plus:
				r += c;
			check_overflow:
				if (r < AO_SCHEME_MIN_BIGINT || AO_SCHEME_MAX_BIGINT < r)
					goto inexact;
				break;
			case builtin_minus:
				r -= c;
				goto check_overflow;
				break;
			case builtin_times:
				t = (int64_t) r * (int64_t) c;
				if (t < AO_SCHEME_MIN_BIGINT || AO_SCHEME_MAX_BIGINT < t)
					goto inexact;
				r = (int32_t) t;
				break;
			case builtin_divide:
				if (c != 0 && (r % c) == 0)
					r /= c;
				else
					goto inexact;
				break;
			case builtin_quotient:
				if (c == 0)
					return ao_scheme_error(AO_SCHEME_DIVIDE_BY_ZERO, "quotient by zero");
				if (r % c != 0 && (c < 0) != (r < 0))
					r = r / c - 1;
				else
					r = r / c;
				break;
			case builtin_remainder:
				if (c == 0)
					return ao_scheme_error(AO_SCHEME_DIVIDE_BY_ZERO, "remainder by zero");
				r %= c;
				break;
			case builtin_modulo:
				if (c == 0)
					return ao_scheme_error(AO_SCHEME_DIVIDE_BY_ZERO, "modulo by zero");
				r %= c;
				if ((r < 0) != (c < 0))
					r += c;
				break;
			default:
				break;
			}
			ret = ao_scheme_integer_poly(r);
		} else if (ao_scheme_number_typep(rt) && ao_scheme_number_typep(ct)) {
			float r, c;
		inexact:
			r = ao_scheme_poly_number(ret);
			c = ao_scheme_poly_number(car);
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
				r /= c;
				break;
			case builtin_quotient:
			case builtin_remainder:
			case builtin_modulo:
				return ao_scheme_error(AO_SCHEME_INVALID, "non-integer value in integer divide");
			default:
				break;
			}
			ret = ao_scheme_float_get(r);
		}

		else if (rt == AO_SCHEME_STRING && ct == AO_SCHEME_STRING && op == builtin_plus)
			ret = ao_scheme_string_poly(ao_scheme_string_cat(ao_scheme_poly_string(ret),
								     ao_scheme_poly_string(car)));
		else
			return ao_scheme_error(AO_SCHEME_INVALID, "invalid args");
	}
	return ret;
}

ao_poly
ao_scheme_do_plus(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_plus);
}

ao_poly
ao_scheme_do_minus(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_minus);
}

ao_poly
ao_scheme_do_times(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_times);
}

ao_poly
ao_scheme_do_divide(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_divide);
}

ao_poly
ao_scheme_do_quotient(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_quotient);
}

ao_poly
ao_scheme_do_modulo(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_modulo);
}

ao_poly
ao_scheme_do_remainder(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_remainder);
}

ao_poly
ao_scheme_compare(struct ao_scheme_cons *cons, enum ao_scheme_builtin_id op)
{
	ao_poly	left;

	if (!cons)
		return _ao_scheme_bool_true;

	left = cons->car;
	for (cons = ao_scheme_cons_cdr(cons); cons; cons = ao_scheme_cons_cdr(cons)) {
		ao_poly	right = cons->car;

		if (op == builtin_equal) {
			if (left != right)
				return _ao_scheme_bool_false;
		} else {
			uint8_t	lt = ao_scheme_poly_type(left);
			uint8_t	rt = ao_scheme_poly_type(right);
			if (ao_scheme_integer_typep(lt) && ao_scheme_integer_typep(rt)) {
				int32_t l = ao_scheme_poly_integer(left);
				int32_t r = ao_scheme_poly_integer(right);

				switch (op) {
				case builtin_less:
					if (!(l < r))
						return _ao_scheme_bool_false;
					break;
				case builtin_greater:
					if (!(l > r))
						return _ao_scheme_bool_false;
					break;
				case builtin_less_equal:
					if (!(l <= r))
						return _ao_scheme_bool_false;
					break;
				case builtin_greater_equal:
					if (!(l >= r))
						return _ao_scheme_bool_false;
					break;
				default:
					break;
				}
			} else if (lt == AO_SCHEME_STRING && rt == AO_SCHEME_STRING) {
				int c = strcmp(ao_scheme_poly_string(left),
					       ao_scheme_poly_string(right));
				switch (op) {
				case builtin_less:
					if (!(c < 0))
						return _ao_scheme_bool_false;
					break;
				case builtin_greater:
					if (!(c > 0))
						return _ao_scheme_bool_false;
					break;
				case builtin_less_equal:
					if (!(c <= 0))
						return _ao_scheme_bool_false;
					break;
				case builtin_greater_equal:
					if (!(c >= 0))
						return _ao_scheme_bool_false;
					break;
				default:
					break;
				}
			}
		}
		left = right;
	}
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_equal(struct ao_scheme_cons *cons)
{
	return ao_scheme_compare(cons, builtin_equal);
}

ao_poly
ao_scheme_do_less(struct ao_scheme_cons *cons)
{
	return ao_scheme_compare(cons, builtin_less);
}

ao_poly
ao_scheme_do_greater(struct ao_scheme_cons *cons)
{
	return ao_scheme_compare(cons, builtin_greater);
}

ao_poly
ao_scheme_do_less_equal(struct ao_scheme_cons *cons)
{
	return ao_scheme_compare(cons, builtin_less_equal);
}

ao_poly
ao_scheme_do_greater_equal(struct ao_scheme_cons *cons)
{
	return ao_scheme_compare(cons, builtin_greater_equal);
}

ao_poly
ao_scheme_do_list_to_string(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_list2d3estring, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_list2d3estring, cons, 0, AO_SCHEME_CONS, 1))
		return AO_SCHEME_NIL;
	return ao_scheme_string_pack(ao_scheme_poly_cons(ao_scheme_arg(cons, 0)));
}

ao_poly
ao_scheme_do_string_to_list(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_string2d3elist, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_string2d3elist, cons, 0, AO_SCHEME_STRING, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_string_unpack(ao_scheme_poly_string(ao_scheme_arg(cons, 0)));
}

ao_poly
ao_scheme_do_flush_output(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_flush2doutput, cons, 0, 0))
		return AO_SCHEME_NIL;
	ao_scheme_os_flush();
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_led(struct ao_scheme_cons *cons)
{
	ao_poly led;
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_INT, 0))
		return AO_SCHEME_NIL;
	led = ao_scheme_arg(cons, 0);
	ao_scheme_os_led(ao_scheme_poly_int(led));
	return led;
}

ao_poly
ao_scheme_do_delay(struct ao_scheme_cons *cons)
{
	ao_poly delay;
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_INT, 0))
		return AO_SCHEME_NIL;
	delay = ao_scheme_arg(cons, 0);
	ao_scheme_os_delay(ao_scheme_poly_int(delay));
	return delay;
}

ao_poly
ao_scheme_do_eval(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_eval, cons, 1, 1))
		return AO_SCHEME_NIL;
	ao_scheme_stack->state = eval_sexpr;
	return cons->car;
}

ao_poly
ao_scheme_do_apply(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_apply, cons, 2, INT_MAX))
		return AO_SCHEME_NIL;
	ao_scheme_stack->state = eval_apply;
	return ao_scheme_cons_poly(cons);
}

ao_poly
ao_scheme_do_read(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_read, cons, 0, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_read();
}

ao_poly
ao_scheme_do_collect(struct ao_scheme_cons *cons)
{
	int	free;
	(void) cons;
	free = ao_scheme_collect(AO_SCHEME_COLLECT_FULL);
	return ao_scheme_integer_poly(free);
}

ao_poly
ao_scheme_do_nullp(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (ao_scheme_arg(cons, 0) == AO_SCHEME_NIL)
		return _ao_scheme_bool_true;
	else
		return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_not(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (ao_scheme_arg(cons, 0) == _ao_scheme_bool_false)
		return _ao_scheme_bool_true;
	else
		return _ao_scheme_bool_false;
}

static ao_poly
ao_scheme_do_typep(int type, struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (ao_scheme_poly_type(ao_scheme_arg(cons, 0)) == type)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_pairp(struct ao_scheme_cons *cons)
{
	ao_poly	v;
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	v = ao_scheme_arg(cons, 0);
	if (v != AO_SCHEME_NIL && ao_scheme_poly_type(v) == AO_SCHEME_CONS)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_integerp(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(ao_scheme_arg(cons, 0))) {
	case AO_SCHEME_INT:
	case AO_SCHEME_BIGINT:
		return _ao_scheme_bool_true;
	default:
		return _ao_scheme_bool_false;
	}
}

ao_poly
ao_scheme_do_numberp(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(ao_scheme_arg(cons, 0))) {
	case AO_SCHEME_INT:
	case AO_SCHEME_BIGINT:
	case AO_SCHEME_FLOAT:
		return _ao_scheme_bool_true;
	default:
		return _ao_scheme_bool_false;
	}
}

ao_poly
ao_scheme_do_stringp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(AO_SCHEME_STRING, cons);
}

ao_poly
ao_scheme_do_symbolp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(AO_SCHEME_ATOM, cons);
}

ao_poly
ao_scheme_do_booleanp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(AO_SCHEME_BOOL, cons);
}

ao_poly
ao_scheme_do_procedurep(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(ao_scheme_arg(cons, 0))) {
	case AO_SCHEME_BUILTIN:
	case AO_SCHEME_LAMBDA:
		return _ao_scheme_bool_true;
	default:
	return _ao_scheme_bool_false;
	}
}

/* This one is special -- a list is either nil or
 * a 'proper' list with only cons cells
 */
ao_poly
ao_scheme_do_listp(struct ao_scheme_cons *cons)
{
	ao_poly	v;
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	v = ao_scheme_arg(cons, 0);
	for (;;) {
		if (v == AO_SCHEME_NIL)
			return _ao_scheme_bool_true;
		if (ao_scheme_poly_type(v) != AO_SCHEME_CONS)
			return _ao_scheme_bool_false;
		v = ao_scheme_poly_cons(v)->cdr;
	}
}

ao_poly
ao_scheme_do_set_car(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 2, 2))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_CONS, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_poly_cons(ao_scheme_arg(cons, 0))->car = ao_scheme_arg(cons, 1);
}

ao_poly
ao_scheme_do_set_cdr(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 2, 2))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_CONS, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_poly_cons(ao_scheme_arg(cons, 0))->cdr = ao_scheme_arg(cons, 1);
}

ao_poly
ao_scheme_do_symbol_to_string(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_ATOM, 0))
		return AO_SCHEME_NIL;
	return ao_scheme_string_poly(ao_scheme_string_copy(ao_scheme_poly_atom(ao_scheme_arg(cons, 0))->name));
}

ao_poly
ao_scheme_do_string_to_symbol(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_STRING, 0))
		return AO_SCHEME_NIL;

	return ao_scheme_atom_poly(ao_scheme_atom_intern(ao_scheme_poly_string(ao_scheme_arg(cons, 0))));
}

ao_poly
ao_scheme_do_read_char(struct ao_scheme_cons *cons)
{
	int	c;
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 0, 0))
		return AO_SCHEME_NIL;
	c = getchar();
	return ao_scheme_int_poly(c);
}

ao_poly
ao_scheme_do_write_char(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 1, 1))
		return AO_SCHEME_NIL;
	if (!ao_scheme_check_argt(_ao_scheme_atom_led, cons, 0, AO_SCHEME_INT, 0))
		return AO_SCHEME_NIL;
	putchar(ao_scheme_poly_integer(ao_scheme_arg(cons, 0)));
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_exit(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 0, 0))
		return AO_SCHEME_NIL;
	ao_scheme_exception |= AO_SCHEME_EXIT;
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_current_jiffy(struct ao_scheme_cons *cons)
{
	int	jiffy;

	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 0, 0))
		return AO_SCHEME_NIL;
	jiffy = ao_scheme_os_jiffy();
	return (ao_scheme_int_poly(jiffy));
}

ao_poly
ao_scheme_do_current_second(struct ao_scheme_cons *cons)
{
	int	second;

	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 0, 0))
		return AO_SCHEME_NIL;
	second = ao_scheme_os_jiffy() / AO_SCHEME_JIFFIES_PER_SECOND;
	return (ao_scheme_int_poly(second));
}

ao_poly
ao_scheme_do_jiffies_per_second(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_check_argc(_ao_scheme_atom_led, cons, 0, 0))
		return AO_SCHEME_NIL;
	return (ao_scheme_int_poly(AO_SCHEME_JIFFIES_PER_SECOND));
}

#define AO_SCHEME_BUILTIN_FUNCS
#include "ao_scheme_builtin.h"
