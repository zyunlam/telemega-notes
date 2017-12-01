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
#include <limits.h>
#include <math.h>

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

#define AO_LISP_BUILTIN_CASENAME
#include "ao_lisp_builtin.h"

char *ao_lisp_args_name(uint8_t args) {
	args &= AO_LISP_FUNC_MASK;
	switch (args) {
	case AO_LISP_FUNC_LAMBDA: return ao_lisp_poly_atom(_ao_lisp_atom_lambda)->name;
	case AO_LISP_FUNC_LEXPR: return ao_lisp_poly_atom(_ao_lisp_atom_lexpr)->name;
	case AO_LISP_FUNC_NLAMBDA: return ao_lisp_poly_atom(_ao_lisp_atom_nlambda)->name;
	case AO_LISP_FUNC_MACRO: return ao_lisp_poly_atom(_ao_lisp_atom_macro)->name;
	default: return "???";
	}
}
#else

#define AO_LISP_BUILTIN_ARRAYNAME
#include "ao_lisp_builtin.h"

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
	args &= AO_LISP_FUNC_MASK;
	if (args < sizeof ao_lisp_args_atoms / sizeof ao_lisp_args_atoms[0])
		return ao_lisp_poly_atom(ao_lisp_args_atoms[args])->name;
	return "(unknown)";
}
#endif

void
ao_lisp_builtin_write(ao_poly b)
{
	struct ao_lisp_builtin *builtin = ao_lisp_poly_builtin(b);
	printf("%s", ao_lisp_builtin_name(builtin->func));
}

ao_poly
ao_lisp_check_argc(ao_poly name, struct ao_lisp_cons *cons, int min, int max)
{
	int	argc = 0;

	while (cons && argc <= max) {
		argc++;
		cons = ao_lisp_cons_cdr(cons);
	}
	if (argc < min || argc > max)
		return ao_lisp_error(AO_LISP_INVALID, "%s: invalid arg count", ao_lisp_poly_atom(name)->name);
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_arg(struct ao_lisp_cons *cons, int argc)
{
	if (!cons)
		return AO_LISP_NIL;
	while (argc--) {
		if (!cons)
			return AO_LISP_NIL;
		cons = ao_lisp_cons_cdr(cons);
	}
	return cons->car;
}

ao_poly
ao_lisp_check_argt(ao_poly name, struct ao_lisp_cons *cons, int argc, int type, int nil_ok)
{
	ao_poly car = ao_lisp_arg(cons, argc);

	if ((!car && !nil_ok) || ao_lisp_poly_type(car) != type)
		return ao_lisp_error(AO_LISP_INVALID, "%s: invalid type for arg %d", ao_lisp_poly_atom(name)->name, argc);
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_do_car(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_car, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_car, cons, 0, AO_LISP_CONS, 0))
		return AO_LISP_NIL;
	return ao_lisp_poly_cons(cons->car)->car;
}

ao_poly
ao_lisp_do_cdr(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_cdr, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_cdr, cons, 0, AO_LISP_CONS, 0))
		return AO_LISP_NIL;
	return ao_lisp_poly_cons(cons->car)->cdr;
}

ao_poly
ao_lisp_do_cons(struct ao_lisp_cons *cons)
{
	ao_poly	car, cdr;
	if(!ao_lisp_check_argc(_ao_lisp_atom_cons, cons, 2, 2))
		return AO_LISP_NIL;
	car = ao_lisp_arg(cons, 0);
	cdr = ao_lisp_arg(cons, 1);
	return ao_lisp__cons(car, cdr);
}

ao_poly
ao_lisp_do_last(struct ao_lisp_cons *cons)
{
	struct ao_lisp_cons	*list;
	if (!ao_lisp_check_argc(_ao_lisp_atom_last, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_last, cons, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	for (list = ao_lisp_poly_cons(ao_lisp_arg(cons, 0));
	     list;
	     list = ao_lisp_cons_cdr(list))
	{
		if (!list->cdr)
			return list->car;
	}
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_do_length(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_length, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_length, cons, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	return ao_lisp_int_poly(ao_lisp_cons_length(ao_lisp_poly_cons(ao_lisp_arg(cons, 0))));
}

ao_poly
ao_lisp_do_quote(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_quote, cons, 1, 1))
		return AO_LISP_NIL;
	return ao_lisp_arg(cons, 0);
}

ao_poly
ao_lisp_do_set(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_set, cons, 2, 2))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_set, cons, 0, AO_LISP_ATOM, 0))
		return AO_LISP_NIL;

	return ao_lisp_atom_set(ao_lisp_arg(cons, 0), ao_lisp_arg(cons, 1));
}

ao_poly
ao_lisp_do_def(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_def, cons, 2, 2))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_def, cons, 0, AO_LISP_ATOM, 0))
		return AO_LISP_NIL;

	return ao_lisp_atom_def(ao_lisp_arg(cons, 0), ao_lisp_arg(cons, 1));
}

ao_poly
ao_lisp_do_setq(struct ao_lisp_cons *cons)
{
	ao_poly	name;
	if (!ao_lisp_check_argc(_ao_lisp_atom_set21, cons, 2, 2))
		return AO_LISP_NIL;
	name = cons->car;
	if (ao_lisp_poly_type(name) != AO_LISP_ATOM)
		return ao_lisp_error(AO_LISP_INVALID, "set! of non-atom");
	if (!ao_lisp_atom_ref(name))
		return ao_lisp_error(AO_LISP_INVALID, "atom not defined");
	return ao_lisp__cons(_ao_lisp_atom_set,
			     ao_lisp__cons(ao_lisp__cons(_ao_lisp_atom_quote,
							 ao_lisp__cons(name, AO_LISP_NIL)),
					   cons->cdr));
}

ao_poly
ao_lisp_do_cond(struct ao_lisp_cons *cons)
{
	ao_lisp_set_cond(cons);
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_do_begin(struct ao_lisp_cons *cons)
{
	ao_lisp_stack->state = eval_begin;
	ao_lisp_stack->sexprs = ao_lisp_cons_poly(cons);
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_do_while(struct ao_lisp_cons *cons)
{
	ao_lisp_stack->state = eval_while;
	ao_lisp_stack->sexprs = ao_lisp_cons_poly(cons);
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_do_write(struct ao_lisp_cons *cons)
{
	ao_poly	val = AO_LISP_NIL;
	while (cons) {
		val = cons->car;
		ao_lisp_poly_write(val);
		cons = ao_lisp_cons_cdr(cons);
		if (cons)
			printf(" ");
	}
	printf("\n");
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_do_display(struct ao_lisp_cons *cons)
{
	ao_poly	val = AO_LISP_NIL;
	while (cons) {
		val = cons->car;
		ao_lisp_poly_display(val);
		cons = ao_lisp_cons_cdr(cons);
	}
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_math(struct ao_lisp_cons *orig_cons, enum ao_lisp_builtin_id op)
{
	struct ao_lisp_cons *cons = cons;
	ao_poly	ret = AO_LISP_NIL;

	for (cons = orig_cons; cons; cons = ao_lisp_cons_cdr(cons)) {
		ao_poly		car = cons->car;
		uint8_t		rt = ao_lisp_poly_type(ret);
		uint8_t		ct = ao_lisp_poly_type(car);

		if (cons == orig_cons) {
			ret = car;
			if (cons->cdr == AO_LISP_NIL) {
				switch (op) {
				case builtin_minus:
					if (ao_lisp_integer_typep(ct))
						ret = ao_lisp_integer_poly(-ao_lisp_poly_integer(ret));
					else if (ct == AO_LISP_FLOAT)
						ret = ao_lisp_float_get(-ao_lisp_poly_number(ret));
					break;
				case builtin_divide:
					if (ao_lisp_integer_typep(ct) && ao_lisp_poly_integer(ret) == 1)
						;
					else if (ao_lisp_number_typep(ct)) {
						float	v = ao_lisp_poly_number(ret);
						ret = ao_lisp_float_get(1/v);
					}
					break;
				default:
					break;
				}
			}
		} else if (ao_lisp_integer_typep(rt) && ao_lisp_integer_typep(ct)) {
			int32_t	r = ao_lisp_poly_integer(ret);
			int32_t	c = ao_lisp_poly_integer(car);

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
				if (c != 0 && (r % c) == 0)
					r /= c;
				else {
					ret = ao_lisp_float_get((float) r / (float) c);
					continue;
				}
				break;
			case builtin_quotient:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "quotient by zero");
				if (r % c != 0 && (c < 0) != (r < 0))
					r = r / c - 1;
				else
					r = r / c;
				break;
			case builtin_remainder:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "remainder by zero");
				r %= c;
				break;
			case builtin_modulo:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "modulo by zero");
				r %= c;
				if ((r < 0) != (c < 0))
					r += c;
				break;
			default:
				break;
			}
			ret = ao_lisp_integer_poly(r);
		} else if (ao_lisp_number_typep(rt) && ao_lisp_number_typep(ct)) {
			float r = ao_lisp_poly_number(ret);
			float c = ao_lisp_poly_number(car);
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
#if 0
			case builtin_quotient:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "quotient by zero");
				if (r % c != 0 && (c < 0) != (r < 0))
					r = r / c - 1;
				else
					r = r / c;
				break;
			case builtin_remainder:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "remainder by zero");
				r %= c;
				break;
			case builtin_modulo:
				if (c == 0)
					return ao_lisp_error(AO_LISP_DIVIDE_BY_ZERO, "modulo by zero");
				r %= c;
				if ((r < 0) != (c < 0))
					r += c;
				break;
#endif
			default:
				break;
			}
			ret = ao_lisp_float_get(r);
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
ao_lisp_do_plus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_plus);
}

ao_poly
ao_lisp_do_minus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_minus);
}

ao_poly
ao_lisp_do_times(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_times);
}

ao_poly
ao_lisp_do_divide(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_divide);
}

ao_poly
ao_lisp_do_quotient(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_quotient);
}

ao_poly
ao_lisp_do_modulo(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_modulo);
}

ao_poly
ao_lisp_do_remainder(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, builtin_remainder);
}

ao_poly
ao_lisp_compare(struct ao_lisp_cons *cons, enum ao_lisp_builtin_id op)
{
	ao_poly	left;

	if (!cons)
		return _ao_lisp_bool_true;

	left = cons->car;
	for (cons = ao_lisp_cons_cdr(cons); cons; cons = ao_lisp_cons_cdr(cons)) {
		ao_poly	right = cons->car;

		if (op == builtin_equal) {
			if (left != right)
				return _ao_lisp_bool_false;
		} else {
			uint8_t	lt = ao_lisp_poly_type(left);
			uint8_t	rt = ao_lisp_poly_type(right);
			if (ao_lisp_integer_typep(lt) && ao_lisp_integer_typep(rt)) {
				int32_t l = ao_lisp_poly_integer(left);
				int32_t r = ao_lisp_poly_integer(right);

				switch (op) {
				case builtin_less:
					if (!(l < r))
						return _ao_lisp_bool_false;
					break;
				case builtin_greater:
					if (!(l > r))
						return _ao_lisp_bool_false;
					break;
				case builtin_less_equal:
					if (!(l <= r))
						return _ao_lisp_bool_false;
					break;
				case builtin_greater_equal:
					if (!(l >= r))
						return _ao_lisp_bool_false;
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
						return _ao_lisp_bool_false;
					break;
				case builtin_greater:
					if (!(c > 0))
						return _ao_lisp_bool_false;
					break;
				case builtin_less_equal:
					if (!(c <= 0))
						return _ao_lisp_bool_false;
					break;
				case builtin_greater_equal:
					if (!(c >= 0))
						return _ao_lisp_bool_false;
					break;
				default:
					break;
				}
			}
		}
		left = right;
	}
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_do_equal(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_equal);
}

ao_poly
ao_lisp_do_less(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_less);
}

ao_poly
ao_lisp_do_greater(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_greater);
}

ao_poly
ao_lisp_do_less_equal(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_less_equal);
}

ao_poly
ao_lisp_do_greater_equal(struct ao_lisp_cons *cons)
{
	return ao_lisp_compare(cons, builtin_greater_equal);
}

ao_poly
ao_lisp_do_list_to_string(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_list2d3estring, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_list2d3estring, cons, 0, AO_LISP_CONS, 1))
		return AO_LISP_NIL;
	return ao_lisp_string_pack(ao_lisp_poly_cons(ao_lisp_arg(cons, 0)));
}

ao_poly
ao_lisp_do_string_to_list(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_string2d3elist, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_string2d3elist, cons, 0, AO_LISP_STRING, 0))
		return AO_LISP_NIL;
	return ao_lisp_string_unpack(ao_lisp_poly_string(ao_lisp_arg(cons, 0)));
}

ao_poly
ao_lisp_do_flush_output(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_flush2doutput, cons, 0, 0))
		return AO_LISP_NIL;
	ao_lisp_os_flush();
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_do_led(struct ao_lisp_cons *cons)
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
ao_lisp_do_delay(struct ao_lisp_cons *cons)
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
ao_lisp_do_apply(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_apply, cons, 2, INT_MAX))
		return AO_LISP_NIL;
	ao_lisp_stack->state = eval_apply;
	return ao_lisp_cons_poly(cons);
}

ao_poly
ao_lisp_do_read(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_read, cons, 0, 0))
		return AO_LISP_NIL;
	return ao_lisp_read();
}

ao_poly
ao_lisp_do_collect(struct ao_lisp_cons *cons)
{
	int	free;
	(void) cons;
	free = ao_lisp_collect(AO_LISP_COLLECT_FULL);
	return ao_lisp_int_poly(free);
}

ao_poly
ao_lisp_do_nullp(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (ao_lisp_arg(cons, 0) == AO_LISP_NIL)
		return _ao_lisp_bool_true;
	else
		return _ao_lisp_bool_false;
}

ao_poly
ao_lisp_do_not(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (ao_lisp_arg(cons, 0) == _ao_lisp_bool_false)
		return _ao_lisp_bool_true;
	else
		return _ao_lisp_bool_false;
}

static ao_poly
ao_lisp_do_typep(int type, struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (ao_lisp_poly_type(ao_lisp_arg(cons, 0)) == type)
		return _ao_lisp_bool_true;
	return _ao_lisp_bool_false;
}

ao_poly
ao_lisp_do_pairp(struct ao_lisp_cons *cons)
{
	ao_poly	v;
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	v = ao_lisp_arg(cons, 0);
	if (v != AO_LISP_NIL && ao_lisp_poly_type(v) == AO_LISP_CONS)
		return _ao_lisp_bool_true;
	return _ao_lisp_bool_false;
}

ao_poly
ao_lisp_do_integerp(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	switch (ao_lisp_poly_type(ao_lisp_arg(cons, 0))) {
	case AO_LISP_INT:
	case AO_LISP_BIGINT:
		return _ao_lisp_bool_true;
	default:
		return _ao_lisp_bool_false;
	}
}

ao_poly
ao_lisp_do_numberp(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	switch (ao_lisp_poly_type(ao_lisp_arg(cons, 0))) {
	case AO_LISP_INT:
	case AO_LISP_BIGINT:
	case AO_LISP_FLOAT:
		return _ao_lisp_bool_true;
	default:
		return _ao_lisp_bool_false;
	}
}

ao_poly
ao_lisp_do_stringp(struct ao_lisp_cons *cons)
{
	return ao_lisp_do_typep(AO_LISP_STRING, cons);
}

ao_poly
ao_lisp_do_symbolp(struct ao_lisp_cons *cons)
{
	return ao_lisp_do_typep(AO_LISP_ATOM, cons);
}

ao_poly
ao_lisp_do_booleanp(struct ao_lisp_cons *cons)
{
	return ao_lisp_do_typep(AO_LISP_BOOL, cons);
}

ao_poly
ao_lisp_do_procedurep(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	switch (ao_lisp_poly_type(ao_lisp_arg(cons, 0))) {
	case AO_LISP_BUILTIN:
	case AO_LISP_LAMBDA:
		return _ao_lisp_bool_true;
	default:
	return _ao_lisp_bool_false;
	}
}

/* This one is special -- a list is either nil or
 * a 'proper' list with only cons cells
 */
ao_poly
ao_lisp_do_listp(struct ao_lisp_cons *cons)
{
	ao_poly	v;
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	v = ao_lisp_arg(cons, 0);
	for (;;) {
		if (v == AO_LISP_NIL)
			return _ao_lisp_bool_true;
		if (ao_lisp_poly_type(v) != AO_LISP_CONS)
			return _ao_lisp_bool_false;
		v = ao_lisp_poly_cons(v)->cdr;
	}
}

ao_poly
ao_lisp_do_set_car(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 2, 2))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_CONS, 0))
		return AO_LISP_NIL;
	return ao_lisp_poly_cons(ao_lisp_arg(cons, 0))->car = ao_lisp_arg(cons, 1);
}

ao_poly
ao_lisp_do_set_cdr(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 2, 2))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_CONS, 0))
		return AO_LISP_NIL;
	return ao_lisp_poly_cons(ao_lisp_arg(cons, 0))->cdr = ao_lisp_arg(cons, 1);
}

ao_poly
ao_lisp_do_symbol_to_string(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_ATOM, 0))
		return AO_LISP_NIL;
	return ao_lisp_string_poly(ao_lisp_string_copy(ao_lisp_poly_atom(ao_lisp_arg(cons, 0))->name));
}

ao_poly
ao_lisp_do_string_to_symbol(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_STRING, 0))
		return AO_LISP_NIL;

	return ao_lisp_atom_poly(ao_lisp_atom_intern(ao_lisp_poly_string(ao_lisp_arg(cons, 0))));
}

ao_poly
ao_lisp_do_read_char(struct ao_lisp_cons *cons)
{
	int	c;
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 0, 0))
		return AO_LISP_NIL;
	c = getchar();
	return ao_lisp_int_poly(c);
}

ao_poly
ao_lisp_do_write_char(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 1, 1))
		return AO_LISP_NIL;
	if (!ao_lisp_check_argt(_ao_lisp_atom_led, cons, 0, AO_LISP_INT, 0))
		return AO_LISP_NIL;
	putchar(ao_lisp_poly_integer(ao_lisp_arg(cons, 0)));
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_do_exit(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 0, 0))
		return AO_LISP_NIL;
	ao_lisp_exception |= AO_LISP_EXIT;
	return _ao_lisp_bool_true;
}

ao_poly
ao_lisp_do_current_jiffy(struct ao_lisp_cons *cons)
{
	int	jiffy;

	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 0, 0))
		return AO_LISP_NIL;
	jiffy = ao_lisp_os_jiffy();
	return (ao_lisp_int_poly(jiffy));
}

ao_poly
ao_lisp_do_current_second(struct ao_lisp_cons *cons)
{
	int	second;

	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 0, 0))
		return AO_LISP_NIL;
	second = ao_lisp_os_jiffy() / AO_LISP_JIFFIES_PER_SECOND;
	return (ao_lisp_int_poly(second));
}

ao_poly
ao_lisp_do_jiffies_per_second(struct ao_lisp_cons *cons)
{
	if (!ao_lisp_check_argc(_ao_lisp_atom_led, cons, 0, 0))
		return AO_LISP_NIL;
	return (ao_lisp_int_poly(AO_LISP_JIFFIES_PER_SECOND));
}

#define AO_LISP_BUILTIN_FUNCS
#include "ao_lisp_builtin.h"
