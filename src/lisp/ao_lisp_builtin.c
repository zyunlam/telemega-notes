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

void
ao_lisp_builtin_print(ao_poly b)
{
	(void) b;
	printf("[builtin]");
}

static int check_argc(struct ao_lisp_cons *cons, int min, int max)
{
	int	argc = 0;

	while (cons && argc <= max) {
		argc++;
		cons = ao_lisp_poly_cons(cons->cdr);
	}
	if (argc < min || argc > max) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return 0;
	}
	return 1;
}

static int check_argt(struct ao_lisp_cons *cons, int argc, int type, int nil_ok)
{
	ao_poly car;

	/* find the desired arg */
	while (argc--)
		cons = ao_lisp_poly_cons(cons->cdr);
	car = cons->car;
	if ((!car && !nil_ok) ||
	    ao_lisp_poly_type(car) != type)
	{
		ao_lisp_exception |= AO_LISP_INVALID;
		return 0;
	}
	return 1;
}

enum math_op { math_plus, math_minus, math_times, math_divide, math_mod };

ao_poly
ao_lisp_car(struct ao_lisp_cons *cons)
{
	if (!check_argc(cons, 1, 1))
		return AO_LISP_NIL;
	if (!check_argt(cons, 0, AO_LISP_CONS, 0)) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	return ao_lisp_poly_cons(cons->car)->car;
}

ao_poly
ao_lisp_cdr(struct ao_lisp_cons *cons)
{
	if (!cons) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	if (!cons->car) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	if (ao_lisp_poly_type(cons->car) != AO_LISP_CONS) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	return ao_lisp_poly_cons(cons->car)->cdr;
}

ao_poly
ao_lisp_cons(struct ao_lisp_cons *cons)
{
	ao_poly	car, cdr;
	if (!cons) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	car = cons->car;
	cdr = cons->cdr;
	if (!car || !cdr) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	cdr = ao_lisp_poly_cons(cdr)->car;
	if (ao_lisp_poly_type(cdr) != AO_LISP_CONS) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	return ao_lisp_cons_poly(ao_lisp_cons_cons(car, ao_lisp_poly_cons(cdr)));
}

ao_poly
ao_lisp_quote(struct ao_lisp_cons *cons)
{
	if (!cons) {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
	return cons->car;
}

ao_poly
ao_lisp_set(struct ao_lisp_cons *cons)
{
	if (!check_argc(cons, 2, 2))
		return AO_LISP_NIL;
	if (!check_argt(cons, 0, AO_LISP_ATOM, 0))
		return AO_LISP_NIL;

	return ao_lisp_atom_set(cons->car, ao_lisp_poly_cons(cons->cdr)->car);
}

ao_poly
ao_lisp_setq(struct ao_lisp_cons *cons)
{
	struct ao_lisp_cons	*expand = 0;
	if (!check_argc(cons, 2, 2))
		return AO_LISP_NIL;
	expand = ao_lisp_cons_cons(_ao_lisp_atom_set,
				   ao_lisp_cons_cons(ao_lisp_cons_poly(ao_lisp_cons_cons(_ao_lisp_atom_quote,
								       ao_lisp_cons_cons(cons->car, NULL))),
						     ao_lisp_poly_cons(cons->cdr)));
	return ao_lisp_cons_poly(expand);
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
	return val;
}

ao_poly
ao_lisp_math(struct ao_lisp_cons *cons, enum math_op op)
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
			case math_plus:
				r += c;
				break;
			case math_minus:
				r -= c;
				break;
			case math_times:
				r *= c;
				break;
			case math_divide:
				if (c == 0) {
					ao_lisp_exception |= AO_LISP_DIVIDE_BY_ZERO;
					return AO_LISP_NIL;
				}
				r /= c;
				break;
			case math_mod:
				if (c == 0) {
					ao_lisp_exception |= AO_LISP_DIVIDE_BY_ZERO;
					return AO_LISP_NIL;
				}
				r %= c;
				break;
			}
			ret = ao_lisp_int_poly(r);
		}

		else if (rt == AO_LISP_STRING && ct == AO_LISP_STRING && op == math_plus)
			ret = ao_lisp_string_poly(ao_lisp_string_cat(ao_lisp_poly_string(ret),
								     ao_lisp_poly_string(car)));
		else {
			ao_lisp_exception |= AO_LISP_INVALID;
			return AO_LISP_NIL;
		}
	}
	return ret;
}

ao_poly
ao_lisp_plus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_plus);
}

ao_poly
ao_lisp_minus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_minus);
}

ao_poly
ao_lisp_times(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_times);
}

ao_poly
ao_lisp_divide(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_divide);
}

ao_poly
ao_lisp_mod(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_mod);
}

ao_lisp_func_t ao_lisp_builtins[] = {
	[builtin_car] = ao_lisp_car,
	[builtin_cdr] = ao_lisp_cdr,
	[builtin_cons] = ao_lisp_cons,
	[builtin_quote] = ao_lisp_quote,
	[builtin_set] = ao_lisp_set,
	[builtin_setq] = ao_lisp_setq,
	[builtin_print] = ao_lisp_print,
	[builtin_plus] = ao_lisp_plus,
	[builtin_minus] = ao_lisp_minus,
	[builtin_times] = ao_lisp_times,
	[builtin_divide] = ao_lisp_divide,
	[builtin_mod] = ao_lisp_mod
};

