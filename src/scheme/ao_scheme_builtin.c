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

#define _GNU_SOURCE
#include "ao_scheme.h"
#include <limits.h>
#include <math.h>
#include <stdarg.h>

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
	default: return (char *) "???";
	}
}
#else

#define AO_SCHEME_BUILTIN_ARRAYNAME
#include "ao_scheme_builtin.h"

static char *
ao_scheme_builtin_name(enum ao_scheme_builtin_id b) {
	if (b < _builtin_last)
		return ao_scheme_poly_atom(builtin_names[b])->name;
	return (char *) "???";
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
	return (char *) "(unknown)";
}
#endif

void
ao_scheme_builtin_write(FILE *out, ao_poly b, bool write)
{
	struct ao_scheme_builtin *builtin = ao_scheme_poly_builtin(b);
	(void) write;
	fputs(ao_scheme_builtin_name(builtin->func), out);
}

static bool
ao_scheme_typecheck(ao_poly actual, int formal_type) {
	int	actual_type;

	if ((formal_type & AO_SCHEME_ARG_MASK) == AO_SCHEME_POLY)
		return true;

	/* allow nil? */
	if (actual == AO_SCHEME_NIL)
		return (formal_type & AO_SCHEME_ARG_NIL_OK) != 0;

	actual_type = ao_scheme_poly_type(actual);
	formal_type &= AO_SCHEME_ARG_MASK;

	if (actual_type == formal_type)
		return true;
	if (actual_type == AO_SCHEME_BUILTIN && formal_type == AO_SCHEME_LAMBDA)
		return true;

#ifdef AO_SCHEME_FEATURE_BIGINT
	if (ao_scheme_integer_typep(actual_type) && formal_type == AO_SCHEME_INT)
		return true;
#endif
#ifdef AO_SCHEME_FEATURE_FLOAT
	if (ao_scheme_number_typep(actual_type) && formal_type == AO_SCHEME_FLOAT)
		return true;
#endif
	return false;
}

int
ao_scheme_parse_args(ao_poly name, struct ao_scheme_cons *cons, ...)
{
	va_list	ap;
	int formal;
	int argc = 0;
	ao_poly car;

	va_start(ap, cons);
	while ((formal = va_arg(ap, int)) != AO_SCHEME_ARG_END) {
		if (formal & AO_SCHEME_ARG_OPTIONAL)
			car = (ao_poly) va_arg(ap, int);
		if (cons) {
			car = cons->car;
			cons = ao_scheme_cons_cdr(cons);
			if (!ao_scheme_typecheck(car, formal)) {
				ao_scheme_error(AO_SCHEME_INVALID, "%v: arg %d invalid type %v", name, argc, car);
				return 0;
			}
		} else if (!(formal & AO_SCHEME_ARG_OPTIONAL)) {
			goto bad_args;
		}
		if (formal & AO_SCHEME_ARG_RET_POLY)
			formal = AO_SCHEME_POLY;

		switch (formal & AO_SCHEME_ARG_MASK) {
		case AO_SCHEME_INT:
#ifdef AO_SCHEME_FEATURE_BIGINT
		case AO_SCHEME_BIGINT:
#endif
			*(va_arg(ap, int32_t *)) = ao_scheme_poly_integer(car);
			break;
#ifdef AO_SCHEME_FEATURE_FLOAT
		case AO_SCHEME_FLOAT:
			*(va_arg(ap, float *)) = ao_scheme_poly_number(car);
			break;
#endif
		case AO_SCHEME_POLY:
			*(va_arg(ap, ao_poly *)) = car;
			break;
		default:
			*(va_arg(ap, void **)) = ao_scheme_ref(car);
			break;
		}
		argc++;
	}
	if (cons) {
	bad_args:
		ao_scheme_error(AO_SCHEME_INVALID, "%v: invalid arg count", name);
		return 0;
	}
	return 1;
}

ao_poly
ao_scheme_arg(struct ao_scheme_cons *cons, int argc)
{
	for (;;) {
		if (!cons)
			return AO_SCHEME_NIL;
		if (argc == 0)
			return cons->car;
		cons = ao_scheme_cons_cdr(cons);
		argc--;
	}
}

ao_poly
ao_scheme_do_quote(struct ao_scheme_cons *cons)
{
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_quote, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return val;
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

static ao_poly
ao_scheme_do_display_or_write(ao_poly proc, struct ao_scheme_cons *cons, bool write)
{
#ifndef AO_SCHEME_FEATURE_PORT
	ao_poly	val;
	ao_poly	port;

	if (!ao_scheme_parse_args(proc, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_POLY | AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	ao_scheme_poly_write(stdout, val, write);
#else
	ao_poly			val;
	struct ao_scheme_port	*port;
	FILE			*file = stdout;

	if (!ao_scheme_parse_args(proc, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_PORT | AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (port) {
		file = port->file;
		if (!file)
			return _ao_scheme_bool_true;
	}
	ao_scheme_poly_write(file, val, write);
#endif
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_write(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_display_or_write(_ao_scheme_atom_write, cons, true);
}

ao_poly
ao_scheme_do_display(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_display_or_write(_ao_scheme_atom_display, cons, false);
}

static ao_poly
ao_scheme_math(struct ao_scheme_cons *orig_cons, enum ao_scheme_builtin_id op)
{
	struct ao_scheme_cons *cons;
	ao_poly	ret = AO_SCHEME_NIL;

	for (cons = orig_cons; cons; cons = ao_scheme_cons_cdr(cons)) {
		ao_poly		car = cons->car;
		uint8_t		rt = ao_scheme_poly_type(ret);
		uint8_t		ct = ao_scheme_poly_type(car);

		if (cons == orig_cons) {
			ret = car;
			ao_scheme_cons_stash(cons);
			if (cons->cdr == AO_SCHEME_NIL) {
				switch (op) {
				case builtin_minus:
					if (ao_scheme_integer_typep(ct))
						ret = ao_scheme_integer_poly(-ao_scheme_poly_integer(ret));
#ifdef AO_SCHEME_FEATURE_FLOAT
					else if (ct == AO_SCHEME_FLOAT)
						ret = ao_scheme_float_get(-ao_scheme_poly_number(ret));
#endif
					break;
				case builtin_divide:
					if (ao_scheme_poly_integer(ret) == 1) {
					} else {
#ifdef AO_SCHEME_FEATURE_FLOAT
						if (ao_scheme_number_typep(ct)) {
							float	v = ao_scheme_poly_number(ret);
							ret = ao_scheme_float_get(1/v);
						}
#else
						ret = ao_scheme_integer_poly(0);
#endif
					}
					break;
				default:
					break;
				}
			}
			cons = ao_scheme_cons_fetch();
		} else if (ao_scheme_integer_typep(rt) && ao_scheme_integer_typep(ct)) {
			int32_t	r = ao_scheme_poly_integer(ret);
			int32_t	c = ao_scheme_poly_integer(car);
#ifdef AO_SCHEME_FEATURE_FLOAT
			int64_t t;
#endif

			switch(op) {
			case builtin_plus:
				r += c;
			check_overflow:
#ifdef AO_SCHEME_FEATURE_FLOAT
				if (r < AO_SCHEME_MIN_BIGINT || AO_SCHEME_MAX_BIGINT < r)
					goto inexact;
#endif
				break;
			case builtin_minus:
				r -= c;
				goto check_overflow;
				break;
			case builtin_times:
#ifdef AO_SCHEME_FEATURE_FLOAT
				t = (int64_t) r * (int64_t) c;
				if (t < AO_SCHEME_MIN_BIGINT || AO_SCHEME_MAX_BIGINT < t)
					goto inexact;
				r = (int32_t) t;
#else
				r = r * c;
#endif
				break;
			case builtin_divide:
#ifdef AO_SCHEME_FEATURE_FLOAT
				if (c != 0 && (r % c) == 0)
					r /= c;
				else
					goto inexact;
#else
				r /= c;
#endif
				break;
			case builtin_quotient:
				if (c == 0)
					return ao_scheme_error(AO_SCHEME_DIVIDE_BY_ZERO, "quotient by zero");
				r = r / c;
				break;
			case builtin_floor_quotient:
				if (c == 0)
					return ao_scheme_error(AO_SCHEME_DIVIDE_BY_ZERO, "floor-quotient by zero");
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
			ao_scheme_cons_stash(cons);
			ret = ao_scheme_integer_poly(r);
			cons = ao_scheme_cons_fetch();
#ifdef AO_SCHEME_FEATURE_FLOAT
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
			case builtin_floor_quotient:
			case builtin_remainder:
			case builtin_modulo:
				return ao_scheme_error(AO_SCHEME_INVALID, "non-integer value in integer divide");
			default:
				break;
			}
			ao_scheme_cons_stash(cons);
			ret = ao_scheme_float_get(r);
			cons = ao_scheme_cons_fetch();
#endif
		}
		else if (rt == AO_SCHEME_STRING && ct == AO_SCHEME_STRING && op == builtin_plus) {
			ao_scheme_cons_stash(cons);
			ret = ao_scheme_string_poly(ao_scheme_string_cat(ao_scheme_poly_string(ret),
									 ao_scheme_poly_string(car)));
			cons = ao_scheme_cons_fetch();
			if (!ret)
				return ret;
		}
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
ao_scheme_do_floor_quotient(struct ao_scheme_cons *cons)
{
	return ao_scheme_math(cons, builtin_floor_quotient);
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

static ao_poly
ao_scheme_compare(struct ao_scheme_cons *cons, enum ao_scheme_builtin_id op)
{
	ao_poly	left;

	if (!cons)
		return _ao_scheme_bool_true;

	left = cons->car;
	for (cons = ao_scheme_cons_cdr(cons); cons; cons = ao_scheme_cons_cdr(cons)) {
		ao_poly	right = cons->car;

		if (op == builtin_equal && left == right) {
			;
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
				case builtin_equal:
					if (!(l == r))
						return _ao_scheme_bool_false;
				default:
					break;
				}
#ifdef AO_SCHEME_FEATURE_FLOAT
			} else if (ao_scheme_number_typep(lt) && ao_scheme_number_typep(rt)) {
				float l, r;

				l = ao_scheme_poly_number(left);
				r = ao_scheme_poly_number(right);

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
				case builtin_equal:
					if (!(l == r))
						return _ao_scheme_bool_false;
				default:
					break;
				}
#endif /* AO_SCHEME_FEATURE_FLOAT */
			} else if (lt == AO_SCHEME_STRING && rt == AO_SCHEME_STRING) {
				int c = strcmp(ao_scheme_poly_string(left)->val,
					       ao_scheme_poly_string(right)->val);
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
				case builtin_equal:
					if (!(c == 0))
						return _ao_scheme_bool_false;
					break;
				default:
					break;
				}
			} else
				return _ao_scheme_bool_false;
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
ao_scheme_do_flush_output(struct ao_scheme_cons *cons)
{
#ifndef AO_SCHEME_FEATURE_PORT
	ao_poly	port;
	if (!ao_scheme_parse_args(_ao_scheme_atom_flush2doutput, cons,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	fflush(stdout);
#else
	struct ao_scheme_port	*port;

	if (!ao_scheme_parse_args(_ao_scheme_atom_flush2doutput, cons,
				  AO_SCHEME_PORT|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	fflush(stdout);
	if (port) {
		if (port->file)
			fflush(port->file);
	} else
		fflush(stdout);
#endif
	return _ao_scheme_bool_true;
}

#ifdef AO_SCHEME_FEATURE_GPIO

ao_poly
ao_scheme_do_led(struct ao_scheme_cons *cons)
{
	int32_t led;
	if (!ao_scheme_parse_args(_ao_scheme_atom_led, cons,
				  AO_SCHEME_INT, &led,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	ao_scheme_os_led(led);
	return _ao_scheme_bool_true;
}

#endif

ao_poly
ao_scheme_do_eval(struct ao_scheme_cons *cons)
{
	ao_poly	expr;
	ao_poly	env;

	if (!ao_scheme_parse_args(_ao_scheme_atom_eval, cons,
				  AO_SCHEME_POLY, &expr,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &env,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	ao_scheme_stack->state = eval_sexpr;
	ao_scheme_stack->frame = AO_SCHEME_NIL;
	ao_scheme_frame_current = NULL;
	return expr;
}

ao_poly
ao_scheme_do_apply(struct ao_scheme_cons *cons)
{
	ao_scheme_stack->state = eval_apply;
	return ao_scheme_cons_poly(cons);
}

ao_poly
ao_scheme_do_read(struct ao_scheme_cons *cons)
{
	FILE	*file = stdin;
#ifndef AO_SCHEME_FEATURE_PORT
	ao_poly	port;
	if (!ao_scheme_parse_args(_ao_scheme_atom_read, cons,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
#else
	struct ao_scheme_port	*port;

	if (!ao_scheme_parse_args(_ao_scheme_atom_read, cons,
				  AO_SCHEME_PORT|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (port) {
		file = port->file;
		if (!file)
			return _ao_scheme_atom_eof;
	}
#endif
	return ao_scheme_read(file);
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
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_not, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (val == AO_SCHEME_NIL)
		return _ao_scheme_bool_true;
	else
		return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_not(struct ao_scheme_cons *cons)
{
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_not, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (val == _ao_scheme_bool_false)
		return _ao_scheme_bool_true;
	else
		return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_typep(ao_poly proc, int type, struct ao_scheme_cons *cons)
{
	ao_poly val;

	if (!ao_scheme_parse_args(proc, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (ao_scheme_poly_type(val) == type)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_procedurep(struct ao_scheme_cons *cons)
{
	ao_poly val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_pair3f, cons,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	switch (ao_scheme_poly_type(val)) {
	case AO_SCHEME_BUILTIN:
	case AO_SCHEME_LAMBDA:
		return _ao_scheme_bool_true;
	default:
		return _ao_scheme_bool_false;
	}
}

ao_poly
ao_scheme_do_read_char(struct ao_scheme_cons *cons)
{
	int	c;
#ifndef AO_SCHEME_FEATURE_PORT
	ao_poly	port;
	if (!ao_scheme_parse_args(_ao_scheme_atom_read2dchar, cons,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	c = getchar();
#else
	struct ao_scheme_port	*port;

	if (!ao_scheme_parse_args(_ao_scheme_atom_read2dchar, cons,
				  AO_SCHEME_PORT|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (port)
		c = ao_scheme_port_getc(port);
	else
		c = getchar();
#endif
	if (c == EOF)
		return _ao_scheme_atom_eof;
	return ao_scheme_integer_poly(c);
}

ao_poly
ao_scheme_do_write_char(struct ao_scheme_cons *cons)
{
	int32_t c;
#ifndef AO_SCHEME_FEATURE_PORT
	ao_poly	port;
	if (!ao_scheme_parse_args(_ao_scheme_atom_write2dchar, cons,
				  AO_SCHEME_INT, &c,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	putchar(c);
#else
	struct ao_scheme_port	*port;
	if (!ao_scheme_parse_args(_ao_scheme_atom_write2dchar, cons,
				  AO_SCHEME_INT, &c,
				  AO_SCHEME_PORT|AO_SCHEME_ARG_OPTIONAL, AO_SCHEME_NIL, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (port)
		ao_scheme_port_putc(port, c);
	else
		putchar(c);
#endif
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_exit(struct ao_scheme_cons *cons)
{
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_exit, cons,
				  AO_SCHEME_POLY|AO_SCHEME_ARG_OPTIONAL, _ao_scheme_bool_true, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	ao_scheme_exception |= AO_SCHEME_EXIT;
	return val;
}

#ifdef AO_SCHEME_FEATURE_TIME

ao_poly
ao_scheme_do_current_jiffy(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_parse_args(_ao_scheme_atom_current2djiffy, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_integer_poly(ao_scheme_os_jiffy());
}

ao_poly
ao_scheme_do_jiffies_per_second(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_parse_args(_ao_scheme_atom_jiffies2dper2dsecond, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_integer_poly(AO_SCHEME_JIFFIES_PER_SECOND);
}

ao_poly
ao_scheme_do_delay(struct ao_scheme_cons *cons)
{
	int32_t delay;

	if (!ao_scheme_parse_args(_ao_scheme_atom_delay, cons,
				  AO_SCHEME_INT, &delay,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	ao_scheme_os_delay(delay);
	return cons->car;
}
#endif

#ifdef AO_SCHEME_FEATURE_POSIX

#include <unistd.h>

static char	**ao_scheme_argv;

void
ao_scheme_set_argv(char **argv)
{
	ao_scheme_argv = argv;
}

ao_poly
ao_scheme_do_command_line(struct ao_scheme_cons *cons)
{
	ao_poly	args = AO_SCHEME_NIL;
	ao_poly	arg;
	int	i;

	if (!ao_scheme_parse_args(_ao_scheme_atom_command2dline, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;

	for (i = 0; ao_scheme_argv[i]; i++);

	while (--i >= 0) {
		ao_scheme_poly_stash(args);
		arg = ao_scheme_string_poly(ao_scheme_string_new(ao_scheme_argv[i]));
		args = ao_scheme_poly_fetch();
		if (!arg)
			return AO_SCHEME_NIL;
		args = ao_scheme_cons(arg, args);
		if (!args)
			return AO_SCHEME_NIL;
	}
	return args;
}

ao_poly
ao_scheme_do_get_environment_variables(struct ao_scheme_cons *cons)
{
	ao_poly	envs = AO_SCHEME_NIL;
	ao_poly	env;
	int	i;

	if (!ao_scheme_parse_args(_ao_scheme_atom_get2denvironment2dvariables, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	for (i = 0; environ[i]; i++);

	while (--i >= 0) {
		ao_scheme_poly_stash(envs);
		env = ao_scheme_string_poly(ao_scheme_string_new(environ[i]));
		envs = ao_scheme_poly_fetch();
		if (!env)
			return AO_SCHEME_NIL;
		envs = ao_scheme_cons(env, envs);
		if (!envs)
			return AO_SCHEME_NIL;
	}
	return envs;
}

ao_poly
ao_scheme_do_get_environment_variable(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*name;
	char			*val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_get2denvironment2dvariable, cons,
				  AO_SCHEME_STRING, &name,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	val = secure_getenv(name->val);
	if (!val)
		return _ao_scheme_bool_false;
	return ao_scheme_string_poly(ao_scheme_string_new(val));
}

ao_poly
ao_scheme_do_file_existsp(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*name;

	if (!ao_scheme_parse_args(_ao_scheme_atom_file2dexists3f, cons,
				  AO_SCHEME_STRING, &name,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (access(name->val, F_OK) == 0)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_delete_file(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*name;

	if (!ao_scheme_parse_args(_ao_scheme_atom_delete2dfile, cons,
				  AO_SCHEME_STRING, &name,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (unlink(name->val) == 0)
		return _ao_scheme_bool_true;
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_current_second(struct ao_scheme_cons *cons)
{
	int32_t	second;

	if (!ao_scheme_parse_args(_ao_scheme_atom_current2dsecond, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	second = (int32_t) time(NULL);
	return ao_scheme_integer_poly(second);
}

#endif /* AO_SCHEME_FEATURE_POSIX */

#define AO_SCHEME_BUILTIN_FUNCS
#include "ao_scheme_builtin.h"
