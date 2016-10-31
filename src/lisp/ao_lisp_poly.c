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

enum math_op { math_plus, math_minus, math_times, math_divide, math_mod };

ao_lisp_poly
ao_lisp_math(struct ao_lisp_cons *cons, enum math_op op)
{
	ao_lisp_poly	ret = AO_LISP_NIL;

	while (cons) {
		ao_lisp_poly	car = cons->car;
		uint8_t		rt = ao_lisp_poly_type(ret);
		uint8_t		ct = ao_lisp_poly_type(car);

		cons = cons->cdr;

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
				if (c == 0)
					return AO_LISP_NIL;
				r /= c;
				break;
			case math_mod:
				if (c == 0)
					return AO_LISP_NIL;
				r %= c;
				break;
			}
			ret = ao_lisp_int_poly(r);
		}

		else if (rt == AO_LISP_STRING && ct == AO_LISP_STRING && op == math_plus)
			ret = ao_lisp_string_poly(ao_lisp_string_cat(ao_lisp_poly_string(ret),
								     ao_lisp_poly_string(car)));
		else {
			/* XXX exception */
			return AO_LISP_NIL;
		}
	}
	return ret;
}

ao_lisp_poly
ao_lisp_plus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_plus);
}

ao_lisp_poly
ao_lisp_minus(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_minus);
}

ao_lisp_poly
ao_lisp_times(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_times);
}

ao_lisp_poly
ao_lisp_divide(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_divide);
}

ao_lisp_poly
ao_lisp_mod(struct ao_lisp_cons *cons)
{
	return ao_lisp_math(cons, math_mod);
}

static const struct ao_lisp_builtin builtin_plus = {
	.type = AO_LISP_BUILTIN,
	.func = ao_lisp_plus,
	.name = "+"
};

static const struct ao_lisp_atom atom_plus = {
	.type = AO_LISP_ATOM,
	.val = AO_LISP_OTHER_POLY(&builtin_plus),
	.next = AO_LISP_ATOM_CONST,
	.name = "plus"
};

/*
static const struct ao_lisp_builtin builtin_minus = {
	.type = AO_LISP_BUILTIN,
	.func = ao_lisp_minus
};

static const struct ao_lisp_builtin builtin_times = {
	.type = AO_LISP_BUILTIN,
	.func = ao_lisp_times
};

*/

const struct ao_lisp_atom const *ao_lisp_builtins[] = {
	&atom_plus,
	0
};
