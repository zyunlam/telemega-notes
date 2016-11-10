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
#include <stdlib.h>
#include <ctype.h>

static struct ao_lisp_builtin *
ao_lisp_make_builtin(enum ao_lisp_builtin_id func, int args) {
	struct ao_lisp_builtin *b = ao_lisp_alloc(sizeof (struct ao_lisp_builtin));

	b->type = AO_LISP_BUILTIN;
	b->func = func;
	b->args = args;
	return b;
}

struct builtin_func {
	char	*name;
	int	args;
	int	func;
};

struct builtin_func funcs[] = {
	"lambda",	AO_LISP_FUNC_NLAMBDA,	builtin_lambda,
	"lexpr",	AO_LISP_FUNC_NLAMBDA,	builtin_lexpr,
	"nlambda",	AO_LISP_FUNC_NLAMBDA,	builtin_nlambda,
	"macro",	AO_LISP_FUNC_NLAMBDA,	builtin_macro,
	"car",		AO_LISP_FUNC_LAMBDA,	builtin_car,
	"cdr",		AO_LISP_FUNC_LAMBDA,	builtin_cdr,
	"cons",		AO_LISP_FUNC_LAMBDA,	builtin_cons,
	"last",		AO_LISP_FUNC_LAMBDA,	builtin_last,
	"quote",	AO_LISP_FUNC_NLAMBDA,	builtin_quote,
	"set",		AO_LISP_FUNC_LAMBDA,	builtin_set,
	"setq",		AO_LISP_FUNC_MACRO,	builtin_setq,
	"cond",		AO_LISP_FUNC_NLAMBDA,	builtin_cond,
	"print",	AO_LISP_FUNC_LEXPR,	builtin_print,
	"patom",	AO_LISP_FUNC_LEXPR,	builtin_patom,
	"+",		AO_LISP_FUNC_LEXPR,	builtin_plus,
	"-",		AO_LISP_FUNC_LEXPR,	builtin_minus,
	"*",		AO_LISP_FUNC_LEXPR,	builtin_times,
	"/",		AO_LISP_FUNC_LEXPR,	builtin_divide,
	"%",		AO_LISP_FUNC_LEXPR,	builtin_mod,
	"=",		AO_LISP_FUNC_LEXPR,	builtin_equal,
	"<",		AO_LISP_FUNC_LEXPR,	builtin_less,
	">",		AO_LISP_FUNC_LEXPR,	builtin_greater,
	"<=",		AO_LISP_FUNC_LEXPR,	builtin_less_equal,
	">=",		AO_LISP_FUNC_LEXPR,	builtin_greater_equal,
	"delay",	AO_LISP_FUNC_LAMBDA,	builtin_delay,
	"led",		AO_LISP_FUNC_LEXPR,	builtin_led,
};

#define N_FUNC (sizeof funcs / sizeof funcs[0])

struct ao_lisp_frame	*globals;

static int
is_atom(int offset)
{
	struct ao_lisp_atom *a;

	for (a = ao_lisp_atoms; a; a = ao_lisp_poly_atom(a->next))
		if (((uint8_t *) a->name - ao_lisp_const) == offset)
			return strlen(a->name);
	return 0;
}

int
main(int argc, char **argv)
{
	int	f, o, i;
	ao_poly	sexpr, val;
	struct ao_lisp_atom	*a;
	struct ao_lisp_builtin	*b;
	int	in_atom;

	printf("/*\n");
	printf(" * Generated file, do not edit\n");
	for (f = 0; f < N_FUNC; f++) {
		b = ao_lisp_make_builtin(funcs[f].func, funcs[f].args);
		a = ao_lisp_atom_intern(funcs[f].name);
		ao_lisp_atom_set(ao_lisp_atom_poly(a),
				 ao_lisp_builtin_poly(b));
	}

	/* boolean constants */
	ao_lisp_atom_set(ao_lisp_atom_poly(ao_lisp_atom_intern("nil")),
			 AO_LISP_NIL);
	a = ao_lisp_atom_intern("t");
	ao_lisp_atom_set(ao_lisp_atom_poly(a),
			 ao_lisp_atom_poly(a));

	for (;;) {
		sexpr = ao_lisp_read();
		if (!sexpr)
			break;
		printf ("sexpr: ");
		ao_lisp_poly_print(sexpr);
		printf("\n");
		val = ao_lisp_eval(sexpr);
		if (ao_lisp_exception)
			exit(1);
		printf("\t");
		ao_lisp_poly_print(val);
		printf("\n");
	}

	/* Reduce to referenced values */
	ao_lisp_collect();
	printf(" */\n");

	printf("#define AO_LISP_POOL_CONST %d\n", ao_lisp_top);
	printf("extern const uint8_t ao_lisp_const[AO_LISP_POOL_CONST] __attribute__((aligned(4)));\n");
	printf("#define ao_builtin_atoms 0x%04x\n", ao_lisp_atom_poly(ao_lisp_atoms));
	printf("#define ao_builtin_frame 0x%04x\n", ao_lisp_frame_poly(ao_lisp_frame_global));

	for (a = ao_lisp_atoms; a; a = ao_lisp_poly_atom(a->next)) {
		char	*n = a->name, c;
		printf ("#define _ao_lisp_atom_");
		while ((c = *n++)) {
			if (isalnum(c))
				printf("%c", c);
			else
				printf("%02x", c);
		}
		printf("  0x%04x\n", ao_lisp_atom_poly(a));
	}
	printf("#ifdef AO_LISP_CONST_BITS\n");
	printf("const uint8_t ao_lisp_const[] = {");
	for (o = 0; o < ao_lisp_top; o++) {
		uint8_t	c;
		if ((o & 0xf) == 0)
			printf("\n\t");
		else
			printf(" ");
		c = ao_lisp_const[o];
		if (!in_atom)
			in_atom = is_atom(o);
		if (in_atom) {
			printf (" '%c',", c);
			in_atom--;
		} else {
			printf("0x%02x,", c);
		}
	}
	printf("\n};\n");
	printf("#endif /* AO_LISP_CONST_BITS */\n");
}
