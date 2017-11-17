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
#include <unistd.h>
#include <getopt.h>

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

#define AO_LISP_BUILTIN_CONSTS
#include "ao_lisp_builtin.h"

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

#define AO_FEC_CRC_INIT	0xffff

static inline uint16_t
ao_fec_crc_byte(uint8_t byte, uint16_t crc)
{
	uint8_t	bit;

	for (bit = 0; bit < 8; bit++) {
		if (((crc & 0x8000) >> 8) ^ (byte & 0x80))
			crc = (crc << 1) ^ 0x8005;
		else
			crc = (crc << 1);
		byte <<= 1;
	}
	return crc;
}

uint16_t
ao_fec_crc(const uint8_t *bytes, uint8_t len)
{
	uint16_t	crc = AO_FEC_CRC_INIT;

	while (len--)
		crc = ao_fec_crc_byte(*bytes++, crc);
	return crc;
}

struct ao_lisp_macro_stack {
	struct ao_lisp_macro_stack *next;
	ao_poly	p;
};

struct ao_lisp_macro_stack *macro_stack;

int
ao_lisp_macro_push(ao_poly p)
{
	struct ao_lisp_macro_stack *m = macro_stack;

	while (m) {
		if (m->p == p)
			return 1;
		m = m->next;
	}
	m = malloc (sizeof (struct ao_lisp_macro_stack));
	m->p = p;
	m->next = macro_stack;
	macro_stack = m;
	return 0;
}

void
ao_lisp_macro_pop(void)
{
	struct ao_lisp_macro_stack *m = macro_stack;

	macro_stack = m->next;
	free(m);
}

#define DBG_MACRO 0
#if DBG_MACRO
int macro_scan_depth;

void indent(void)
{
	int i;
	for (i = 0; i < macro_scan_depth; i++)
		printf("  ");
}
#define MACRO_DEBUG(a)	a
#else
#define MACRO_DEBUG(a)
#endif

ao_poly
ao_has_macro(ao_poly p);

ao_poly
ao_macro_test_get(ao_poly atom)
{
	ao_poly	*ref = ao_lisp_atom_ref(ao_lisp_frame_global, atom);
	if (ref)
		return *ref;
	return AO_LISP_NIL;
}

ao_poly
ao_is_macro(ao_poly p)
{
	struct ao_lisp_builtin	*builtin;
	struct ao_lisp_lambda	*lambda;
	ao_poly ret;

	MACRO_DEBUG(indent(); printf ("is macro "); ao_lisp_poly_print(p); printf("\n"); ++macro_scan_depth);
	switch (ao_lisp_poly_type(p)) {
	case AO_LISP_ATOM:
		if (ao_lisp_macro_push(p))
			ret = AO_LISP_NIL;
		else {
			if (ao_is_macro(ao_macro_test_get(p)))
				ret = p;
			else
				ret = AO_LISP_NIL;
			ao_lisp_macro_pop();
		}
		break;
	case AO_LISP_CONS:
		ret = ao_has_macro(p);
		break;
	case AO_LISP_BUILTIN:
		builtin = ao_lisp_poly_builtin(p);
		if ((builtin->args & AO_LISP_FUNC_MASK) == AO_LISP_FUNC_MACRO)
			ret = p;
		else
			ret = 0;
		break;

	case AO_LISP_LAMBDA:
		lambda = ao_lisp_poly_lambda(p);
		if (lambda->args == AO_LISP_FUNC_MACRO)
			ret = p;
		else
			ret = ao_has_macro(lambda->code);
		break;
	default:
		ret = AO_LISP_NIL;
		break;
	}
	MACRO_DEBUG(--macro_scan_depth;	indent(); printf ("... "); ao_lisp_poly_print(ret); printf("\n"));
	return ret;
}

ao_poly
ao_has_macro(ao_poly p)
{
	struct ao_lisp_cons	*cons;
	struct ao_lisp_lambda	*lambda;
	ao_poly			m;

	if (p == AO_LISP_NIL)
		return AO_LISP_NIL;

	MACRO_DEBUG(indent(); printf("has macro "); ao_lisp_poly_print(p); printf("\n"); ++macro_scan_depth);
	switch (ao_lisp_poly_type(p)) {
	case AO_LISP_LAMBDA:
		lambda = ao_lisp_poly_lambda(p);
		p = ao_has_macro(lambda->code);
		break;
	case AO_LISP_CONS:
		cons = ao_lisp_poly_cons(p);
		if ((p = ao_is_macro(cons->car)))
			break;

		cons = ao_lisp_poly_cons(cons->cdr);
		p = AO_LISP_NIL;
		while (cons) {
			m = ao_has_macro(cons->car);
			if (m) {
				p = m;
				break;
			}
			cons = ao_lisp_poly_cons(cons->cdr);
		}
		break;

	default:
		p = AO_LISP_NIL;
		break;
	}
	MACRO_DEBUG(--macro_scan_depth;	indent(); printf("... "); ao_lisp_poly_print(p); printf("\n"));
	return p;
}

int
ao_lisp_read_eval_abort(void)
{
	ao_poly	in, out = AO_LISP_NIL;
	for(;;) {
		in = ao_lisp_read();
		if (in == _ao_lisp_atom_eof)
			break;
		out = ao_lisp_eval(in);
		if (ao_lisp_exception)
			return 0;
		ao_lisp_poly_print(out);
		putchar ('\n');
	}
	return 1;
}

static FILE	*in;
static FILE	*out;

int
ao_lisp_getc(void)
{
	return getc(in);
}

static const struct option options[] = {
	{ .name = "out", .has_arg = 1, .val = 'o' },
	{ 0, 0, 0, 0 }
};

static void usage(char *program)
{
	fprintf(stderr, "usage: %s [--out=<output>] [input]\n", program);
	exit(1);
}

int
main(int argc, char **argv)
{
	int	f, o;
	ao_poly	val;
	struct ao_lisp_atom	*a;
	struct ao_lisp_builtin	*b;
	int	in_atom = 0;
	char	*out_name = NULL;
	int	c;

	in = stdin;
	out = stdout;

	while ((c = getopt_long(argc, argv, "o:", options, NULL)) != -1) {
		switch (c) {
		case 'o':
			out_name = optarg;
			break;
		default:
			usage(argv[0]);
			break;
		}
	}

	/* Boolean values #f and #t */
	ao_lisp_bool_get(0);
	ao_lisp_bool_get(1);

	for (f = 0; f < (int) N_FUNC; f++) {
		b = ao_lisp_make_builtin(funcs[f].func, funcs[f].args);
		a = ao_lisp_atom_intern(funcs[f].name);
		ao_lisp_atom_set(ao_lisp_atom_poly(a),
				 ao_lisp_builtin_poly(b));
	}

	/* end of file value */
	a = ao_lisp_atom_intern("eof");
	ao_lisp_atom_set(ao_lisp_atom_poly(a),
			 ao_lisp_atom_poly(a));

	/* 'else' */
	a = ao_lisp_atom_intern("else");

	if (argv[optind]){
		in = fopen(argv[optind], "r");
		if (!in) {
			perror(argv[optind]);
			exit(1);
		}
	}
	if (!ao_lisp_read_eval_abort()) {
		fprintf(stderr, "eval failed\n");
		exit(1);
	}

	/* Reduce to referenced values */
	ao_lisp_collect(AO_LISP_COLLECT_FULL);

	for (f = 0; f < ao_lisp_frame_global->num; f++) {
		val = ao_has_macro(ao_lisp_frame_global->vals[f].val);
		if (val != AO_LISP_NIL) {
			printf("error: function %s contains unresolved macro: ",
			       ao_lisp_poly_atom(ao_lisp_frame_global->vals[f].atom)->name);
			ao_lisp_poly_print(val);
			printf("\n");
			exit(1);
		}
	}

	if (out_name) {
		out = fopen(out_name, "w");
		if (!out) {
			perror(out_name);
			exit(1);
		}
	}

	fprintf(out, "/* Generated file, do not edit */\n\n");

	fprintf(out, "#define AO_LISP_POOL_CONST %d\n", ao_lisp_top);
	fprintf(out, "extern const uint8_t ao_lisp_const[AO_LISP_POOL_CONST] __attribute__((aligned(4)));\n");
	fprintf(out, "#define ao_builtin_atoms 0x%04x\n", ao_lisp_atom_poly(ao_lisp_atoms));
	fprintf(out, "#define ao_builtin_frame 0x%04x\n", ao_lisp_frame_poly(ao_lisp_frame_global));
	fprintf(out, "#define ao_lisp_const_checksum ((uint16_t) 0x%04x)\n", ao_fec_crc(ao_lisp_const, ao_lisp_top));

	fprintf(out, "#define _ao_lisp_bool_false 0x%04x\n", ao_lisp_bool_poly(ao_lisp_false));
	fprintf(out, "#define _ao_lisp_bool_true 0x%04x\n", ao_lisp_bool_poly(ao_lisp_true));

	for (a = ao_lisp_atoms; a; a = ao_lisp_poly_atom(a->next)) {
		char	*n = a->name, c;
		fprintf(out, "#define _ao_lisp_atom_");
		while ((c = *n++)) {
			if (isalnum(c))
				fprintf(out, "%c", c);
			else
				fprintf(out, "%02x", c);
		}
		fprintf(out, "  0x%04x\n", ao_lisp_atom_poly(a));
	}
	fprintf(out, "#ifdef AO_LISP_CONST_BITS\n");
	fprintf(out, "const uint8_t ao_lisp_const[AO_LISP_POOL_CONST] __attribute((aligned(4))) = {");
	for (o = 0; o < ao_lisp_top; o++) {
		uint8_t	c;
		if ((o & 0xf) == 0)
			fprintf(out, "\n\t");
		else
			fprintf(out, " ");
		c = ao_lisp_const[o];
		if (!in_atom)
			in_atom = is_atom(o);
		if (in_atom) {
			fprintf(out, " '%c',", c);
			in_atom--;
		} else {
			fprintf(out, "0x%02x,", c);
		}
	}
	fprintf(out, "\n};\n");
	fprintf(out, "#endif /* AO_LISP_CONST_BITS */\n");
	exit(0);
}
