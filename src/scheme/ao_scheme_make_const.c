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
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <getopt.h>
#include <stdbool.h>

static struct ao_scheme_builtin *
ao_scheme_make_builtin(enum ao_scheme_builtin_id func, int args) {
	struct ao_scheme_builtin *b = ao_scheme_alloc(sizeof (struct ao_scheme_builtin));

	b->type = AO_SCHEME_BUILTIN;
	b->func = func;
	b->args = args;
	return b;
}

struct builtin_func {
	const char	*feature;
	const char	*name;
	int		args;
	enum ao_scheme_builtin_id	func;
};

struct builtin_atom {
	const char	*feature;
	const char	*name;
};

#define AO_SCHEME_BUILTIN_CONSTS
#define AO_SCHEME_BUILTIN_ATOM_NAMES

#include "ao_scheme_builtin.h"

#define N_FUNC		(sizeof funcs / sizeof funcs[0])

#define N_ATOM		(sizeof atoms / sizeof atoms[0])

struct ao_scheme_frame	*globals;

static int
is_atom(int offset)
{
	struct ao_scheme_atom *a;

	for (a = ao_scheme_atoms; a; a = ao_scheme_poly_atom(a->next))
		if (((uint8_t *) a->name - ao_scheme_const) == offset)
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

static uint16_t
ao_fec_crc(const uint8_t *bytes, uint8_t len)
{
	uint16_t	crc = AO_FEC_CRC_INIT;

	while (len--)
		crc = ao_fec_crc_byte(*bytes++, crc);
	return crc;
}

struct ao_scheme_macro_stack {
	struct ao_scheme_macro_stack *next;
	ao_poly	p;
};

struct ao_scheme_macro_stack *macro_stack;

static int
ao_scheme_macro_push(ao_poly p)
{
	struct ao_scheme_macro_stack *m = macro_stack;

	while (m) {
		if (m->p == p)
			return 1;
		m = m->next;
	}
	m = malloc (sizeof (struct ao_scheme_macro_stack));
	m->p = p;
	m->next = macro_stack;
	macro_stack = m;
	return 0;
}

static void
ao_scheme_macro_pop(void)
{
	struct ao_scheme_macro_stack *m = macro_stack;

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

static ao_poly
ao_macro_test_get(ao_poly atom)
{
	ao_poly	*ref = ao_scheme_atom_ref(atom, NULL);
	if (ref)
		return *ref;
	return AO_SCHEME_NIL;
}

static ao_poly
ao_is_macro(ao_poly p)
{
	struct ao_scheme_builtin	*builtin;
	struct ao_scheme_lambda	*lambda;
	ao_poly ret;

	MACRO_DEBUG(indent(); printf ("is macro "); ao_scheme_poly_write(p); printf("\n"); ++macro_scan_depth);
	switch (ao_scheme_poly_type(p)) {
	case AO_SCHEME_ATOM:
		if (ao_scheme_macro_push(p))
			ret = AO_SCHEME_NIL;
		else {
			if (ao_is_macro(ao_macro_test_get(p)))
				ret = p;
			else
				ret = AO_SCHEME_NIL;
			ao_scheme_macro_pop();
		}
		break;
	case AO_SCHEME_CONS:
		ret = ao_has_macro(p);
		break;
	case AO_SCHEME_BUILTIN:
		builtin = ao_scheme_poly_builtin(p);
		if ((builtin->args & AO_SCHEME_FUNC_MASK) == AO_SCHEME_FUNC_MACRO)
			ret = p;
		else
			ret = 0;
		break;

	case AO_SCHEME_LAMBDA:
		lambda = ao_scheme_poly_lambda(p);
		if (lambda->args == AO_SCHEME_FUNC_MACRO)
			ret = p;
		else
			ret = ao_has_macro(lambda->code);
		break;
	default:
		ret = AO_SCHEME_NIL;
		break;
	}
	MACRO_DEBUG(--macro_scan_depth;	indent(); printf ("... "); ao_scheme_poly_write(ret); printf("\n"));
	return ret;
}

ao_poly
ao_has_macro(ao_poly p)
{
	struct ao_scheme_cons	*cons;
	struct ao_scheme_lambda	*lambda;
	ao_poly			m;
	ao_poly			list;

	if (p == AO_SCHEME_NIL)
		return AO_SCHEME_NIL;

	MACRO_DEBUG(indent(); printf("has macro "); ao_scheme_poly_write(p); printf("\n"); ++macro_scan_depth);
	switch (ao_scheme_poly_type(p)) {
	case AO_SCHEME_LAMBDA:
		lambda = ao_scheme_poly_lambda(p);
		p = ao_has_macro(lambda->code);
		break;
	case AO_SCHEME_CONS:
		cons = ao_scheme_poly_cons(p);
		if ((p = ao_is_macro(cons->car)))
			break;

		list = cons->cdr;
		p = AO_SCHEME_NIL;
		while (list != AO_SCHEME_NIL && AO_SCHEME_IS_CONS(list)) {
			cons = ao_scheme_poly_cons(list);
			m = ao_has_macro(cons->car);
			if (m) {
				p = m;
				break;
			}
			list = cons->cdr;
		}
		break;

	default:
		p = AO_SCHEME_NIL;
		break;
	}
	MACRO_DEBUG(--macro_scan_depth;	indent(); printf("... "); ao_scheme_poly_write(p); printf("\n"));
	return p;
}

static struct ao_scheme_builtin *
ao_scheme_get_builtin(ao_poly p)
{
	if (ao_scheme_poly_type(p) == AO_SCHEME_BUILTIN)
		return ao_scheme_poly_builtin(p);
	return NULL;
}

struct seen_builtin {
	struct seen_builtin 		*next;
	struct ao_scheme_builtin	*builtin;
};

static struct seen_builtin *seen_builtins;

static int
ao_scheme_seen_builtin(struct ao_scheme_builtin *b)
{
	struct seen_builtin	*s;

	for (s = seen_builtins; s; s = s->next)
		if (s->builtin == b)
			return 1;
	s = malloc (sizeof (struct seen_builtin));
	s->builtin = b;
	s->next = seen_builtins;
	seen_builtins = s;
	return 0;
}

static int
ao_scheme_read_eval_abort(void)
{
	ao_poly	in, out = AO_SCHEME_NIL;
	for(;;) {
		in = ao_scheme_read();
		if (in == _ao_scheme_atom_eof)
			break;
		out = ao_scheme_eval(in);
		if (ao_scheme_exception)
			return 0;
		ao_scheme_poly_write(out, true);
		putchar ('\n');
	}
	return 1;
}

static FILE	*in;
static FILE	*out;

struct feature {
	struct feature	*next;
	char		name[];
};

static struct feature *enable;
static struct feature *disable;

static void
ao_scheme_add_feature(struct feature **list, char *name)
{
	struct feature *feature = malloc (sizeof (struct feature) + strlen(name) + 1);
	strcpy(feature->name, name);
	feature->next = *list;
	*list = feature;
}

static bool
ao_scheme_has_feature(struct feature *list, const char *name)
{
	while (list) {
		if (!strcmp(list->name, name))
			return true;
		list = list->next;
	}
	return false;
}

static void
ao_scheme_add_features(struct feature **list, const char *names)
{
	char	*saveptr = NULL;
	char	*name;
	char	*copy = strdup(names);
	char	*save = copy;

	while ((name = strtok_r(copy, ",", &saveptr)) != NULL) {
		copy = NULL;
		if (!ao_scheme_has_feature(*list, name))
			ao_scheme_add_feature(list, name);
	}
	free(save);
}

int
ao_scheme_getc(void)
{
	return getc(in);
}

static const struct option options[] = {
	{ .name = "out", .has_arg = 1, .val = 'o' },
	{ .name = "disable", .has_arg = 1, .val = 'd' },
	{ .name = "enable", .has_arg = 1, .val = 'e' },
	{ 0, 0, 0, 0 }
};

static void usage(char *program)
{
	fprintf(stderr, "usage: %s [--out=<output>] [--disable={feature,...}] [--enable={feature,...} [input]\n", program);
	exit(1);
}

int
main(int argc, char **argv)
{
	int	f, o, an;
	ao_poly	val;
	struct ao_scheme_atom	*a;
	struct ao_scheme_builtin	*b;
	struct feature			*d;
	int	in_atom = 0;
	char	*out_name = NULL;
	int	c;
	enum ao_scheme_builtin_id	prev_func;
	enum ao_scheme_builtin_id	target_func;
	enum ao_scheme_builtin_id	func_map[_builtin_last];

	in = stdin;
	out = stdout;

	while ((c = getopt_long(argc, argv, "o:d:e:", options, NULL)) != -1) {
		switch (c) {
		case 'o':
			out_name = optarg;
			break;
		case 'd':
			ao_scheme_add_features(&disable, optarg);
			break;
		case 'e':
			ao_scheme_add_features(&enable, optarg);
			break;
		default:
			usage(argv[0]);
			break;
		}
	}

	ao_scheme_frame_init();

	/* Boolean values #f and #t */
	ao_scheme_bool_get(0);
	ao_scheme_bool_get(1);

	prev_func = _builtin_last;
	target_func = 0;
	b = NULL;
	for (f = 0; f < (int) N_FUNC; f++) {
		if (ao_scheme_has_feature(enable, funcs[f].feature) || !ao_scheme_has_feature(disable, funcs[f].feature)) {
			if (funcs[f].func != prev_func) {
				prev_func = funcs[f].func;
				b = ao_scheme_make_builtin(prev_func, funcs[f].args);

				/* Target may have only a subset of
				 * the enum values; record what those
				 * values will be here. This obviously
				 * depends on the functions in the
				 * array being in the same order as
				 * the enumeration; which
				 * ao_scheme_make_builtin ensures.
				 */
				func_map[prev_func] = target_func++;
			}
			a = ao_scheme_atom_intern((char *) funcs[f].name);
			ao_scheme_atom_def(ao_scheme_atom_poly(a),
					   ao_scheme_builtin_poly(b));
		}
	}

	/* atoms */
	for (an = 0; an < (int) N_ATOM; an++) {
		if (ao_scheme_has_feature(enable, atoms[an].feature) || !ao_scheme_has_feature(disable, atoms[an].feature))
			a = ao_scheme_atom_intern((char *) atoms[an].name);
	}

	if (argv[optind]){
		in = fopen(argv[optind], "r");
		if (!in) {
			perror(argv[optind]);
			exit(1);
		}
	}
	if (!ao_scheme_read_eval_abort()) {
		fprintf(stderr, "eval failed\n");
		exit(1);
	}

	/* Reduce to referenced values */
	ao_scheme_collect(AO_SCHEME_COLLECT_FULL);

	for (f = 0; f < ao_scheme_frame_global->num; f++) {
		struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(ao_scheme_frame_global->vals);

		val = ao_has_macro(vals->vals[f].val);
		if (val != AO_SCHEME_NIL) {
			printf("error: function %s contains unresolved macro: ",
			       ao_scheme_poly_atom(vals->vals[f].atom)->name);
			ao_scheme_poly_write(val, true);
			printf("\n");
			exit(1);
		}

		/* Remap builtin enum values to match target set */
		b = ao_scheme_get_builtin(vals->vals[f].val);
		if (b != NULL) {
			if (!ao_scheme_seen_builtin(b))
				b->func = func_map[b->func];
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

	for (d = disable; d; d = d->next)
		fprintf(out, "#undef AO_SCHEME_FEATURE_%s\n", d->name);

	fprintf(out, "#define AO_SCHEME_POOL_CONST %d\n", ao_scheme_top);
	fprintf(out, "extern const uint8_t ao_scheme_const[AO_SCHEME_POOL_CONST] __attribute__((aligned(4)));\n");
	fprintf(out, "#define ao_builtin_atoms 0x%04x\n", ao_scheme_atom_poly(ao_scheme_atoms));
	fprintf(out, "#define ao_builtin_frame 0x%04x\n", ao_scheme_frame_poly(ao_scheme_frame_global));
	fprintf(out, "#define ao_scheme_const_checksum ((uint16_t) 0x%04x)\n", ao_fec_crc(ao_scheme_const, ao_scheme_top));

	fprintf(out, "#define _ao_scheme_bool_false 0x%04x\n", ao_scheme_bool_poly(ao_scheme_false));
	fprintf(out, "#define _ao_scheme_bool_true 0x%04x\n", ao_scheme_bool_poly(ao_scheme_true));

	for (a = ao_scheme_atoms; a; a = ao_scheme_poly_atom(a->next)) {
		const char	*n = a->name;
		char		ch;
		fprintf(out, "#define _ao_scheme_atom_");
		while ((ch = *n++)) {
			if (isalnum(ch))
				fprintf(out, "%c", ch);
			else
				fprintf(out, "%02x", ch);
		}
		fprintf(out, "  0x%04x\n", ao_scheme_atom_poly(a));
	}
	fprintf(out, "#ifdef AO_SCHEME_CONST_BITS\n");
	fprintf(out, "const uint8_t ao_scheme_const[AO_SCHEME_POOL_CONST] __attribute((aligned(4))) = {");
	for (o = 0; o < ao_scheme_top; o++) {
		uint8_t	ch;
		if ((o & 0xf) == 0)
			fprintf(out, "\n\t");
		else
			fprintf(out, " ");
		ch = ao_scheme_const[o];
		if (!in_atom)
			in_atom = is_atom(o);
		if (in_atom) {
			fprintf(out, " '%c',", ch);
			in_atom--;
		} else {
			fprintf(out, "0x%02x,", ch);
		}
	}
	fprintf(out, "\n};\n");
	fprintf(out, "#endif /* AO_SCHEME_CONST_BITS */\n");
	exit(0);
}
