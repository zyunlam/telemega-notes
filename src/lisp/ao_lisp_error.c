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
#include <stdarg.h>

static void
ao_lisp_error_cons(char *name, struct ao_lisp_cons *cons)
{
	int first = 1;
	printf("\t\t%s(", name);
	if (cons) {
		while (cons) {
			if (!first)
				printf("\t\t         ");
			else
				first = 0;
			ao_lisp_poly_print(cons->car);
			printf("\n");
			cons = ao_lisp_poly_cons(cons->cdr);
		}
		printf("\t\t         )\n");
	} else
		printf(")\n");
}

static void tabs(int indent)
{
	while (indent--)
		printf("\t");
}

static void
ao_lisp_error_frame(int indent, char *name, struct ao_lisp_frame *frame)
{
	int			f;

	tabs(indent);
	printf ("%s{", name);
	if (frame) {
		for (f = 0; f < frame->num; f++) {
			if (f != 0) {
				tabs(indent);
				printf("         ");
			}
			ao_lisp_poly_print(frame->vals[f].atom);
			printf(" = ");
			ao_lisp_poly_print(frame->vals[f].val);
			printf("\n");
		}
		if (frame->next)
			ao_lisp_error_frame(indent + 1, "next:   ", ao_lisp_poly_frame(frame->next));
	}
	tabs(indent);
	printf("        }\n");
}

static const char *state_names[] = {
	"sexpr",
	"val",
	"formal",
	"exec",
	"cond",
	"cond_test",
};

void
ao_lisp_stack_print(void)
{
	struct ao_lisp_stack *s;
	printf("Value:  "); ao_lisp_poly_print(ao_lisp_v); printf("\n");
	printf("Stack:\n");
	for (s = ao_lisp_stack; s; s = ao_lisp_poly_stack(s->prev)) {
		printf("\t[\n");
		printf("\t\texpr:   "); ao_lisp_poly_print(s->list); printf("\n");
		printf("\t\tstate:  %s\n", state_names[s->state]);
//		printf("\t\tmacro:  %s\n", s->macro ? "true" : "false");
		ao_lisp_error_cons ("sexprs: ", ao_lisp_poly_cons(s->sexprs));
		ao_lisp_error_cons ("values: ", ao_lisp_poly_cons(s->values));
		ao_lisp_error_frame(2, "frame:  ", ao_lisp_poly_frame(s->frame));
//		ao_lisp_error_frame(2, "mframe: ", ao_lisp_poly_frame(s->macro_frame));
		printf("\t]\n");
	}
}

ao_poly
ao_lisp_error(int error, char *format, ...)
{
	va_list	args;

	ao_lisp_exception |= error;
	va_start(args, format);
	vprintf(format, args);
	va_end(args);
	printf("\n");
	ao_lisp_stack_print();
	return AO_LISP_NIL;
}
