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

void
ao_lisp_error_poly(char *name, ao_poly poly, ao_poly last)
{
	int first = 1;
	printf("\t\t%s(", name);
	if (ao_lisp_poly_type(poly) == AO_LISP_CONS) {
		if (poly) {
			while (poly) {
				struct ao_lisp_cons *cons = ao_lisp_poly_cons(poly);
				if (!first)
					printf("\t\t         ");
				else
					first = 0;
				ao_lisp_poly_write(cons->car);
				printf("\n");
				if (poly == last)
					break;
				poly = cons->cdr;
			}
			printf("\t\t         )\n");
		} else
			printf(")\n");
	} else {
		ao_lisp_poly_write(poly);
		printf("\n");
	}
}

static void tabs(int indent)
{
	while (indent--)
		printf("\t");
}

void
ao_lisp_error_frame(int indent, char *name, struct ao_lisp_frame *frame)
{
	int			f;

	tabs(indent);
	printf ("%s{", name);
	if (frame) {
		struct ao_lisp_frame_vals	*vals = ao_lisp_poly_frame_vals(frame->vals);
		if (frame->type & AO_LISP_FRAME_PRINT)
			printf("recurse...");
		else {
			frame->type |= AO_LISP_FRAME_PRINT;
			for (f = 0; f < frame->num; f++) {
				if (f != 0) {
					tabs(indent);
					printf("         ");
				}
				ao_lisp_poly_write(vals->vals[f].atom);
				printf(" = ");
				ao_lisp_poly_write(vals->vals[f].val);
				printf("\n");
			}
			if (frame->prev)
				ao_lisp_error_frame(indent + 1, "prev:   ", ao_lisp_poly_frame(frame->prev));
			frame->type &= ~AO_LISP_FRAME_PRINT;
		}
		tabs(indent);
		printf("        }\n");
	} else
		printf ("}\n");
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
	printf("Value: "); ao_lisp_poly_write(ao_lisp_v); printf("\n");
	printf("Stack:\n");
	ao_lisp_stack_write(ao_lisp_stack_poly(ao_lisp_stack));
	printf("Globals:\n\t");
	ao_lisp_frame_write(ao_lisp_frame_poly(ao_lisp_frame_global));
	printf("\n");
	return AO_LISP_NIL;
}
