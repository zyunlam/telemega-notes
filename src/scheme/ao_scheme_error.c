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
#include <stdarg.h>

void
ao_scheme_error_poly(char *name, ao_poly poly, ao_poly last)
{
	int first = 1;
	printf("\t\t%s(", name);
	if (ao_scheme_poly_type(poly) == AO_SCHEME_CONS) {
		if (poly) {
			while (poly) {
				struct ao_scheme_cons *cons = ao_scheme_poly_cons(poly);
				if (!first)
					printf("\t\t         ");
				else
					first = 0;
				ao_scheme_poly_write(cons->car);
				printf("\n");
				if (poly == last)
					break;
				poly = cons->cdr;
			}
			printf("\t\t         )\n");
		} else
			printf(")\n");
	} else {
		ao_scheme_poly_write(poly);
		printf("\n");
	}
}

static void tabs(int indent)
{
	while (indent--)
		printf("\t");
}

void
ao_scheme_error_frame(int indent, char *name, struct ao_scheme_frame *frame)
{
	int			f;

	tabs(indent);
	printf ("%s{", name);
	if (frame) {
		struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);
		if (frame->type & AO_SCHEME_FRAME_PRINT)
			printf("recurse...");
		else {
			frame->type |= AO_SCHEME_FRAME_PRINT;
			for (f = 0; f < frame->num; f++) {
				if (f != 0) {
					tabs(indent);
					printf("         ");
				}
				ao_scheme_poly_write(vals->vals[f].atom);
				printf(" = ");
				ao_scheme_poly_write(vals->vals[f].val);
				printf("\n");
			}
			if (frame->prev)
				ao_scheme_error_frame(indent + 1, "prev:   ", ao_scheme_poly_frame(frame->prev));
			frame->type &= ~AO_SCHEME_FRAME_PRINT;
		}
		tabs(indent);
		printf("        }\n");
	} else
		printf ("}\n");
}

void
ao_scheme_vprintf(char *format, va_list args)
{
	char c;

	while ((c = *format++) != '\0') {
		if (c == '%') {
			switch (c = *format++) {
			case 'v':
				ao_scheme_poly_write((ao_poly) va_arg(args, unsigned int));
				break;
			case 'p':
				printf("%p", va_arg(args, void *));
				break;
			case 'd':
				printf("%d", va_arg(args, int));
				break;
			case 's':
				printf("%s", va_arg(args, char *));
				break;
			default:
				putchar(c);
				break;
			}
		} else
			putchar(c);
	}
}

void
ao_scheme_printf(char *format, ...)
{
	va_list args;
	va_start(args, format);
	ao_scheme_vprintf(format, args);
	va_end(args);
}

ao_poly
ao_scheme_error(int error, char *format, ...)
{
	va_list	args;

	ao_scheme_exception |= error;
	va_start(args, format);
	ao_scheme_vprintf(format, args);
	putchar('\n');
	va_end(args);
	ao_scheme_printf("Value:  %v\n", ao_scheme_v);
	ao_scheme_printf("Frame:  %v\n", ao_scheme_frame_poly(ao_scheme_frame_current));
	printf("Stack:\n");
	ao_scheme_stack_write(ao_scheme_stack_poly(ao_scheme_stack));
	ao_scheme_printf("Globals: %v\n", ao_scheme_frame_poly(ao_scheme_frame_global));
	return AO_SCHEME_NIL;
}
