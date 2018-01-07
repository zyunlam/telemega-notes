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
ao_scheme_vfprintf(FILE *out, const char *format, va_list args)
{
	char c;

	while ((c = *format++) != '\0') {
		if (c == '%') {
			switch (c = *format++) {
			case 'v':
				ao_scheme_poly_write(out, (ao_poly) va_arg(args, unsigned int), true);
				break;
			case 'V':
				ao_scheme_poly_write(out, (ao_poly) va_arg(args, unsigned int), false);
				break;
			case 'p':
				fprintf(out, "%p", va_arg(args, void *));
				break;
			case 'd':
				fprintf(out, "%d", va_arg(args, int));
				break;
			case 'x':
				fprintf(out, "%x", va_arg(args, int));
				break;
			case 's':
				fprintf(out, "%s", va_arg(args, char *));
				break;
			default:
				putc(c, out);
				break;
			}
		} else
			putc(c, out);
	}
}

void
ao_scheme_fprintf(FILE *out, const char *format, ...)
{
	va_list args;
	va_start(args, format);
	ao_scheme_vfprintf(out, format, args);
	va_end(args);
}

ao_poly
ao_scheme_error(int error, const char *format, ...)
{
	va_list	args;

	ao_scheme_exception |= error;
	va_start(args, format);
	ao_scheme_vfprintf(stdout, format, args);
	putchar('\n');
	va_end(args);
	ao_scheme_fprintf(stdout, "Value:  %v\n", ao_scheme_v);
	ao_scheme_fprintf(stdout, "Frame:  %v\n", ao_scheme_frame_poly(ao_scheme_frame_current));
	printf("Stack:\n");
	ao_scheme_stack_write(stdout, ao_scheme_stack_poly(ao_scheme_stack), true);
	ao_scheme_fprintf(stdout, "Globals: %v\n", ao_scheme_frame_poly(ao_scheme_frame_global));
	return AO_SCHEME_NIL;
}
