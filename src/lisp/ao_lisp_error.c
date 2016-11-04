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

ao_poly
ao_lisp_error(int error, char *format, ...)
{
	va_list	args;

	ao_lisp_exception |= error;
	va_start(args, format);
	vprintf(format, args);
	va_end(args);
	printf("\n");
	return AO_LISP_NIL;
}
