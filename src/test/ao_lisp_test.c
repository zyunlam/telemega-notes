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
#include <stdio.h>

static FILE *ao_lisp_file;
static int newline = 1;

int
ao_lisp_getc(void)
{
	int c;

	if (ao_lisp_file)
		return getc(ao_lisp_file);

	if (newline) {
		printf("> ");
		newline = 0;
	}
	c = getchar();
	if (c == '\n')
		newline = 1;
	return c;
}

int
main (int argc, char **argv)
{
	while (*++argv) {
		ao_lisp_file = fopen(*argv, "r");
		if (!ao_lisp_file) {
			perror(*argv);
			exit(1);
		}
		ao_lisp_read_eval_print();
		fclose(ao_lisp_file);
		ao_lisp_file = NULL;
	}
	ao_lisp_read_eval_print();
}
