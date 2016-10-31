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

static struct ao_lisp_cons	*list;
static char			*string;

int
main (int argc, char **argv)
{
	int	i, j;
	struct ao_lisp_atom	*atom;
	ao_lisp_root_add(&ao_lisp_cons_type, (void **) &list);
	ao_lisp_root_add(&ao_lisp_string_type, (void **) &string);

	/* allocator test */
	for (j = 0; j < 10; j++) {
		list = 0;
		string = ao_lisp_string_new(0);
		for (i = 0; i < 7; i++) {
			string = ao_lisp_string_cat(string, "a");
			list = ao_lisp_cons(ao_lisp_string_poly(string), list);
			list = ao_lisp_cons(ao_lisp_int_poly(i), list);
			atom = ao_lisp_atom_intern("ant");
			atom->val = ao_lisp_cons_poly(list);
			list = ao_lisp_cons(ao_lisp_atom_poly(atom), list);
		}
		ao_lisp_poly_print(ao_lisp_cons_poly(list));
		printf("\n");
	}

	atom = ao_lisp_atom_intern("ant");
	atom->val = ao_lisp_string_poly(ao_lisp_string_cat("hello world", ""));

	list = ao_lisp_cons(ao_lisp_atom_poly(ao_lisp_atom_intern("plus")),
			    ao_lisp_cons(ao_lisp_cons_poly(ao_lisp_cons(ao_lisp_atom_poly(ao_lisp_atom_intern("plus")),
									ao_lisp_cons(ao_lisp_int_poly(3),
										     ao_lisp_cons(ao_lisp_int_poly(4), NULL)))),
					 ao_lisp_cons(ao_lisp_int_poly(2), NULL)));
	printf("list: ");
	ao_lisp_poly_print(ao_lisp_cons_poly(list));
	printf ("\n");
	ao_lisp_poly_print(ao_lisp_eval(ao_lisp_cons_poly(list)));
	printf ("\n");
}
