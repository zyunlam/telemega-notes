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

#if 0
static struct ao_lisp_cons	*list;
static char			*string;
#endif

int
main (int argc, char **argv)
{
#if 0
	int			i, j;

	struct ao_lisp_atom	*atom;
	ao_lisp_root_add(&ao_lisp_cons_type, (void **) &list);
	ao_lisp_root_add(&ao_lisp_string_type, (void **) &string);

	/* allocator test */
	for (j = 0; j < 10; j++) {
		list = 0;
		string = ao_lisp_string_new(0);
		for (i = 0; i < 2; i++) {
			string = ao_lisp_string_cat(string, "a");
			list = ao_lisp_cons_cons(ao_lisp_string_poly(string), list);
			list = ao_lisp_cons_cons(ao_lisp_int_poly(i), list);
			atom = ao_lisp_atom_intern("ant");
			list = ao_lisp_cons_cons(ao_lisp_atom_poly(atom), list);
		}
		ao_lisp_poly_print(ao_lisp_cons_poly(list));
		printf("\n");
	}

	for (atom = ao_lisp_poly_atom(ao_builtin_atoms); atom; atom = ao_lisp_poly_atom(atom->next)) {
		printf("%s = ", atom->name);
		ao_lisp_poly_print(ao_lisp_atom_get(ao_lisp_atom_poly(atom)));
		printf("\n");
	}
#endif
#if 0
	list = ao_lisp_cons_cons(ao_lisp_atom_poly(ao_lisp_atom_intern("+")),
				 ao_lisp_cons_cons(ao_lisp_cons_poly(ao_lisp_cons_cons(ao_lisp_atom_poly(ao_lisp_atom_intern("+")),
										       ao_lisp_cons_cons(ao_lisp_int_poly(3),
													 ao_lisp_cons_cons(ao_lisp_int_poly(4), NULL)))),
						   ao_lisp_cons_cons(ao_lisp_int_poly(2), NULL)));
	printf("list: ");
	ao_lisp_poly_print(ao_lisp_cons_poly(list));
	printf ("\n");
	ao_lisp_poly_print(ao_lisp_eval(ao_lisp_cons_poly(list)));
	printf ("\n");
#endif
#if 1
	ao_lisp_read_eval_print();
#endif
}
