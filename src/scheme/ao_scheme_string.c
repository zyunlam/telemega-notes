/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "ao_scheme.h"

static void string_mark(void *addr)
{
	(void) addr;
}

static int string_size(void *addr)
{
	if (!addr)
		return 0;
	return strlen(addr) + 1;
}

static void string_move(void *addr)
{
	(void) addr;
}

const struct ao_scheme_type ao_scheme_string_type = {
	.mark = string_mark,
	.size = string_size,
	.move = string_move,
	.name = "string",
};

char *
ao_scheme_string_copy(char *a)
{
	int	alen = strlen(a);

	ao_scheme_string_stash(0, a);
	char	*r = ao_scheme_alloc(alen + 1);
	a = ao_scheme_string_fetch(0);
	if (!r)
		return NULL;
	strcpy(r, a);
	return r;
}

char *
ao_scheme_string_cat(char *a, char *b)
{
	int	alen = strlen(a);
	int	blen = strlen(b);

	ao_scheme_string_stash(0, a);
	ao_scheme_string_stash(1, b);
	char	*r = ao_scheme_alloc(alen + blen + 1);
	a = ao_scheme_string_fetch(0);
	b = ao_scheme_string_fetch(1);
	if (!r)
		return NULL;
	strcpy(r, a);
	strcpy(r+alen, b);
	return r;
}

ao_poly
ao_scheme_string_pack(struct ao_scheme_cons *cons)
{
	int	len = ao_scheme_cons_length(cons);
	ao_scheme_cons_stash(0, cons);
	char	*r = ao_scheme_alloc(len + 1);
	cons = ao_scheme_cons_fetch(0);
	char	*s = r;

	while (cons) {
		if (!ao_scheme_integer_typep(ao_scheme_poly_type(cons->car)))
			return ao_scheme_error(AO_SCHEME_INVALID, "non-int passed to pack");
		*s++ = ao_scheme_poly_integer(cons->car);
		cons = ao_scheme_poly_cons(cons->cdr);
	}
	*s++ = 0;
	return ao_scheme_string_poly(r);
}

ao_poly
ao_scheme_string_unpack(char *a)
{
	struct ao_scheme_cons	*cons = NULL, *tail = NULL;
	int			c;
	int			i;

	for (i = 0; (c = a[i]); i++) {
		ao_scheme_cons_stash(0, cons);
		ao_scheme_cons_stash(1, tail);
		ao_scheme_string_stash(0, a);
		struct ao_scheme_cons	*n = ao_scheme_cons_cons(ao_scheme_int_poly(c), AO_SCHEME_NIL);
		a = ao_scheme_string_fetch(0);
		cons = ao_scheme_cons_fetch(0);
		tail = ao_scheme_cons_fetch(1);

		if (!n) {
			cons = NULL;
			break;
		}
		if (tail)
			tail->cdr = ao_scheme_cons_poly(n);
		else
			cons = n;
		tail = n;
	}
	return ao_scheme_cons_poly(cons);
}

void
ao_scheme_string_write(ao_poly p)
{
	char	*s = ao_scheme_poly_string(p);
	char	c;

	putchar('"');
	while ((c = *s++)) {
		switch (c) {
		case '\n':
			printf ("\\n");
			break;
		case '\r':
			printf ("\\r");
			break;
		case '\t':
			printf ("\\t");
			break;
		default:
			if (c < ' ')
				printf("\\%03o", c);
			else
				putchar(c);
			break;
		}
	}
	putchar('"');
}

void
ao_scheme_string_display(ao_poly p)
{
	char	*s = ao_scheme_poly_string(p);
	char	c;

	while ((c = *s++))
		putchar(c);
}
