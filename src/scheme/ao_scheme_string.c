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
	struct ao_scheme_string	*string = addr;
	if (!addr)
		return 0;
	return strlen(string->val) + 2;
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

static struct ao_scheme_string *
ao_scheme_string_alloc(int len)
{
	struct ao_scheme_string	*s;

	s = ao_scheme_alloc(len + 2);
	if (!s)
		return NULL;
	s->type = AO_SCHEME_STRING;
	return s;
}

struct ao_scheme_string *
ao_scheme_string_copy(struct ao_scheme_string *a)
{
	int			alen = strlen(a->val);
	struct ao_scheme_string	*r;

	ao_scheme_string_stash(0, a);
	r = ao_scheme_string_alloc(alen);
	a = ao_scheme_string_fetch(0);
	if (!r)
		return NULL;
	strcpy(r->val, a->val);
	return r;
}

struct ao_scheme_string *
ao_scheme_string_make(char *a)
{
	struct ao_scheme_string	*r;

	r = ao_scheme_string_alloc(strlen(a));
	if (!r)
		return NULL;
	strcpy(r->val, a);
	return r;
}

struct ao_scheme_string *
ao_scheme_atom_to_string(struct ao_scheme_atom *a)
{
	int			alen = strlen(a->name);
	struct ao_scheme_string	*r;

	ao_scheme_poly_stash(0, ao_scheme_atom_poly(a));
	r = ao_scheme_string_alloc(alen);
	a = ao_scheme_poly_atom(ao_scheme_poly_fetch(0));
	if (!r)
		return NULL;
	strcpy(r->val, a->name);
	return r;
}

struct ao_scheme_string *
ao_scheme_string_cat(struct ao_scheme_string *a, struct ao_scheme_string *b)
{
	int				alen = strlen(a->val);
	int				blen = strlen(b->val);
	struct ao_scheme_string 	*r;

	ao_scheme_string_stash(0, a);
	ao_scheme_string_stash(1, b);
	r = ao_scheme_string_alloc(alen + blen);
	a = ao_scheme_string_fetch(0);
	b = ao_scheme_string_fetch(1);
	if (!r)
		return NULL;
	strcpy(r->val, a->val);
	strcpy(r->val+alen, b->val);
	return r;
}

ao_poly
ao_scheme_string_pack(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*r;
	char			*rval;
	int			len;

	len = ao_scheme_cons_length(cons);
	ao_scheme_cons_stash(0, cons);
	r = ao_scheme_string_alloc(len);
	cons = ao_scheme_cons_fetch(0);
	if (!r)
		return AO_SCHEME_NIL;
	rval = r->val;

	while (cons) {
		bool fail = false;
		ao_poly	car = cons->car;
		*rval++ = ao_scheme_poly_integer(car, &fail);
		if (fail)
			return ao_scheme_error(AO_SCHEME_INVALID, "non-int passed to pack");
		cons = ao_scheme_cons_cdr(cons);
	}
	*rval++ = 0;
	return ao_scheme_string_poly(r);
}

ao_poly
ao_scheme_string_unpack(struct ao_scheme_string *a)
{
	struct ao_scheme_cons	*cons = NULL, *tail = NULL;
	int			c;
	int			i;

	for (i = 0; (c = a->val[i]); i++) {
		struct ao_scheme_cons	*n;
		ao_scheme_cons_stash(0, cons);
		ao_scheme_cons_stash(1, tail);
		ao_scheme_string_stash(0, a);
		n = ao_scheme_cons_cons(ao_scheme_int_poly(c), AO_SCHEME_NIL);
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
ao_scheme_string_write(ao_poly p, bool write)
{
	struct ao_scheme_string	*s = ao_scheme_poly_string(p);
	char			*sval = s->val;
	char			c;

	if (write) {
		putchar('"');
		while ((c = *sval++)) {
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
	} else {
		while ((c = *sval++))
			putchar(c);
	}
}
