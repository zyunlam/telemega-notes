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

#include "ao_lisp.h"

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

char *
ao_lisp_string_new(int len) {
	char	*a = ao_lisp_alloc(len + 1);
	if (!a)
		return NULL;
	a[len] = '\0';
	return a;
}

char *
ao_lisp_string_copy(char *a)
{
	int	alen = strlen(a);

	char	*r = ao_lisp_alloc(alen + 1);
	if (!r)
		return NULL;
	strcpy(r, a);
	return r;
}

char *
ao_lisp_string_cat(char *a, char *b)
{
	int	alen = strlen(a);
	int	blen = strlen(b);
	char	*r = ao_lisp_alloc(alen + blen + 1);
	if (!r)
		return NULL;
	strcpy(r, a);
	strcpy(r+alen, b);
	return r;
}

const struct ao_lisp_type ao_lisp_string_type = {
	.mark = string_mark,
	.size = string_size,
	.move = string_move,
};

void
ao_lisp_string_print(ao_poly p)
{
	char	*s = ao_lisp_poly_string(p);
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
			putchar(c);
			break;
		}
	}
	putchar('"');
}

void
ao_lisp_string_patom(ao_poly p)
{
	char	*s = ao_lisp_poly_string(p);
	char	c;

	while ((c = *s++))
		putchar(c);
}
