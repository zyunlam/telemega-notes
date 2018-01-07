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
	s->val[len] = '\0';
	return s;
}

struct ao_scheme_string *
ao_scheme_string_new(char *a)
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

	ao_scheme_atom_stash(a);
	r = ao_scheme_string_alloc(alen);
	a = ao_scheme_atom_fetch();
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

	ao_scheme_string_stash(a);
	ao_scheme_string_stash(b);
	r = ao_scheme_string_alloc(alen + blen);
	b = ao_scheme_string_fetch();
	a = ao_scheme_string_fetch();
	if (!r)
		return NULL;
	strcpy(r->val, a->val);
	strcpy(r->val+alen, b->val);
	return r;
}

static ao_poly
ao_scheme_string_pack(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*string;
	char			*s;
	int			len;

	len = ao_scheme_cons_length(cons);
	ao_scheme_cons_stash(cons);
	string = ao_scheme_string_alloc(len);
	cons = ao_scheme_cons_fetch();
	if (!string)
		return AO_SCHEME_NIL;
	s = string->val;

	while (cons) {
		ao_poly	car = cons->car;
		int32_t c;
		if (!ao_scheme_is_integer(car) || (c = ao_scheme_poly_integer(car)) == 0)
			return ao_scheme_error(AO_SCHEME_INVALID, "%v: Invalid %v", _ao_scheme_atom_list2d3estring, car);
		*s++ = c;
		cons = ao_scheme_cons_cdr(cons);
	}
	return ao_scheme_string_poly(string);
}

static ao_poly
ao_scheme_string_unpack(struct ao_scheme_string *a)
{
	ao_poly	cons = AO_SCHEME_NIL;
	int	i;

	for (i = strlen(a->val); --i >= 0;) {
		ao_scheme_string_stash(a);
		cons = ao_scheme_cons(ao_scheme_int_poly(a->val[i]), cons);
		a = ao_scheme_string_fetch();
		if (!cons)
			break;
	}
	return cons;
}

void
ao_scheme_string_write(FILE *out, ao_poly p, bool write)
{
	struct ao_scheme_string	*s = ao_scheme_poly_string(p);
	char			*sval = s->val;
	char			c;

	if (write) {
		putc('"', out);
		while ((c = *sval++)) {
			switch (c) {
			case '\a':
				fputs("\\a", out);
				break;
			case '\b':
				fputs("\\b", out);
				break;
			case '\t':
				fputs("\\t", out);
				break;
			case '\n':
				fputs("\\n", out);
				break;
			case '\r':
				fputs("\\r", out);
				break;
			case '\f':
				fputs("\\f", out);
				break;
			case '\v':
				fputs("\\v", out);
				break;
			case '\"':
				fputs("\\\"", out);
				break;
			case '\\':
				fputs("\\\\", out);
				break;
			default:
				if (c < ' ')
					fprintf(out, "\\%03o", c);
				else
					putc(c, out);
				break;
			}
		}
		putc('"', out);
	} else {
		while ((c = *sval++))
			putc(c, out);
	}
}

ao_poly
ao_scheme_do_stringp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(_ao_scheme_atom_string3f, AO_SCHEME_STRING, cons);
}

ao_poly
ao_scheme_do_list_to_string(struct ao_scheme_cons *cons)
{
	struct ao_scheme_cons	*list;

	if (!ao_scheme_parse_args(_ao_scheme_atom_list2d3estring, cons,
				  AO_SCHEME_CONS, &list,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_string_pack(list);
}

ao_poly
ao_scheme_do_string_to_list(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*string;

	if (!ao_scheme_parse_args(_ao_scheme_atom_string2d3elist, cons,
				  AO_SCHEME_STRING, &string,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_string_unpack(string);
}

static char *
ao_scheme_string_ref(struct ao_scheme_string *string, int32_t r)
{
	char *s = string->val;
	while (*s && r) {
		++s;
		--r;
	}
	return s;
}

ao_poly
ao_scheme_do_string_ref(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*string;
	int32_t			ref;
	char			*s;

	if (!ao_scheme_parse_args(_ao_scheme_atom_string2dref, cons,
				  AO_SCHEME_STRING, &string,
				  AO_SCHEME_INT, &ref,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;

	s = ao_scheme_string_ref(string, ref);
	if (!*s)
		return ao_scheme_error(AO_SCHEME_INVALID, "%v: string %v ref %v invalid",
				       _ao_scheme_atom_string2dref,
				       cons->car,
				       ao_scheme_arg(cons, 1));
	return ao_scheme_integer_poly(*s);
}

ao_poly
ao_scheme_do_string_length(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string *string;

	if (!ao_scheme_parse_args(_ao_scheme_atom_string2dlength, cons,
				  AO_SCHEME_STRING, &string,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_integer_poly(strlen(string->val));
}

ao_poly
ao_scheme_do_string_set(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*string;
	int32_t			ref;
	int32_t			val;
	char			*s;

	if (!ao_scheme_parse_args(_ao_scheme_atom_string2dset21, cons,
				  AO_SCHEME_STRING, &string,
				  AO_SCHEME_INT, &ref,
				  AO_SCHEME_INT, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!val)
		goto fail;
	s = ao_scheme_string_ref(string, ref);
	if (!*s)
		goto fail;
	*s = val;
	return ao_scheme_integer_poly(val);
fail:
	return ao_scheme_error(AO_SCHEME_INVALID, "%v: %v[%v] = %v invalid",
			       _ao_scheme_atom_string2dset21,
			       ao_scheme_arg(cons, 0),
			       ao_scheme_arg(cons, 1),
			       ao_scheme_arg(cons, 2));
}

ao_poly
ao_scheme_do_make_string(struct ao_scheme_cons *cons)
{
	int32_t			len;
	int32_t			fill;
	struct ao_scheme_string	*string;

	if (!ao_scheme_parse_args(_ao_scheme_atom_make2dstring, cons,
				  AO_SCHEME_INT, &len,
				  AO_SCHEME_INT|AO_SCHEME_ARG_OPTIONAL, ao_scheme_int_poly(' '), &fill,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!fill)
		return ao_scheme_error(AO_SCHEME_INVALID, "%v: fill 0 invalid",
				       _ao_scheme_atom_make2dstring);
	string = ao_scheme_string_alloc(len);
	if (!string)
		return AO_SCHEME_NIL;
	memset(string->val, fill, len);
	return ao_scheme_string_poly(string);
}

ao_poly
ao_scheme_do_symbol_to_string(struct ao_scheme_cons *cons)
{
	struct ao_scheme_atom	*atom;

	if (!ao_scheme_parse_args(_ao_scheme_atom_symbol2d3estring, cons,
				  AO_SCHEME_ATOM, &atom,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_string_poly(ao_scheme_atom_to_string(atom));
}

ao_poly
ao_scheme_do_string_to_symbol(struct ao_scheme_cons *cons)
{
	struct ao_scheme_string	*string;

	if (!ao_scheme_parse_args(_ao_scheme_atom_string2d3esymbol, cons,
				  AO_SCHEME_STRING, &string,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_atom_poly(ao_scheme_string_to_atom(string));
}
