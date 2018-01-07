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

static int name_size(char *name)
{
	return sizeof(struct ao_scheme_atom) + strlen(name) + 1;
}

static int atom_size(void *addr)
{
	struct ao_scheme_atom	*atom = addr;
	if (!atom)
		return 0;
	return name_size(atom->name);
}

static void atom_mark(void *addr)
{
	MDBG_MOVE("mark atom %s\n", ((struct ao_scheme_atom *) addr)->name);
	(void) addr;
}

static void atom_move(void *addr)
{
	(void) addr;
}

const struct ao_scheme_type ao_scheme_atom_type = {
	.mark = atom_mark,
	.size = atom_size,
	.move = atom_move,
	.name = "atom"
};

struct ao_scheme_atom	*ao_scheme_atoms;

static struct ao_scheme_atom *
ao_scheme_atom_find(const char *name)
{
	struct ao_scheme_atom	*atom;

#ifdef ao_builtin_atoms
	if (!ao_scheme_atoms)
		ao_scheme_atoms = ao_scheme_poly_atom(ao_builtin_atoms);
#endif
	for (atom = ao_scheme_atoms; atom; atom = ao_scheme_poly_atom(atom->next)) {
		if (!strcmp(atom->name, name))
			return atom;
	}
	return NULL;
}

#ifdef AO_SCHEME_MAKE_CONST

#define AO_SCHEME_BUILTIN_SYNTAX_ATOMS
#include "ao_scheme_builtin.h"
#undef AO_SCHEME_BUILTIN_SYNTAX_ATOMS

static void
ao_scheme_atom_mark_syntax(void)
{
	unsigned	a;
	for (a = 0; a < sizeof(syntax_atoms)/sizeof(syntax_atoms[0]); a++) {
		struct ao_scheme_atom *atom = ao_scheme_atom_find(syntax_atoms[a]);
		if (atom)
			ao_scheme_mark_memory(&ao_scheme_atom_type, atom);
	}
}

#else
#define ao_scheme_atom_mark_syntax()
#endif

void
ao_scheme_atom_move(void)
{
	struct ao_scheme_atom	*atom;
	ao_scheme_move_memory(&ao_scheme_atom_type, (void **) (void *) &ao_scheme_atoms);
	for (atom = ao_scheme_atoms; atom; atom = ao_scheme_poly_atom(atom->next)) {
		if (!ao_scheme_is_pool_addr(atom)) {
			MDBG_DO(printf("atom out of pool %s\n", atom->name));
			break;
		}
		MDBG_DO(printf("move atom %s\n", atom->name));
		ao_scheme_poly_move(&atom->next, 0);
	}
}

void
ao_scheme_atom_check_references(void)
{
	struct ao_scheme_atom	*atom;
	ao_poly			*prev = NULL;

	ao_scheme_atom_mark_syntax();
	for (atom = ao_scheme_atoms; atom; atom = ao_scheme_poly_atom(atom->next)) {
		if (!ao_scheme_marked(atom)) {
			MDBG_DO(printf("unreferenced atom %s\n", atom->name));
			if (prev)
				*prev = atom->next;
			else
				ao_scheme_atoms = ao_scheme_poly_atom(atom->next);
		} else
			prev = &atom->next;
	}
}

static void
ao_scheme_atom_init(struct ao_scheme_atom *atom, char *name)
{
	if (atom) {
		atom->type = AO_SCHEME_ATOM;
		strcpy(atom->name, name);
		atom->next = ao_scheme_atom_poly(ao_scheme_atoms);
		ao_scheme_atoms = atom;
	}
}

struct ao_scheme_atom *
ao_scheme_string_to_atom(struct ao_scheme_string *string)
{
	struct ao_scheme_atom	*atom = ao_scheme_atom_find(string->val);

	if (atom)
		return atom;
	ao_scheme_string_stash(string);
	atom = ao_scheme_alloc(name_size(string->val));
	string = ao_scheme_string_fetch();
	ao_scheme_atom_init(atom, string->val);
	return atom;
}

struct ao_scheme_atom *
ao_scheme_atom_intern(char *name)
{
	struct ao_scheme_atom	*atom = ao_scheme_atom_find(name);
	if (atom)
		return atom;

	atom = ao_scheme_alloc(name_size(name));
	ao_scheme_atom_init(atom, name);
	return atom;
}

ao_poly *
ao_scheme_atom_ref(ao_poly atom, struct ao_scheme_frame **frame_ref)
{
	ao_poly	*ref;
	struct ao_scheme_frame *frame;

	for (frame = ao_scheme_frame_current; frame; frame = ao_scheme_poly_frame(frame->prev)) {
		ref = ao_scheme_frame_ref(frame, atom);
		if (ref) {
			if (frame_ref)
				*frame_ref = frame;
			return ref;
		}
	}
	ref = ao_scheme_frame_ref(ao_scheme_frame_global, atom);
	if (ref)
		if (frame_ref)
			*frame_ref = ao_scheme_frame_global;
	return ref;
}

ao_poly
ao_scheme_atom_get(ao_poly atom)
{
	ao_poly *ref = ao_scheme_atom_ref(atom, NULL);

#ifdef ao_builtin_frame
	if (!ref)
		ref = ao_scheme_frame_ref(ao_scheme_poly_frame(ao_builtin_frame), atom);
#endif
	if (ref)
		return *ref;
	return ao_scheme_error(AO_SCHEME_UNDEFINED, "undefined atom %s", ao_scheme_poly_atom(atom)->name);
}

ao_poly
ao_scheme_atom_def(ao_poly atom, ao_poly val)
{
	struct ao_scheme_frame	*frame;
	ao_poly *ref = ao_scheme_atom_ref(atom, &frame);

	if (ref) {
		if (frame == ao_scheme_frame_current)
			return ao_scheme_error(AO_SCHEME_REDEFINED, "attempt to redefine atom %s", ao_scheme_poly_atom(atom)->name);
		*ref = val;
		return val;
	}
	return ao_scheme_frame_add(ao_scheme_frame_current ? ao_scheme_frame_current : ao_scheme_frame_global, atom, val);
}

void
ao_scheme_atom_write(FILE *out, ao_poly a, bool write)
{
	struct ao_scheme_atom *atom = ao_scheme_poly_atom(a);
	(void) write;
	fprintf(out, "%s", atom->name);
}

ao_poly
ao_scheme_do_symbolp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(_ao_scheme_atom_symbol3f, AO_SCHEME_ATOM, cons);
}

ao_poly
ao_scheme_do_set(struct ao_scheme_cons *cons)
{
	ao_poly	atom;
	ao_poly val;
	ao_poly *ref;

	if (!ao_scheme_parse_args(_ao_scheme_atom_set, cons,
				  AO_SCHEME_ATOM|AO_SCHEME_ARG_RET_POLY, &atom,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;

	ref = ao_scheme_atom_ref(atom, NULL);

	if (!ref)
		return ao_scheme_error(AO_SCHEME_UNDEFINED, "%v: undefined atom %v",
				       _ao_scheme_atom_set, atom);
	*ref = val;
	return val;
}

ao_poly
ao_scheme_do_def(struct ao_scheme_cons *cons)
{
	ao_poly	atom;
	ao_poly	val;

	if (!ao_scheme_parse_args(_ao_scheme_atom_set, cons,
				  AO_SCHEME_ATOM|AO_SCHEME_ARG_RET_POLY, &atom,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_atom_def(atom, val);
}

ao_poly
ao_scheme_do_setq(struct ao_scheme_cons *cons)
{
	ao_poly	atom;
	ao_poly	val;
	ao_poly	p;

	if (!ao_scheme_parse_args(_ao_scheme_atom_set21, cons,
				  AO_SCHEME_ATOM|AO_SCHEME_ARG_RET_POLY, &atom,
				  AO_SCHEME_POLY, &val,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!ao_scheme_atom_ref(atom, NULL))
		return ao_scheme_error(AO_SCHEME_INVALID, "%v: symbol %v not defined",
				       _ao_scheme_atom_set21, atom);
	/*
	 * Build the macro return -- `(set (quote ,atom) ,val)
	 */
	ao_scheme_poly_stash(cons->cdr);
	p = ao_scheme_cons(atom, AO_SCHEME_NIL);
	p = ao_scheme_cons(_ao_scheme_atom_quote, p);
	p = ao_scheme_cons(p, ao_scheme_poly_fetch());
	return ao_scheme_cons(_ao_scheme_atom_set, p);
}

#ifdef AO_SCHEME_FEATURE_UNDEF
ao_poly
ao_scheme_do_undef(struct ao_scheme_cons *cons)
{
	ao_poly	atom;

	if (!ao_scheme_parse_args(_ao_scheme_atom_set, cons,
				  AO_SCHEME_ATOM|AO_SCHEME_ARG_RET_POLY, &atom,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return ao_scheme_frame_del(ao_scheme_frame_global, atom);
}
#endif
