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

static int name_size(char *name)
{
	return sizeof(struct ao_lisp_atom) + strlen(name) + 1;
}

static int atom_size(void *addr)
{
	struct ao_lisp_atom	*atom = addr;
	if (!atom)
		return 0;
	return name_size(atom->name);
}

static void atom_mark(void *addr)
{
	struct ao_lisp_atom	*atom = addr;

	for (;;) {
		atom = ao_lisp_poly_atom(atom->next);
		if (!atom)
			break;
		if (ao_lisp_mark_memory(&ao_lisp_atom_type, atom))
			break;
	}
}

static void atom_move(void *addr)
{
	struct ao_lisp_atom	*atom = addr;
	int			ret;

	for (;;) {
		struct ao_lisp_atom *next = ao_lisp_poly_atom(atom->next);

		if (!next)
			break;
		ret = ao_lisp_move_memory(&ao_lisp_atom_type, (void **) &next);
		if (next != ao_lisp_poly_atom(atom->next))
			atom->next = ao_lisp_atom_poly(next);
		if (ret)
			break;
		atom = next;
	}
}

const struct ao_lisp_type ao_lisp_atom_type = {
	.mark = atom_mark,
	.size = atom_size,
	.move = atom_move,
	.name = "atom"
};

struct ao_lisp_atom	*ao_lisp_atoms;

struct ao_lisp_atom *
ao_lisp_atom_intern(char *name)
{
	struct ao_lisp_atom	*atom;

	for (atom = ao_lisp_atoms; atom; atom = ao_lisp_poly_atom(atom->next)) {
		if (!strcmp(atom->name, name))
			return atom;
	}
#ifdef ao_builtin_atoms
	for (atom = ao_lisp_poly_atom(ao_builtin_atoms); atom; atom = ao_lisp_poly_atom(atom->next)) {
		if (!strcmp(atom->name, name))
			return atom;
	}
#endif
	ao_lisp_string_stash(0, name);
	atom = ao_lisp_alloc(name_size(name));
	name = ao_lisp_string_fetch(0);
	if (atom) {
		atom->type = AO_LISP_ATOM;
		atom->next = ao_lisp_atom_poly(ao_lisp_atoms);
		ao_lisp_atoms = atom;
		strcpy(atom->name, name);
	}
	return atom;
}

ao_poly *
ao_lisp_atom_ref(ao_poly atom)
{
	ao_poly	*ref;
	struct ao_lisp_frame *frame;

	for (frame = ao_lisp_frame_current; frame; frame = ao_lisp_poly_frame(frame->prev)) {
		ref = ao_lisp_frame_ref(frame, atom);
		if (ref)
			return ref;
	}
	return ao_lisp_frame_ref(ao_lisp_frame_global, atom);
}

ao_poly
ao_lisp_atom_get(ao_poly atom)
{
	ao_poly *ref = ao_lisp_atom_ref(atom);

#ifdef ao_builtin_frame
	if (!ref)
		ref = ao_lisp_frame_ref(ao_lisp_poly_frame(ao_builtin_frame), atom);
#endif
	if (ref)
		return *ref;
	return ao_lisp_error(AO_LISP_UNDEFINED, "undefined atom %s", ao_lisp_poly_atom(atom)->name);
}

ao_poly
ao_lisp_atom_set(ao_poly atom, ao_poly val)
{
	ao_poly *ref = ao_lisp_atom_ref(atom);

	if (!ref)
		return ao_lisp_error(AO_LISP_UNDEFINED, "undefined atom %s", ao_lisp_poly_atom(atom)->name);
	*ref = val;
	return val;
}

ao_poly
ao_lisp_atom_def(ao_poly atom, ao_poly val)
{
	ao_poly *ref = ao_lisp_atom_ref(atom);

	if (ref) {
		if (ao_lisp_frame_current)
			return ao_lisp_error(AO_LISP_REDEFINED, "attempt to redefine atom %s", ao_lisp_poly_atom(atom)->name);
		*ref = val;
		return val;
	}
	return ao_lisp_frame_add(ao_lisp_frame_current ? ao_lisp_frame_current : ao_lisp_frame_global, atom, val);
}

void
ao_lisp_atom_write(ao_poly a)
{
	struct ao_lisp_atom *atom = ao_lisp_poly_atom(a);
	printf("%s", atom->name);
}
