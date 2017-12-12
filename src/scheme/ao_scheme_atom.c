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
	struct ao_scheme_atom	*atom = addr;

	for (;;) {
		atom = ao_scheme_poly_atom(atom->next);
		if (!atom)
			break;
		if (ao_scheme_mark_memory(&ao_scheme_atom_type, atom))
			break;
	}
}

static void atom_move(void *addr)
{
	struct ao_scheme_atom	*atom = addr;
	int			ret;

	for (;;) {
		struct ao_scheme_atom *next = ao_scheme_poly_atom(atom->next);

		if (!next)
			break;
		ret = ao_scheme_move_memory(&ao_scheme_atom_type, (void **) &next);
		if (next != ao_scheme_poly_atom(atom->next))
			atom->next = ao_scheme_atom_poly(next);
		if (ret)
			break;
		atom = next;
	}
}

const struct ao_scheme_type ao_scheme_atom_type = {
	.mark = atom_mark,
	.size = atom_size,
	.move = atom_move,
	.name = "atom"
};

struct ao_scheme_atom	*ao_scheme_atoms;

struct ao_scheme_atom *
ao_scheme_atom_intern(char *name)
{
	struct ao_scheme_atom	*atom;

	for (atom = ao_scheme_atoms; atom; atom = ao_scheme_poly_atom(atom->next)) {
		if (!strcmp(atom->name, name))
			return atom;
	}
#ifdef ao_builtin_atoms
	for (atom = ao_scheme_poly_atom(ao_builtin_atoms); atom; atom = ao_scheme_poly_atom(atom->next)) {
		if (!strcmp(atom->name, name))
			return atom;
	}
#endif
	ao_scheme_string_stash(0, name);
	atom = ao_scheme_alloc(name_size(name));
	name = ao_scheme_string_fetch(0);
	if (atom) {
		atom->type = AO_SCHEME_ATOM;
		atom->next = ao_scheme_atom_poly(ao_scheme_atoms);
		ao_scheme_atoms = atom;
		strcpy(atom->name, name);
	}
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
ao_scheme_atom_set(ao_poly atom, ao_poly val)
{
	ao_poly *ref = ao_scheme_atom_ref(atom, NULL);

	if (!ref)
		return ao_scheme_error(AO_SCHEME_UNDEFINED, "undefined atom %s", ao_scheme_poly_atom(atom)->name);
	*ref = val;
	return val;
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
ao_scheme_atom_write(ao_poly a)
{
	struct ao_scheme_atom *atom = ao_scheme_poly_atom(a);
	printf("%s", atom->name);
}
