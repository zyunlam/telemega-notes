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

#if 0
#define DBG(...)	printf(__VA_ARGS__)
#else
#define DBG(...)
#endif

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

	DBG ("\tatom start %s\n", atom->name);
	for (;;) {
		atom = ao_lisp_poly_atom(atom->next);
		if (!atom)
			break;
		DBG("\t\tatom mark %s %d\n", atom->name, (uint8_t *) atom - ao_lisp_const);
		if (ao_lisp_mark_memory(atom, atom_size(atom)))
			break;
	}
	DBG ("\tatom done\n");
}

static void atom_move(void *addr)
{
	struct ao_lisp_atom	*atom = addr;

	DBG("\tatom move start %s %d next %s %d\n",
	    atom->name, ((uint8_t *) atom - ao_lisp_const),
	    atom->next ? ao_lisp_poly_atom(atom->next)->name : "(none)",
	    atom->next ? ((uint8_t *) ao_lisp_poly_atom(atom->next) - ao_lisp_const) : 0);
	for (;;) {
		struct ao_lisp_atom	*next;

		next = ao_lisp_poly_atom(atom->next);
		next = ao_lisp_move_memory(next, atom_size(next));
		if (!next)
			break;
		DBG("\t\tatom move %s %d->%d\n", next->name, ((uint8_t *) ao_lisp_poly_atom(atom->next) - ao_lisp_const), ((uint8_t *) next - ao_lisp_const));
		atom->next = ao_lisp_atom_poly(next);
		atom = next;
	}
	DBG("\tatom move end\n");
}

const struct ao_lisp_type ao_lisp_atom_type = {
	.mark = atom_mark,
	.size = atom_size,
	.move = atom_move,
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
	atom = ao_lisp_alloc(name_size(name));
	if (atom) {
		atom->type = AO_LISP_ATOM;
		atom->next = ao_lisp_atom_poly(ao_lisp_atoms);
		if (!ao_lisp_atoms)
			ao_lisp_root_add(&ao_lisp_atom_type, &ao_lisp_atoms);
		ao_lisp_atoms = atom;
		strcpy(atom->name, name);
	}
	return atom;
}

static struct ao_lisp_frame	*globals;

ao_poly
ao_lisp_atom_get(ao_poly atom)
{
	struct ao_lisp_frame	*frame = globals;
#ifdef ao_builtin_frame
	if (!frame)
		frame = ao_lisp_poly_frame(ao_builtin_frame);
#endif
	return ao_lisp_frame_get(frame, atom);
}

ao_poly
ao_lisp_atom_set(ao_poly atom, ao_poly val)
{
	if (!ao_lisp_frame_set(globals, atom, val)) {
		globals = ao_lisp_frame_add(globals, atom, val);
		if (!globals->next) {
			ao_lisp_root_add(&ao_lisp_frame_type, &globals);
#ifdef ao_builtin_frame
			globals->next = ao_builtin_frame;
#endif
		}
	}
	return val;
}

void
ao_lisp_atom_print(ao_poly a)
{
	struct ao_lisp_atom *atom = ao_lisp_poly_atom(a);
	printf("%s", atom->name);
}
