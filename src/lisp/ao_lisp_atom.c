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

	if (atom->next == AO_LISP_ATOM_CONST)
		return;

	for (;;) {
		ao_lisp_poly_mark(atom->val);
		atom = atom->next;
		if (!atom)
			break;
		if (ao_lisp_mark_memory(atom, atom_size(atom)))
			break;
	}
}

static void atom_move(void *addr)
{
	struct ao_lisp_atom	*atom = addr;

	if (atom->next == AO_LISP_ATOM_CONST)
		return;

	for (;;) {
		struct ao_lisp_atom	*next;

		atom->val = ao_lisp_poly_move(atom->val);
		next = ao_lisp_move_memory(atom->next, atom_size(atom->next));
		if (!next)
			break;
		atom->next = next;
		atom = next;
	}
}

const struct ao_lisp_mem_type ao_lisp_atom_type = {
	.mark = atom_mark,
	.size = atom_size,
	.move = atom_move,
};

struct ao_lisp_atom	*atoms;

struct ao_lisp_atom *
ao_lisp_atom_intern(char *name)
{
	struct ao_lisp_atom	*atom;
	int			b;

	for (atom = atoms; atom; atom = atom->next) {
		if (!strcmp(atom->name, name))
			return atom;
	}
	for (b = 0; ao_lisp_builtins[b]; b++)
		if (!strcmp(ao_lisp_builtins[b]->name, name))
			return (struct ao_lisp_atom *) ao_lisp_builtins[b];
	if (!atoms)
		ao_lisp_root_add(&ao_lisp_atom_type, (void **) &atoms);
	atom = ao_lisp_alloc(name_size(name));
	if (atom) {
		atom->type = AO_LISP_ATOM;
		atom->next = atoms;
		atoms = atom;
		strcpy(atom->name, name);
		atom->val = AO_LISP_NIL;
	}
	return atom;
}

void
ao_lisp_atom_print(struct ao_lisp_atom *a)
{
	fputs(a->name, stdout);
}
