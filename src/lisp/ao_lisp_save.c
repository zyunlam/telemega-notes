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

#include <ao_lisp.h>

ao_poly
ao_lisp_save(struct ao_lisp_cons *cons)
{
#ifdef AO_LISP_SAVE
	struct ao_lisp_os_save *os = (struct ao_lisp_os_save *) &ao_lisp_pool[AO_LISP_POOL];

	ao_lisp_collect();
	os->ao_lisp_atoms = ao_lisp_atom_poly(ao_lisp_atoms);
	os->ao_lisp_globals = ao_lisp_frame_poly(ao_lisp_frame_global);
	if (ao_lisp_os_save())
		return _ao_lisp_atom_t;
#endif
	return AO_LISP_NIL;
}

ao_poly
ao_lisp_restore(struct ao_lisp_cons *cons)
{
#ifdef AO_LISP_SAVE
	struct ao_lisp_os_save *os = (struct ao_lisp_os_save *) &ao_lisp_pool[AO_LISP_POOL];

	if (ao_lisp_os_restore()) {

		ao_lisp_atoms = ao_lisp_poly_atom(os->ao_lisp_atoms);
		ao_lisp_frame_global = ao_lisp_poly_frame(os->ao_lisp_globals);

		/* Clear the eval global variabls */
		ao_lisp_eval_clear_globals();

		/* Reset the allocator */
		ao_lisp_top = AO_LISP_POOL;
		ao_lisp_collect();

		/* Re-create the evaluator stack */
		if (!ao_lisp_eval_restart())
			return AO_LISP_NIL;
		return _ao_lisp_atom_t;
	}
#endif
	return AO_LISP_NIL;
}
