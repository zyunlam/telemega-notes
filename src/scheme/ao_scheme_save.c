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

#include "ao_scheme.h"

#ifdef AO_SCHEME_FEATURE_SAVE
ao_poly
ao_scheme_do_save(struct ao_scheme_cons *cons)
{
#ifndef AO_SCHEME_MAKE_CONST
	struct ao_scheme_os_save *os;

	if (!ao_scheme_parse_args(_ao_scheme_atom_save, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;

	os = (struct ao_scheme_os_save *) (void *) &ao_scheme_pool[AO_SCHEME_POOL];

	ao_scheme_collect(AO_SCHEME_COLLECT_FULL);
	os->atoms = ao_scheme_atom_poly(ao_scheme_atoms);
	os->globals = ao_scheme_frame_poly(ao_scheme_frame_global);
	os->const_checksum = ao_scheme_const_checksum;
	os->const_checksum_inv = (uint16_t) ~ao_scheme_const_checksum;

	if (ao_scheme_os_save())
		return _ao_scheme_bool_true;
#else
	(void) cons;
#endif
	return _ao_scheme_bool_false;
}

ao_poly
ao_scheme_do_restore(struct ao_scheme_cons *cons)
{
#ifndef AO_SCHEME_MAKE_CONST
	struct ao_scheme_os_save save;
	struct ao_scheme_os_save *os = (struct ao_scheme_os_save *) (void *) &ao_scheme_pool[AO_SCHEME_POOL];
	if (!ao_scheme_parse_args(_ao_scheme_atom_restore, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;

	os = (struct ao_scheme_os_save *) (void *) &ao_scheme_pool[AO_SCHEME_POOL];

	if (!ao_scheme_os_restore_save(&save, AO_SCHEME_POOL))
		return ao_scheme_error(AO_SCHEME_INVALID, "header restore failed");

	if (save.const_checksum != ao_scheme_const_checksum ||
	    save.const_checksum_inv != (uint16_t) ~ao_scheme_const_checksum)
	{
		return ao_scheme_error(AO_SCHEME_INVALID, "image is corrupted or stale");
	}

	if (ao_scheme_os_restore()) {

		ao_scheme_atoms = ao_scheme_poly_atom(os->atoms);
		ao_scheme_frame_global = ao_scheme_poly_frame(os->globals);

		/* Clear the eval global variabls */
		ao_scheme_eval_clear_globals();

		/* Reset the allocator */
		ao_scheme_top = AO_SCHEME_POOL;
		ao_scheme_collect(AO_SCHEME_COLLECT_FULL);

		/* Re-create the evaluator stack */
		if (!ao_scheme_eval_restart())
			return _ao_scheme_bool_false;

		return _ao_scheme_bool_true;
	}
#else
	(void) cons;
#endif
	return _ao_scheme_bool_false;
}

#endif /* AO_SCHEME_FEATURE_SAVE */
