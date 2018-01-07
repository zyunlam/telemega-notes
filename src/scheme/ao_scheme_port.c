/*
 * Copyright © 2018 Keith Packard <keithp@keithp.com>
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

#ifdef AO_SCHEME_FEATURE_PORT

static void port_mark(void *addr)
{
	(void) addr;
}

static int port_size(void *addr)
{
	(void) addr;
	return sizeof(struct ao_scheme_port);
}

static void port_move(void *addr)
{
	struct ao_scheme_port	*port = addr;

	(void) ao_scheme_poly_move(&port->next, 0);
}

const struct ao_scheme_type	ao_scheme_port_type = {
	.mark = port_mark,
	.size = port_size,
	.move = port_move,
	.name = "port",
};

void
ao_scheme_port_write(FILE *out, ao_poly v, bool write)
{
	(void) write;
	ao_scheme_fprintf(out, "#port<%d>", fileno(ao_scheme_poly_port(v)->file));
}

ao_poly		ao_scheme_stdin, ao_scheme_stdout, ao_scheme_stderr;

ao_poly		ao_scheme_open_ports;

void
ao_scheme_port_check_references(void)
{
	struct ao_scheme_port	*p;

	for (p = ao_scheme_poly_port(ao_scheme_open_ports); p; p = ao_scheme_poly_port(p->next)) {
		if (!ao_scheme_marked(p))
			ao_scheme_port_close(p);
	}
}

struct ao_scheme_port *
ao_scheme_port_alloc(FILE *file, bool stayopen)
{
	struct ao_scheme_port	*p;

	p = ao_scheme_alloc(sizeof (struct ao_scheme_port));
	if (!p)
		return NULL;
	p->type = AO_SCHEME_PORT;
	p->stayopen = stayopen;
	p->file = file;
	p->next = ao_scheme_open_ports;
	ao_scheme_open_ports = ao_scheme_port_poly(p);
	return p;
}

void
ao_scheme_port_close(struct ao_scheme_port *port)
{
	ao_poly			*prev;
	struct ao_scheme_port	*ref;

	if (port->file && !port->stayopen) {
		fclose(port->file);
		port->file = NULL;
		for (prev = &ao_scheme_open_ports; (ref = ao_scheme_poly_port(*prev)); prev = &ref->next)
			if (ref == port) {
				*prev = port->next;
				break;
			}
	}
}

ao_poly
ao_scheme_do_portp(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_typep(_ao_scheme_atom_port3f, AO_SCHEME_PORT, cons);
}

ao_poly
ao_scheme_do_port_openp(struct ao_scheme_cons *cons)
{
	struct ao_scheme_port	*port;

	if (!ao_scheme_parse_args(_ao_scheme_atom_port2dopen3f, cons,
				  AO_SCHEME_PORT, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	return port->file ? _ao_scheme_bool_true : _ao_scheme_bool_false;
}

static ao_poly
ao_scheme_do_open_file(ao_poly proc, struct ao_scheme_cons *cons, const char *mode)
{
	FILE			*file;
	struct ao_scheme_string	*name;

	if (!ao_scheme_parse_args(proc, cons,
				  AO_SCHEME_STRING, &name,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	file = fopen(name->val, mode);
	if (!file)
		return ao_scheme_error(AO_SCHEME_FILEERROR,
				       "%v: no such file \"%v\"",
				       proc, name);
	return ao_scheme_port_poly(ao_scheme_port_alloc(file, false));
}

ao_poly
ao_scheme_do_open_input_file(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_open_file(_ao_scheme_atom_open2dinput2dfile, cons, "r");
}

ao_poly
ao_scheme_do_open_output_file(struct ao_scheme_cons *cons)
{
	return ao_scheme_do_open_file(_ao_scheme_atom_open2doutput2dfile, cons, "w");
}

ao_poly
ao_scheme_do_close_port(struct ao_scheme_cons *cons)
{
	struct ao_scheme_port	*port;

	if (!ao_scheme_parse_args(_ao_scheme_atom_port2dopen3f, cons,
				  AO_SCHEME_PORT, &port,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	ao_scheme_port_close(port);
	return _ao_scheme_bool_true;
}

ao_poly
ao_scheme_do_current_input_port(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_parse_args(_ao_scheme_atom_current2dinput2dport, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!ao_scheme_stdin)
		ao_scheme_stdin = ao_scheme_port_poly(ao_scheme_port_alloc(stdin, true));
	return ao_scheme_stdin;
}

ao_poly
ao_scheme_do_current_output_port(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_parse_args(_ao_scheme_atom_current2doutput2dport, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!ao_scheme_stdout)
		ao_scheme_stdout = ao_scheme_port_poly(ao_scheme_port_alloc(stdout, true));
	return ao_scheme_stdout;
}

ao_poly
ao_scheme_do_current_error_port(struct ao_scheme_cons *cons)
{
	if (!ao_scheme_parse_args(_ao_scheme_atom_current2derror2dport, cons,
				  AO_SCHEME_ARG_END))
		return AO_SCHEME_NIL;
	if (!ao_scheme_stderr)
		ao_scheme_stderr = ao_scheme_port_poly(ao_scheme_port_alloc(stderr, true));
	return ao_scheme_stderr;
}

#endif /* AO_SCHEME_FEATURE_PORT */
