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

#ifndef _AO_LISP_H_
#define _AO_LISP_H_

#include <stdint.h>
#include <string.h>
#include <stdio.h>


# define AO_LISP_CONS	0
# define AO_LISP_INT	1
# define AO_LISP_STRING	2
# define AO_LISP_OTHER	3

# define AO_LISP_ATOM		4
# define AO_LISP_BUILTIN	5

# define AO_LISP_NIL	0

#define AO_LISP_POOL	1024
#define AO_LISP_ROOT	16

static inline void *ao_lisp_set_ref(void *addr) {
	return (void *) ((intptr_t)addr | 1);
}

static inline void *ao_lisp_clear_ref(void *addr) {
	return (void *) ((intptr_t)addr & ~1);
}

extern uint8_t	ao_lisp_pool[AO_LISP_POOL];

struct ao_lisp_mem_type {
	void	(*mark)(void *addr);
	int	(*size)(void *addr);
	void	(*move)(void *addr);
};

typedef intptr_t	ao_lisp_poly;

struct ao_lisp_cons {
	ao_lisp_poly		car;
	struct ao_lisp_cons	*cdr;
};

struct ao_lisp_atom {
	uint8_t			type;
	ao_lisp_poly		val;
	struct ao_lisp_atom	*next;
	char			name[];
};

#define AO_LISP_ATOM_CONST	((struct ao_lisp_atom *) (intptr_t) 1)

extern const struct ao_lisp_atom *ao_lisp_builtins[];

struct ao_lisp_builtin {
	uint8_t			type;
	ao_lisp_poly		(*func)(struct ao_lisp_cons *cons);
	char			name[];
};

static inline void *
ao_lisp_poly_other(ao_lisp_poly poly) {
	return (void *) (poly - AO_LISP_OTHER);
}

static const inline ao_lisp_poly
ao_lisp_other_poly(const void *other)
{
	return (ao_lisp_poly) other + AO_LISP_OTHER;
}

#define AO_LISP_OTHER_POLY(other) ((ao_lisp_poly)(other) + AO_LISP_OTHER)

static inline int ao_lisp_poly_type(ao_lisp_poly poly) {
	int	type = poly & 3;
	if (type == AO_LISP_OTHER)
		return *((uint8_t *) ao_lisp_poly_other(poly));
	return type;
}

static inline struct ao_lisp_cons *
ao_lisp_poly_cons(ao_lisp_poly poly)
{
	return (struct ao_lisp_cons *) (poly - AO_LISP_CONS);
}

static inline ao_lisp_poly
ao_lisp_cons_poly(struct ao_lisp_cons *cons)
{
	return (ao_lisp_poly) cons + AO_LISP_CONS;
}

static inline int
ao_lisp_poly_int(ao_lisp_poly poly)
{
	return (int) (poly >> 2);
}

static inline ao_lisp_poly
ao_lisp_int_poly(int i)
{
	return ((ao_lisp_poly) i << 2) + AO_LISP_INT;
}

static inline char *
ao_lisp_poly_string(ao_lisp_poly poly)
{
	return (char *) (poly - AO_LISP_STRING);
}

static inline ao_lisp_poly
ao_lisp_string_poly(char *s) {
	return (ao_lisp_poly) s + AO_LISP_STRING;
}

static inline struct ao_lisp_atom *
ao_lisp_poly_atom(ao_lisp_poly poly)
{
	return (struct ao_lisp_atom *) (poly - AO_LISP_OTHER);
}

static inline ao_lisp_poly
ao_lisp_atom_poly(struct ao_lisp_atom *a)
{
	return (ao_lisp_poly) a + AO_LISP_OTHER;
}

static inline struct ao_lisp_builtin *
ao_lisp_poly_builtin(ao_lisp_poly poly)
{
	return (struct ao_lisp_builtin *) (poly - AO_LISP_OTHER);
}

static inline ao_lisp_poly
ao_lisp_builtin_poly(struct ao_lisp_builtin *b)
{
	return (ao_lisp_poly) b + AO_LISP_OTHER;
}

/* memory functions */

void
ao_lisp_mark(const struct ao_lisp_mem_type *type, void *addr);

/* returns 1 if the object was already marked */
int
ao_lisp_mark_memory(void *addr, int size);

void *
ao_lisp_move(const struct ao_lisp_mem_type *type, void *addr);

/* returns NULL if the object was already moved */
void *
ao_lisp_move_memory(void *addr, int size);

void *
ao_lisp_alloc(int size);

int
ao_lisp_root_add(const struct ao_lisp_mem_type *type, void *addr);

void
ao_lisp_root_clear(void *addr);

/* cons */
extern const struct ao_lisp_mem_type ao_lisp_cons_type;

struct ao_lisp_cons *
ao_lisp_cons(ao_lisp_poly car, struct ao_lisp_cons *cdr);

void
ao_lisp_cons_print(struct ao_lisp_cons *cons);

/* string */
extern const struct ao_lisp_mem_type ao_lisp_string_type;

char *
ao_lisp_string_new(int len);

char *
ao_lisp_string_copy(char *a);

char *
ao_lisp_string_cat(char *a, char *b);

void
ao_lisp_string_print(char *s);

/* atom */
extern const struct ao_lisp_mem_type ao_lisp_atom_type;

void
ao_lisp_atom_init(void);

void
ao_lisp_atom_print(struct ao_lisp_atom *atom);

struct ao_lisp_atom *
ao_lisp_atom_intern(char *name);

/* int */
void
ao_lisp_int_print(int i);

/* prim */
ao_lisp_poly
ao_lisp_poly_print(ao_lisp_poly p);

void
ao_lisp_poly_mark(ao_lisp_poly p);

ao_lisp_poly
ao_lisp_poly_move(ao_lisp_poly p);

/* eval */
ao_lisp_poly
ao_lisp_eval(ao_lisp_poly p);

/* builtin */
void
ao_lisp_builtin_print(struct ao_lisp_builtin *b);

/* read */
ao_lisp_poly
ao_lisp_read(void);

#endif /* _AO_LISP_H_ */
