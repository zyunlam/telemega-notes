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

#if !defined(AO_LISP_TEST) && !defined(AO_LISP_MAKE_CONST)
#include <ao.h>
#define AO_LISP_ALTOS	1
#endif

#include <stdint.h>
#include <string.h>
#include <stdio.h>

#ifdef AO_LISP_MAKE_CONST
#define AO_LISP_POOL_CONST	16384
extern uint8_t ao_lisp_const[AO_LISP_POOL_CONST];
#else
#include "ao_lisp_const.h"
#endif

/* Primitive types */
#define AO_LISP_CONS		0
#define AO_LISP_INT		1
#define AO_LISP_STRING		2
#define AO_LISP_OTHER		3

#define AO_LISP_TYPE_MASK	0x0003
#define AO_LISP_TYPE_SHIFT	2
#define AO_LISP_REF_MASK	0x7ffc
#define AO_LISP_CONST		0x8000

/* These have a type value at the start of the struct */
#define AO_LISP_ATOM		4
#define AO_LISP_BUILTIN		5
#define AO_LISP_NUM_TYPE	6

#define AO_LISP_NIL	0

#define AO_LISP_POOL	1024

extern uint8_t		ao_lisp_pool[AO_LISP_POOL];
extern uint16_t		ao_lisp_top;

#define AO_LISP_OOM		0x01
#define AO_LISP_DIVIDE_BY_ZERO	0x02
#define AO_LISP_INVALID		0x04

extern uint8_t		ao_lisp_exception;

typedef uint16_t	ao_poly;

static inline void *
ao_lisp_ref(ao_poly poly) {
	if (poly == AO_LISP_NIL)
		return NULL;
	if (poly & AO_LISP_CONST)
		return (void *) ((ao_lisp_const - 4) + (poly & AO_LISP_REF_MASK));
	else
		return (void *) ((ao_lisp_pool - 4) + (poly & AO_LISP_REF_MASK));
}

static inline ao_poly
ao_lisp_poly(const void *addr, ao_poly type) {
	const uint8_t	*a = addr;
	if (addr == NULL)
		return AO_LISP_NIL;
	if (ao_lisp_pool <= a && a < ao_lisp_pool + AO_LISP_POOL)
		return (a - (ao_lisp_pool - 4)) | type;
	else if (ao_lisp_const <= a && a <= ao_lisp_const + AO_LISP_POOL_CONST)
		return AO_LISP_CONST | (a - (ao_lisp_const - 4)) | type;
	else {
		ao_lisp_exception |= AO_LISP_INVALID;
		return AO_LISP_NIL;
	}
}

#define AO_LISP_POLY(addr, type) (((ao_lisp_pool <= ((uint8_t *) (a)) && \
				    ((uint8_t *) (a)) < ao_lisp_pool + AO_LISP_POOL) ? \
				   ((uint8_t *) (a) - (ao_lisp_pool - 4)) : \
				   (((uint8_t *) (a) - (ao_lisp_const - 4)) | AO_LISP_POOL_CONST)) | \
				  (type))

struct ao_lisp_type {
	void	(*mark)(void *addr);
	int	(*size)(void *addr);
	void	(*move)(void *addr);
};

struct ao_lisp_cons {
	ao_poly		car;
	ao_poly		cdr;
};

struct ao_lisp_atom {
	uint8_t		type;
	uint8_t		pad[1];
	ao_poly		val;
	ao_poly		next;
	char		name[];
};

#define AO_LISP_LAMBDA	0
#define AO_LISP_NLAMBDA	1
#define AO_LISP_MACRO	2
#define AO_LISP_LEXPR	3

struct ao_lisp_builtin {
	uint8_t		type;
	uint8_t		args;
	uint16_t	func;
};

enum ao_lisp_builtin_id {
	builtin_car,
	builtin_cdr,
	builtin_cons,
	builtin_quote,
	builtin_print,
	builtin_plus,
	builtin_minus,
	builtin_times,
	builtin_divide,
	builtin_mod,
	builtin_last
};

typedef ao_poly (*ao_lisp_func_t)(struct ao_lisp_cons *cons);

extern ao_lisp_func_t	ao_lisp_builtins[];

static inline ao_lisp_func_t
ao_lisp_func(struct ao_lisp_builtin *b)
{
	return ao_lisp_builtins[b->func];
}

static inline void *
ao_lisp_poly_other(ao_poly poly) {
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_other_poly(const void *other)
{
	return ao_lisp_poly(other, AO_LISP_OTHER);
}

static inline int
ao_lisp_mem_round(int size)
{
	return (size + 3) & ~3;
}

#define AO_LISP_OTHER_POLY(other) ((ao_poly)(other) + AO_LISP_OTHER)

static inline int ao_lisp_poly_type(ao_poly poly) {
	int	type = poly & 3;
	if (type == AO_LISP_OTHER)
		return *((uint8_t *) ao_lisp_poly_other(poly));
	return type;
}

static inline struct ao_lisp_cons *
ao_lisp_poly_cons(ao_poly poly)
{
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_cons_poly(struct ao_lisp_cons *cons)
{
	return ao_lisp_poly(cons, AO_LISP_CONS);
}

static inline int
ao_lisp_poly_int(ao_poly poly)
{
	return (int) poly >> AO_LISP_TYPE_SHIFT;
}

static inline ao_poly
ao_lisp_int_poly(int i)
{
	return ((ao_poly) i << 2) + AO_LISP_INT;
}

static inline char *
ao_lisp_poly_string(ao_poly poly)
{
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_string_poly(char *s)
{
	return ao_lisp_poly(s, AO_LISP_STRING);
}

static inline struct ao_lisp_atom *
ao_lisp_poly_atom(ao_poly poly)
{
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_atom_poly(struct ao_lisp_atom *a)
{
	return ao_lisp_poly(a, AO_LISP_OTHER);
}

static inline struct ao_lisp_builtin *
ao_lisp_poly_builtin(ao_poly poly)
{
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_builtin_poly(struct ao_lisp_builtin *b)
{
	return ao_lisp_poly(b, AO_LISP_OTHER);
}

/* memory functions */
void
ao_lisp_mark(const struct ao_lisp_type *type, void *addr);

/* returns 1 if the object was already marked */
int
ao_lisp_mark_memory(void *addr, int size);

void *
ao_lisp_move(const struct ao_lisp_type *type, void *addr);

/* returns NULL if the object was already moved */
void *
ao_lisp_move_memory(void *addr, int size);

void *
ao_lisp_alloc(int size);

int
ao_lisp_root_add(const struct ao_lisp_type *type, void *addr);

void
ao_lisp_root_clear(void *addr);

/* cons */
extern const struct ao_lisp_type ao_lisp_cons_type;

struct ao_lisp_cons *
ao_lisp_cons_cons(ao_poly car, struct ao_lisp_cons *cdr);

void
ao_lisp_cons_print(ao_poly);

/* string */
extern const struct ao_lisp_type ao_lisp_string_type;

char *
ao_lisp_string_new(int len);

char *
ao_lisp_string_copy(char *a);

char *
ao_lisp_string_cat(char *a, char *b);

void
ao_lisp_string_print(ao_poly s);

/* atom */
extern const struct ao_lisp_type ao_lisp_atom_type;

extern struct ao_lisp_atom *ao_lisp_atoms;

void
ao_lisp_atom_init(void);

void
ao_lisp_atom_print(ao_poly a);

struct ao_lisp_atom *
ao_lisp_atom_intern(char *name);

/* int */
void
ao_lisp_int_print(ao_poly i);

/* prim */
ao_poly
ao_lisp_poly_print(ao_poly p);

void
ao_lisp_poly_mark(ao_poly p);

ao_poly
ao_lisp_poly_move(ao_poly p);

/* eval */
ao_poly
ao_lisp_eval(ao_poly p);

/* builtin */
void
ao_lisp_builtin_print(ao_poly b);

/* read */
ao_poly
ao_lisp_read(void);

/* rep */
ao_poly
ao_lisp_read_eval_print(void);

#endif /* _AO_LISP_H_ */
