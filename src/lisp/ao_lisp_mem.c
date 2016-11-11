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

#define AO_LISP_CONST_BITS

#include "ao_lisp.h"
#include <stdio.h>

#ifdef AO_LISP_MAKE_CONST
#include <stdlib.h>
uint8_t ao_lisp_const[AO_LISP_POOL_CONST] __attribute__((aligned(4)));
#define ao_lisp_pool ao_lisp_const
#undef AO_LISP_POOL
#define AO_LISP_POOL AO_LISP_POOL_CONST
#else
uint8_t	ao_lisp_pool[AO_LISP_POOL] __attribute__((aligned(4)));
#endif

#if 0
#define MDBG_COLLECT_ALWAYS
#endif

#if 0
#define MDBG_POOL
#endif

#if 0
#define MDBG_INCLUDE
#define MDBG_DUMP	1
#define MDBG_MOVE	0
#define MDBG_OFFSET(a)	((int) ((uint8_t *) (a) - ao_lisp_pool))
#define MDBG(...) printf(__VA_ARGS__)
#define MDBG_DO(a)	a
static int move_dump = MDBG_MOVE;
static int move_depth;
#define MDBG_RESET() (move_depth = 0)
#define MDBG_MOVE(...) do { if(move_dump) { int d; for (d = 0; d < move_depth; d++) printf ("  "); printf(__VA_ARGS__); } } while (0)
#define MDBG_MOVE_IN()	(move_depth++)
#define MDBG_MOVE_OUT()	(move_depth--)
#else
#define MDBG(...)
#define MDBG_DO(a)
#define MDBG_RESET()
#define MDBG_MOVE(...)
#define MDBG_MOVE_IN()
#define MDBG_MOVE_OUT()
#endif

uint8_t	ao_lisp_exception;

struct ao_lisp_root {
	void				**addr;
	const struct ao_lisp_type	*type;
};

#define AO_LISP_ROOT	16

static struct ao_lisp_root	ao_lisp_root[AO_LISP_ROOT];

static uint8_t	ao_lisp_busy[AO_LISP_POOL / 32];
static uint8_t	ao_lisp_moving[AO_LISP_POOL / 32];
static uint8_t	ao_lisp_cons_note[AO_LISP_POOL / 32];
static uint8_t	ao_lisp_cons_last[AO_LISP_POOL / 32];
static uint8_t	ao_lisp_cons_noted;

uint16_t	ao_lisp_top;

static inline void mark(uint8_t *tag, int offset) {
	int	byte = offset >> 5;
	int	bit = (offset >> 2) & 7;
	tag[byte] |= (1 << bit);
}

static inline void clear(uint8_t *tag, int offset) {
	int	byte = offset >> 5;
	int	bit = (offset >> 2) & 7;
	tag[byte] &= ~(1 << bit);
}

static inline int busy(uint8_t *tag, int offset) {
	int	byte = offset >> 5;
	int	bit = (offset >> 2) & 7;
	return (tag[byte] >> bit) & 1;
}

static inline int min(int a, int b) { return a < b ? a : b; }
static inline int max(int a, int b) { return a > b ? a : b; }

static inline int limit(int offset) {
	return min(AO_LISP_POOL, max(offset, 0));
}

static int
mark_object(uint8_t *tag, void *addr, int size) {
	int	base;
	int	bound;

	if (!addr)
		return 1;

	if ((uint8_t *) addr < ao_lisp_pool || ao_lisp_pool + AO_LISP_POOL <= (uint8_t*) addr)
		return 1;

	base = (uint8_t *) addr - ao_lisp_pool;
	bound = base + size;

	base = limit(base);
	bound = limit(bound);
	if (busy(tag, base))
		return 1;
	while (base < bound) {
		mark(tag, base);
		base += 4;
	}
	return 0;
}

static int
clear_object(uint8_t *tag, void *addr, int size) {
	int	base;
	int	bound;
	if (!addr)
		return 1;

	base = (uint8_t *) addr - ao_lisp_pool;
	bound = base + size;

	base = limit(base);
	bound = limit(bound);
	if (!busy(tag, base))
		return 1;
	while (base < bound) {
		clear(tag, base);
		base += 4;
	}
	return 0;
}

static int
busy_object(uint8_t *tag, void *addr) {
	int	base;

	if (!addr)
		return 1;

	if ((uint8_t *) addr < ao_lisp_pool || ao_lisp_pool + AO_LISP_POOL <= (uint8_t*) addr)
		return 1;

	base = (uint8_t *) addr - ao_lisp_pool;
	base = limit(base);
	if (busy(tag, base))
		return 1;
	return 0;
}

static void
note_cons(void *addr)
{
	MDBG_MOVE("note cons %d\n", MDBG_OFFSET(addr));
	if (AO_LISP_IS_POOL(addr)) {
		int	offset = (uint8_t *) addr - ao_lisp_pool;
		ao_lisp_cons_noted = 1;
		mark(ao_lisp_cons_note, offset);
	}
}

/*
 * Walk all referenced objects calling functions on each one
 */

static void
walk_all(uint8_t *tag,
	 int (*visit_addr)(const struct ao_lisp_type *type, void **addr),
	 int (*visit_poly)(ao_poly *p, uint8_t do_note_cons))
{
	int i;

	memset(tag, '\0', sizeof (ao_lisp_busy));
	memset(ao_lisp_cons_note, '\0', sizeof (ao_lisp_cons_note));
	ao_lisp_cons_noted = 0;
	for (i = 0; i < AO_LISP_ROOT; i++) {
		if (ao_lisp_root[i].type) {
			void **a = ao_lisp_root[i].addr, *v;
			if (a && (v = *a)) {
				MDBG("root %d\n", MDBG_OFFSET(v));
				visit_addr(ao_lisp_root[i].type, a);
			}
		} else {
			ao_poly *a = (ao_poly *) ao_lisp_root[i].addr, p;
			if (a && (p = *a)) {
				MDBG("root 0x%04x\n", p);
				visit_poly(a, 0);
			}
		}
	}
	while (ao_lisp_cons_noted) {
		memcpy(ao_lisp_cons_last, ao_lisp_cons_note, sizeof (ao_lisp_cons_note));
		memset(ao_lisp_cons_note, '\0', sizeof (ao_lisp_cons_note));
		ao_lisp_cons_noted = 0;
		for (i = 0; i < AO_LISP_POOL; i += 4) {
			if (busy(ao_lisp_cons_last, i)) {
				void *v = ao_lisp_pool + i;
				MDBG("cons %d\n", MDBG_OFFSET(v));
				visit_addr(&ao_lisp_cons_type, &v);
			}
		}
	}
}

static void	*move_old, *move_new;
static int	move_size;

static void
move_object(void)
{
	walk_all(ao_lisp_moving, ao_lisp_move, ao_lisp_poly_move);
}

#if MDBG_DUMP
static void
dump_busy(void)
{
	int	i;
	printf("busy:");
	for (i = 0; i < ao_lisp_top; i += 4) {
		if ((i & 0xff) == 0)
			printf("\n");
		else if ((i & 0x1f) == 0)
			printf(" ");
		if (busy(ao_lisp_busy, i))
			putchar('*');
		else
			putchar('-');
	}
	printf ("\n");
}
#define DUMP_BUSY()	dump_busy()
#else
#define DUMP_BUSY()
#endif

static const struct ao_lisp_type const *ao_lisp_types[AO_LISP_NUM_TYPE] = {
	[AO_LISP_CONS] = &ao_lisp_cons_type,
	[AO_LISP_INT] = NULL,
	[AO_LISP_STRING] = &ao_lisp_string_type,
	[AO_LISP_OTHER] = (void *) 0x1,
	[AO_LISP_ATOM] = &ao_lisp_atom_type,
	[AO_LISP_BUILTIN] = &ao_lisp_builtin_type,
	[AO_LISP_FRAME] = &ao_lisp_frame_type,
	[AO_LISP_LAMBDA] = &ao_lisp_lambda_type,
};

static int
ao_lisp_mark_ref(const struct ao_lisp_type *type, void **ref)
{
	return ao_lisp_mark(type, *ref);
}

static int
ao_lisp_poly_mark_ref(ao_poly *p, uint8_t do_note_cons)
{
	return ao_lisp_poly_mark(*p, do_note_cons);
}

static void
ao_lisp_mark_busy(void)
{
	walk_all(ao_lisp_busy, ao_lisp_mark_ref, ao_lisp_poly_mark_ref);
}

void
ao_lisp_collect(void)
{
	int	i;
	int	top;

	MDBG("collect\n");
	/* Mark */
	ao_lisp_mark_busy();

	DUMP_BUSY();
	/* Compact */
	MDBG("find first busy\n");
	for (i = 0; i < ao_lisp_top; i += 4) {
		if (!busy(ao_lisp_busy, i))
			break;
	}
	top = i;
	while(i < ao_lisp_top) {
		if (busy(ao_lisp_busy, i)) {
			MDBG("busy %d -> %d\n", i, top);
			move_old = &ao_lisp_pool[i];
			move_new = &ao_lisp_pool[top];
			move_size = 0;
			move_object();
			MDBG("\tbusy size %d\n", move_size);
			if (move_size == 0)
				ao_lisp_abort();
			clear_object(ao_lisp_busy, move_old, move_size);
			mark_object(ao_lisp_busy, move_new, move_size);
			if (busy_object(ao_lisp_cons_note, move_old)) {
				clear_object(ao_lisp_cons_note, move_old, move_size);
				mark_object(ao_lisp_cons_note, move_new, move_size);
			}
			i += move_size;
			top += move_size;
#if MDBG_MOVE
			DUMP_BUSY();
#endif
		} else {
			i += 4;
		}
	}
	ao_lisp_top = top;
}


int
ao_lisp_mark(const struct ao_lisp_type *type, void *addr)
{
	if (!addr)
		return 1;
	if (mark_object(ao_lisp_busy, addr, type->size(addr)))
		return 1;
	type->mark(addr);
	return 0;
}

int
ao_lisp_poly_mark(ao_poly p, uint8_t do_note_cons)
{
	uint8_t type = ao_lisp_poly_type(p);

	if (!p)
		return 1;
	if (type == AO_LISP_CONS && do_note_cons) {
		note_cons(ao_lisp_ref(p));
		return 0;
	} else {
		const struct ao_lisp_type *lisp_type = ao_lisp_types[ao_lisp_poly_type(p)];
		if (lisp_type)
			return ao_lisp_mark(lisp_type, ao_lisp_ref(p));
		return 1;
	}
}

int
ao_lisp_mark_memory(void *addr, int size)
{
	return mark_object(ao_lisp_busy, addr, size);
}

/*
 * After the object has been moved, we have to reference it
 * in the new location. This is only relevant for ao_lisp_poly_move
 * as it needs to fetch the type byte from the object, which
 * may have been overwritten by the copy
 */
void *
ao_lisp_move_map(void *addr)
{
	if (addr == move_old) {
		if (busy_object(ao_lisp_moving, addr))
			return move_new;
	}
	return addr;
}

static void *
check_move(void *addr, int size)
{
	if (addr == move_old) {
		MDBG_MOVE("mapping %d -> %d\n", MDBG_OFFSET(addr), MDBG_OFFSET(move_new));
		if (!busy_object(ao_lisp_moving, addr)) {
			MDBG_MOVE("  copy %d\n", size);
			memmove(move_new, move_old, size);
			move_size = (size + 3) & ~3;
		}
		addr = move_new;
	}
	return addr;
}

int
ao_lisp_move(const struct ao_lisp_type *type, void **ref)
{
	void		*addr = *ref;
	uint8_t		*a = addr;
	int		size = type->size(addr);

	if (!addr)
		return 1;

#ifndef AO_LISP_MAKE_CONST
	if (AO_LISP_IS_CONST(addr))
		return 1;
#endif
	MDBG_MOVE("object %d\n", MDBG_OFFSET(addr));
	if (!AO_LISP_IS_POOL(a))
		ao_lisp_abort();
	MDBG_MOVE_IN();
	addr = check_move(addr, size);
	if (addr != *ref)
		*ref = addr;
	if (mark_object(ao_lisp_moving, addr, size)) {
		MDBG_MOVE("already moved\n");
		MDBG_MOVE_OUT();
		return 1;
	}
	MDBG_MOVE_OUT();
	MDBG_MOVE("recursing...\n");
	MDBG_MOVE_IN();
	type->move(addr);
	MDBG_MOVE_OUT();
	MDBG_MOVE("done %d\n", MDBG_OFFSET(addr));
	return 0;
}

int
ao_lisp_move_memory(void **ref, int size)
{
	void *addr = *ref;
	if (!addr)
		return 1;

	MDBG_MOVE("memory %d\n", MDBG_OFFSET(addr));
	MDBG_MOVE_IN();
	addr = check_move(addr, size);
	if (addr != *ref)
		*ref = addr;
	if (mark_object(ao_lisp_moving, addr, size)) {
		MDBG_MOVE("already moved\n");
		MDBG_MOVE_OUT();
		return 1;
	}
	MDBG_MOVE_OUT();
	return 0;
}

int
ao_lisp_poly_move(ao_poly *ref, uint8_t do_note_cons)
{
	uint8_t				type;
	ao_poly				p = *ref;
	const struct ao_lisp_type	*lisp_type;
	int				ret;
	void				*addr;

	if (!p)
		return 1;

	type = ao_lisp_poly_base_type(p);
	addr = ao_lisp_ref(p);
	if (type == AO_LISP_CONS && do_note_cons) {
		note_cons(addr);
		addr = check_move(addr, sizeof (struct ao_lisp_cons));
		ret = 1;
	} else {

		if (type == AO_LISP_OTHER)
			type = ao_lisp_other_type(ao_lisp_move_map(ao_lisp_poly_other(p)));

		if (type >= AO_LISP_NUM_TYPE)
			ao_lisp_abort();

		lisp_type = ao_lisp_types[type];
		if (!lisp_type)
			return 1;
		ret = ao_lisp_move(lisp_type, &addr);
	}

	if (addr != ao_lisp_ref(p)) {
		ao_poly np = ao_lisp_poly(addr, p & AO_LISP_TYPE_MASK);
		MDBG("poly %d moved %04x -> %04x\n",
		    type, p, np);
		*ref = np;
	}
	return ret;
}

#ifdef MDBG_POOL
static int AO_LISP_POOL_CUR = AO_LISP_POOL / 8;

static void
ao_lisp_poison(void)
{
	int	i;

	printf("poison\n");
	ao_lisp_mark_busy();
	for (i = 0; i < AO_LISP_POOL_CUR; i += 4) {
		uint32_t	*a = (uint32_t *) &ao_lisp_pool[i];
		if (!busy_object(ao_lisp_busy, a))
			*a = 0xBEEFBEEF;
	}
	for (i = 0; i < AO_LISP_POOL_CUR; i += 2) {
		ao_poly		*a = (uint16_t *) &ao_lisp_pool[i];
		ao_poly		p = *a;

		if (!ao_lisp_is_const(p)) {
			void	*r = ao_lisp_ref(p);

			if (ao_lisp_pool <= (uint8_t *) r &&
			    (uint8_t *) r <= ao_lisp_pool + AO_LISP_POOL_CUR)
			{
				if (!busy_object(ao_lisp_busy, r)) {
					printf("missing reference from %d to %d\n",
					       (int) ((uint8_t *) a - ao_lisp_pool),
					       (int) ((uint8_t *) r - ao_lisp_pool));
				}
			}
		}
	}
}

#else
#define AO_LISP_POOL_CUR AO_LISP_POOL
#endif

void *
ao_lisp_alloc(int size)
{
	void	*addr;

	size = ao_lisp_mem_round(size);
#ifdef MDBG_COLLECT_ALWAYS
	ao_lisp_collect();
#endif
	if (ao_lisp_top + size > AO_LISP_POOL_CUR) {
#ifdef MDBG_POOL
		if (AO_LISP_POOL_CUR < AO_LISP_POOL) {
			AO_LISP_POOL_CUR += AO_LISP_POOL / 8;
			ao_lisp_poison();
		} else
#endif
		ao_lisp_collect();
#ifdef MDBG_POOL
		{
			int	i;

			for (i = ao_lisp_top; i < AO_LISP_POOL; i += 4) {
				uint32_t	*p = (uint32_t *) &ao_lisp_pool[i];
				*p = 0xbeefbeef;
			}
		}
#endif

		if (ao_lisp_top + size > AO_LISP_POOL) {
			ao_lisp_error(AO_LISP_OOM, "out of memory");
			return NULL;
		}
	}
	addr = ao_lisp_pool + ao_lisp_top;
	ao_lisp_top += size;
	return addr;
}

int
ao_lisp_root_add(const struct ao_lisp_type *type, void *addr)
{
	int	i;
	MDBG("add root type %p addr %p\n", type, addr);
	for (i = 0; i < AO_LISP_ROOT; i++) {
		if (!ao_lisp_root[i].addr) {
			ao_lisp_root[i].addr = addr;
			ao_lisp_root[i].type = type;
			return 1;
		}
	}
	ao_lisp_abort();
	return 0;
}

int
ao_lisp_root_poly_add(ao_poly *p)
{
	return ao_lisp_root_add(NULL, p);
}

void
ao_lisp_root_clear(void *addr)
{
	int	i;
	for (i = 0; i < AO_LISP_ROOT; i++) {
		if (ao_lisp_root[i].addr == addr) {
			ao_lisp_root[i].addr = 0;
			ao_lisp_root[i].type = 0;
			break;
		}
	}
}
