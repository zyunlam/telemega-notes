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
#define DBG_DUMP
#define DBG_OFFSET(a)	((int) ((uint8_t *) (a) - ao_lisp_pool))
#define DBG(...) printf(__VA_ARGS__)
static int move_dump;
static int move_depth;
#define DBG_RESET() (move_depth = 0)
#define DBG_MOVE(...) do { if(move_dump) { int d; for (d = 0; d < move_depth; d++) printf ("  "); printf(__VA_ARGS__); } } while (0)
#define DBG_MOVE_IN()	(move_depth++)
#define DBG_MOVE_OUT()	(move_depth--)
#else
#define DBG(...)
#define DBG_RESET()
#define DBG_MOVE(...)
#define DBG_MOVE_IN()
#define DBG_MOVE_OUT()
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

static void	*move_old, *move_new;
static int	move_size;

static void
move_object(void)
{
	int	i;

	DBG_RESET();
	DBG_MOVE("move %d -> %d\n", DBG_OFFSET(move_old), DBG_OFFSET(move_new));
	DBG_MOVE_IN();
	memset(ao_lisp_moving, '\0', sizeof (ao_lisp_moving));
	for (i = 0; i < AO_LISP_ROOT; i++)
		if (ao_lisp_root[i].addr && *ao_lisp_root[i].addr) {
			void *new;
			DBG_MOVE("root %d\n", DBG_OFFSET(*ao_lisp_root[i].addr));
			new = ao_lisp_move(ao_lisp_root[i].type, *ao_lisp_root[i].addr);
			if (new)
				*ao_lisp_root[i].addr = new;
		}
	DBG_MOVE_OUT();
	DBG_MOVE("move done\n");
}

#ifdef DBG_DUMP
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

void
ao_lisp_collect(void)
{
	int	i;
	int	top;

	/* Mark */
	memset(ao_lisp_busy, '\0', sizeof (ao_lisp_busy));
	DBG("mark\n");
	for (i = 0; i < AO_LISP_ROOT; i++)
		if (ao_lisp_root[i].addr && *ao_lisp_root[i].addr) {
			DBG("root %p\n", *ao_lisp_root[i].addr);
			ao_lisp_mark(ao_lisp_root[i].type, *ao_lisp_root[i].addr);
		}

	DUMP_BUSY();
	/* Compact */
	DBG("find first busy\n");
	for (i = 0; i < ao_lisp_top; i += 4) {
		if (!busy(ao_lisp_busy, i))
			break;
	}
	top = i;
	while(i < ao_lisp_top) {
		if (busy(ao_lisp_busy, i)) {
			DBG("busy %d -> %d\n", i, top);
			move_old = &ao_lisp_pool[i];
			move_new = &ao_lisp_pool[top];
			move_size = 0;
			move_object();
			DBG("\tbusy size %d\n", move_size);
			if (move_size == 0)
				abort();
			clear_object(ao_lisp_busy, move_old, move_size);
			mark_object(ao_lisp_busy, move_new, move_size);
			i += move_size;
			top += move_size;
			DUMP_BUSY();
		} else {
			i += 4;
		}
	}
	ao_lisp_top = top;
}


void
ao_lisp_mark(const struct ao_lisp_type *type, void *addr)
{
	if (!addr)
		return;
	if (mark_object(ao_lisp_busy, addr, type->size(addr)))
		return;
	type->mark(addr);
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
		DBG_MOVE("mapping %d -> %d\n", DBG_OFFSET(addr), DBG_OFFSET(move_new));
		if (!busy_object(ao_lisp_moving, addr)) {
			DBG_MOVE("  copy %d\n", size);
			memmove(move_new, move_old, size);
			move_size = (size + 3) & ~3;
		}
		addr = move_new;
	}
	return addr;
}

void *
ao_lisp_move(const struct ao_lisp_type *type, void *addr)
{
	uint8_t *a = addr;
	int	size = type->size(addr);

	if (!addr)
		return NULL;

#ifndef AO_LISP_MAKE_CONST
	if (AO_LISP_IS_CONST(addr))
		return addr;
#endif
	DBG_MOVE("object %d\n", DBG_OFFSET(addr));
	if (a < ao_lisp_pool || ao_lisp_pool + AO_LISP_POOL <= a)
		abort();
	DBG_MOVE_IN();
	addr = check_move(addr, size);
	if (mark_object(ao_lisp_moving, addr, size)) {
		DBG_MOVE("already moved\n");
		DBG_MOVE_OUT();
		return addr;
	}
	DBG_MOVE_OUT();
	DBG_MOVE("recursing...\n");
	DBG_MOVE_IN();
	type->move(addr);
	DBG_MOVE_OUT();
	DBG_MOVE("done %d\n", DBG_OFFSET(addr));
	return addr;
}

void *
ao_lisp_move_memory(void *addr, int size)
{
	if (!addr)
		return NULL;

	DBG_MOVE("memory %d\n", DBG_OFFSET(addr));
	DBG_MOVE_IN();
	addr = check_move(addr, size);
	if (mark_object(ao_lisp_moving, addr, size)) {
		DBG_MOVE("already moved\n");
		DBG_MOVE_OUT();
		return addr;
	}
	DBG_MOVE_OUT();
	return addr;
}

void *
ao_lisp_alloc(int size)
{
	void	*addr;

	size = ao_lisp_mem_round(size);
	if (ao_lisp_top + size > AO_LISP_POOL) {
		ao_lisp_collect();
		if (ao_lisp_top + size > AO_LISP_POOL) {
			ao_lisp_exception |= AO_LISP_OOM;
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
	DBG("add root type %p addr %p\n", type, addr);
	for (i = 0; i < AO_LISP_ROOT; i++) {
		if (!ao_lisp_root[i].addr) {
			ao_lisp_root[i].addr = addr;
			ao_lisp_root[i].type = type;
			return 1;
		}
	}
	abort();
	return 0;
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
