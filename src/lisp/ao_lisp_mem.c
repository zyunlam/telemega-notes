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

#include "ao_lisp.h"
#include <stdio.h>

uint8_t	ao_lisp_pool[AO_LISP_POOL];

struct ao_lisp_root {
	void				**addr;
	const struct ao_lisp_mem_type	*type;
};

static struct ao_lisp_root	ao_lisp_root[AO_LISP_ROOT];

static uint8_t	ao_lisp_busy[AO_LISP_POOL / 32];

static uint8_t	ao_lisp_moving[AO_LISP_POOL / 32];

static uint16_t	ao_lisp_top;

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

static void	*move_old, *move_new;
static int	move_size;

static void
move_object(void)
{
	int	i;

	memset(ao_lisp_moving, '\0', sizeof (ao_lisp_moving));
	for (i = 0; i < AO_LISP_ROOT; i++)
		if (ao_lisp_root[i].addr) {
			void *new;
			new = ao_lisp_move(ao_lisp_root[i].type, *ao_lisp_root[i].addr);
			if (new)
				*ao_lisp_root[i].addr = new;
		}
}

static void
collect(void)
{
	int	i;

	printf("collect\n");
	/* Mark */
	memset(ao_lisp_busy, '\0', sizeof (ao_lisp_busy));
	for (i = 0; i < AO_LISP_ROOT; i++)
		if (ao_lisp_root[i].addr)
			ao_lisp_mark(ao_lisp_root[i].type, *ao_lisp_root[i].addr);

	/* Compact */
	ao_lisp_top = 0;
	for (i = 0; i < AO_LISP_POOL; i += 4) {
		if (!busy(ao_lisp_busy, i))
			break;
	}
	ao_lisp_top = i;
	while(i < AO_LISP_POOL) {
		if (busy(ao_lisp_busy, i)) {
			move_old = &ao_lisp_pool[i];
			move_new = &ao_lisp_pool[ao_lisp_top];
			move_size = 0;
			move_object();
			clear_object(ao_lisp_busy, move_old, move_size);
			i += move_size;
			ao_lisp_top += move_size;
		} else {
			i += 4;
		}
	}
}


void
ao_lisp_mark(const struct ao_lisp_mem_type *type, void *addr)
{
	if (mark_object(ao_lisp_busy, addr, type->size(addr)))
		return;
	type->mark(addr);
}

int
ao_lisp_mark_memory(void *addr, int size)
{
	return mark_object(ao_lisp_busy, addr, size);
}

static void *
check_move(void *addr, int size)
{
	if (addr == move_old) {
		memmove(move_new, move_old, size);
		move_size = (size + 3) & ~3;
		addr = move_new;
	}
	return addr;
}

void *
ao_lisp_move(const struct ao_lisp_mem_type *type, void *addr)
{
	int	size = type->size(addr);

	if (!addr)
		return NULL;

	addr = check_move(addr, size);
	if (mark_object(ao_lisp_moving, addr, size))
		return addr;
	type->move(addr);
	return addr;
}

void *
ao_lisp_move_memory(void *addr, int size)
{
	if (!addr)
		return NULL;

	addr = check_move(addr, size);
	if (mark_object(ao_lisp_moving, addr, size))
		return NULL;
	return addr;
}

void *
ao_lisp_alloc(int size)
{
	void	*addr;

	size = (size + 3) & ~3;
	if (ao_lisp_top + size > AO_LISP_POOL) {
		collect();
		if (ao_lisp_top + size > AO_LISP_POOL)
			return NULL;
	}
	addr = ao_lisp_pool + ao_lisp_top;
	ao_lisp_top += size;
	return addr;
}

int
ao_lisp_root_add(const struct ao_lisp_mem_type *type, void *addr)
{
	int	i;
	for (i = 0; i < AO_LISP_ROOT; i++) {
		if (!ao_lisp_root[i].addr) {
			ao_lisp_root[i].addr = addr;
			ao_lisp_root[i].type = type;
			return 1;
		}
	}
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
