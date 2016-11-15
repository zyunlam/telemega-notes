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

/*
 * When building the constant table, it is the
 * pool for allocations.
 */

#include <stdlib.h>
uint8_t ao_lisp_const[AO_LISP_POOL_CONST] __attribute__((aligned(4)));
#define ao_lisp_pool ao_lisp_const
#undef AO_LISP_POOL
#define AO_LISP_POOL AO_LISP_POOL_CONST

#else

uint8_t	ao_lisp_pool[AO_LISP_POOL + AO_LISP_POOL_EXTRA] __attribute__((aligned(4)));

#endif

#if 0
#define MDBG_POOL
#endif

#if DBG_MEM
int dbg_move_depth;
int dbg_mem = DBG_MEM_START;
int dbg_validate = 0;

struct ao_lisp_record {
	struct ao_lisp_record		*next;
	const struct ao_lisp_type	*type;
	void				*addr;
	int				size;
};

static struct ao_lisp_record	*record_head, **record_tail;

static void
ao_lisp_record_free(struct ao_lisp_record *record)
{
	while (record) {
		struct ao_lisp_record *next = record->next;
		free(record);
		record = next;
	}
}

static void
ao_lisp_record_reset(void)
{
	ao_lisp_record_free(record_head);
	record_head = NULL;
	record_tail = &record_head;
}

static void
ao_lisp_record(const struct ao_lisp_type	*type,
	       void				*addr,
	       int				size)
{
	struct ao_lisp_record	*r = malloc(sizeof (struct ao_lisp_record));

	r->next = NULL;
	r->type = type;
	r->addr = addr;
	r->size = size;
	*record_tail = r;
	record_tail = &r->next;
}

static struct ao_lisp_record *
ao_lisp_record_save(void)
{
	struct ao_lisp_record *r = record_head;

	record_head = NULL;
	record_tail = &record_head;
	return r;
}

static void
ao_lisp_record_compare(char *where,
		       struct ao_lisp_record *a,
		       struct ao_lisp_record *b)
{
	while (a && b) {
		if (a->type != b->type || a->size != b->size) {
			printf("%s record difers %d %s %d -> %d %s %d\n",
			       where,
			       MDBG_OFFSET(a->addr),
			       a->type->name,
			       a->size,
			       MDBG_OFFSET(b->addr),
			       b->type->name,
			       b->size);
			ao_lisp_abort();
		}
		a = a->next;
		b = b->next;
	}
	if (a) {
		printf("%s record differs %d %s %d -> NULL\n",
		       where,
		       MDBG_OFFSET(a->addr),
		       a->type->name,
		       a->size);
		ao_lisp_abort();
	}
	if (b) {
		printf("%s record differs NULL -> %d %s %d\n",
		       where,
		       MDBG_OFFSET(b->addr),
		       b->type->name,
		       b->size);
		ao_lisp_abort();
	}
}

#else
#define ao_lisp_record_reset()
#endif

uint8_t	ao_lisp_exception;

struct ao_lisp_root {
	const struct ao_lisp_type	*type;
	void				**addr;
};

static struct ao_lisp_cons 	*save_cons[2];
static char			*save_string[2];
static ao_poly			save_poly[2];

static const struct ao_lisp_root	ao_lisp_root[] = {
	{
		.type = &ao_lisp_cons_type,
		.addr = (void **) &save_cons[0],
	},
	{
		.type = &ao_lisp_cons_type,
		.addr = (void **) &save_cons[1],
	},
	{
		.type = &ao_lisp_string_type,
		.addr = (void **) &save_string[0]
	},
	{
		.type = &ao_lisp_string_type,
		.addr = (void **) &save_string[1]
	},
	{
		.type = NULL,
		.addr = (void **) &save_poly[0]
	},
	{
		.type = NULL,
		.addr = (void **) &save_poly[1]
	},
	{
		.type = &ao_lisp_atom_type,
		.addr = (void **) &ao_lisp_atoms
	},
	{
		.type = &ao_lisp_frame_type,
		.addr = (void **) &ao_lisp_frame_global,
	},
	{
		.type = &ao_lisp_frame_type,
		.addr = (void **) &ao_lisp_frame_current,
	},
	{
		.type = &ao_lisp_stack_type,
		.addr = (void **) &ao_lisp_stack,
	},
	{
		.type = NULL,
		.addr = (void **) &ao_lisp_v,
	},
	{
		.type = &ao_lisp_cons_type,
		.addr = (void **) &ao_lisp_read_cons,
	},
	{
		.type = &ao_lisp_cons_type,
		.addr = (void **) &ao_lisp_read_cons_tail,
	},
	{
		.type = &ao_lisp_cons_type,
		.addr = (void **) &ao_lisp_read_stack,
	},
};

#define AO_LISP_ROOT	(sizeof (ao_lisp_root) / sizeof (ao_lisp_root[0]))

static const void ** const ao_lisp_cache[] = {
	(const void **) &ao_lisp_cons_free_list,
	(const void **) &ao_lisp_stack_free_list,
};

#define AO_LISP_CACHE	(sizeof (ao_lisp_cache) / sizeof (ao_lisp_cache[0]))

#define AO_LISP_BUSY_SIZE	((AO_LISP_POOL + 31) / 32)

static uint8_t	ao_lisp_busy[AO_LISP_BUSY_SIZE];
static uint8_t	ao_lisp_cons_note[AO_LISP_BUSY_SIZE];
static uint8_t	ao_lisp_cons_last[AO_LISP_BUSY_SIZE];
static uint8_t	ao_lisp_cons_noted;

uint16_t	ao_lisp_top;

struct ao_lisp_chunk {
	uint16_t		old_addr;
	union {
		uint16_t	size;
		uint16_t	new_addr;
	};
};

#define AO_LISP_NCHUNK	64

static struct ao_lisp_chunk ao_lisp_chunk[AO_LISP_NCHUNK];

/* Offset of an address within the pool. */
static inline uint16_t pool_offset(void *addr) {
#if DBG_MEM
	if (!AO_LISP_IS_POOL(addr))
		ao_lisp_abort();
#endif
	return ((uint8_t *) addr) - ao_lisp_pool;
}

/*
 * Convert back and forth between 'poly's used
 * as short addresses in the pool and addresses.
 * These are used in the chunk code.
 */
static inline ao_poly pool_poly(void *addr) {
#if DBG_MEM
	if (!AO_LISP_IS_POOL(addr))
		ao_lisp_abort();
#endif
	return ((uint8_t *) addr) - AO_LISP_POOL_BASE;
}

static inline void *pool_ref(ao_poly p) {
	return AO_LISP_POOL_BASE + p;
}

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

static int total_marked;

static void
note_cons(void *addr)
{
	if (AO_LISP_IS_POOL(addr)) {
		int	offset = pool_offset(addr);
		MDBG_MOVE("note cons %d\n", MDBG_OFFSET(addr));
		ao_lisp_cons_noted = 1;
		mark(ao_lisp_cons_note, offset);
	}
}

static uint16_t	chunk_low;
static uint16_t	chunk_first, chunk_last;

static void
note_chunk(uint16_t addr, uint16_t size)
{
	int i;

	if (addr < chunk_low)
		return;

	for (i = 0; i < AO_LISP_NCHUNK; i++) {
		if (ao_lisp_chunk[i].size && ao_lisp_chunk[i].old_addr == addr) {
#if DBG_MEM
			if (ao_lisp_chunk[i].size != size)
				ao_lisp_abort();
#endif
			return;
		}
		if (ao_lisp_chunk[i].old_addr > addr) {
			memmove(&ao_lisp_chunk[i+1],
				&ao_lisp_chunk[i],
				(AO_LISP_NCHUNK - (i+1)) * sizeof (struct ao_lisp_chunk));
			ao_lisp_chunk[i].size = 0;
		}
		if (ao_lisp_chunk[i].size == 0) {
			ao_lisp_chunk[i].old_addr = addr;
			ao_lisp_chunk[i].size = size;
			return;
		}
	}
}

/*
 * Walk all referenced objects calling functions on each one
 */

static void
walk(int (*visit_addr)(const struct ao_lisp_type *type, void **addr),
     int (*visit_poly)(ao_poly *p, uint8_t do_note_cons))
{
	int i;

	total_marked = 0;
	ao_lisp_record_reset();
	memset(ao_lisp_busy, '\0', sizeof (ao_lisp_busy));
	memset(ao_lisp_cons_note, '\0', sizeof (ao_lisp_cons_note));
	ao_lisp_cons_noted = 0;
	for (i = 0; i < (int) AO_LISP_ROOT; i++) {
		if (ao_lisp_root[i].type) {
			void **a = ao_lisp_root[i].addr, *v;
			if (a && (v = *a)) {
				MDBG_MOVE("root ptr %d\n", MDBG_OFFSET(v));
				visit_addr(ao_lisp_root[i].type, a);
			}
		} else {
			ao_poly *a = (ao_poly *) ao_lisp_root[i].addr, p;
			if (a && (p = *a)) {
				MDBG_MOVE("root poly %d\n", MDBG_OFFSET(ao_lisp_ref(p)));
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
				MDBG_MOVE("root cons %d\n", MDBG_OFFSET(v));
				visit_addr(&ao_lisp_cons_type, &v);
			}
		}
	}
}

#if MDBG_DUMP
static void
dump_busy(void)
{
	int	i;
	MDBG_MOVE("busy:");
	for (i = 0; i < ao_lisp_top; i += 4) {
		if ((i & 0xff) == 0) {
			MDBG_MORE("\n");
			MDBG_MOVE("%s", "");
		}
		else if ((i & 0x1f) == 0)
			MDBG_MORE(" ");
		if (busy(ao_lisp_busy, i))
			MDBG_MORE("*");
		else
			MDBG_MORE("-");
	}
	MDBG_MORE ("\n");
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

int ao_lisp_collects;

void
ao_lisp_collect(void)
{
	int	i;
	int	top;
#if DBG_MEM
	int	loops = 0;
	int	marked;
	int	moved;
	struct ao_lisp_record	*mark_record = NULL, *move_record = NULL;

	MDBG_MOVE("collect %d\n", ao_lisp_collects);
	marked = moved = 0;
#endif

	++ao_lisp_collects;

	/* Clear references to all caches */
	for (i = 0; i < (int) AO_LISP_CACHE; i++)
		*ao_lisp_cache[i] = NULL;
	chunk_low = 0;
	top = 0;
	for (;;) {
		MDBG_DO(loops++);
		MDBG_MOVE("move chunks from %d to %d\n", chunk_low, top);
		/* Find the sizes of the first chunk of objects to move */
		memset(ao_lisp_chunk, '\0', sizeof (ao_lisp_chunk));
		walk(ao_lisp_mark_ref, ao_lisp_poly_mark_ref);
#if DBG_MEM
		marked = total_marked;

		ao_lisp_record_free(mark_record);
		mark_record = ao_lisp_record_save();
		if (mark_record && move_record)
			ao_lisp_record_compare("mark", move_record, mark_record);

		if (moved && moved != marked)
			ao_lisp_abort();
#endif

		DUMP_BUSY();

		/* Find the first moving object */
		for (i = 0; i < AO_LISP_NCHUNK; i++) {
			uint16_t	size = ao_lisp_chunk[i].size;

			if (!size)
				break;

			if (ao_lisp_chunk[i].old_addr > top)
				break;
#if DBG_MEM
			if (ao_lisp_chunk[i].old_addr != top)
				ao_lisp_abort();
#endif

			top += size;
			MDBG_MOVE("chunk %d %d not moving\n",
				  ao_lisp_chunk[i].old_addr,
				  ao_lisp_chunk[i].size);
			chunk_low = ao_lisp_chunk[i].old_addr + size;
		}

		chunk_first = i;
		/* Copy all of the objects */
		for (; i < AO_LISP_NCHUNK; i++) {
			uint16_t	size = ao_lisp_chunk[i].size;

			if (!size)
				break;

			MDBG_MOVE("chunk %d %d -> %d\n",
				  ao_lisp_chunk[i].old_addr,
				  size,
				  top);
			ao_lisp_chunk[i].new_addr = top;
			memmove(&ao_lisp_pool[top],
				&ao_lisp_pool[ao_lisp_chunk[i].old_addr],
				size);
			top += size;
			chunk_low = ao_lisp_chunk[i].old_addr + size;
		}

		chunk_last = i;

		if (chunk_first < chunk_last) {
			/* Relocate all references to the objects */
			walk(ao_lisp_move, ao_lisp_poly_move);

#if DBG_MEM
			ao_lisp_record_free(move_record);
			move_record = ao_lisp_record_save();
			if (mark_record && move_record)
				ao_lisp_record_compare("move", mark_record, move_record);

			moved = total_marked;
			if (moved != marked)
				ao_lisp_abort();
#endif
		}

		if (chunk_last != AO_LISP_NCHUNK)
			break;
	}
	ao_lisp_top = top;

	MDBG_DO(memset(ao_lisp_chunk, '\0', sizeof (ao_lisp_chunk));
		walk(ao_lisp_mark_ref, ao_lisp_poly_mark_ref));

//	printf ("collect. top %d loops %d\n", top, loops);
}

/*
 * Mark interfaces for objects
 *
 * Note a reference to memory and
 * collect information about a few object sizes
 * at a time
 */

int
ao_lisp_mark_memory(const struct ao_lisp_type *type, void *addr)
{
	int offset;
	if (!AO_LISP_IS_POOL(addr))
		return 1;

	offset = pool_offset(addr);
	MDBG_MOVE("mark memory %d\n", MDBG_OFFSET(addr));
	if (busy(ao_lisp_busy, offset)) {
		MDBG_MOVE("already marked\n");
		return 1;
	}
	mark(ao_lisp_busy, offset);
	note_chunk(offset, ao_lisp_size(type, addr));
	return 0;
}

int
ao_lisp_mark(const struct ao_lisp_type *type, void *addr)
{
	int ret;
	MDBG_MOVE("mark %d\n", MDBG_OFFSET(addr));
	MDBG_MOVE_IN();
	ret = ao_lisp_mark_memory(type, addr);
	if (!ret) {
		MDBG_MOVE("mark recurse\n");
		type->mark(addr);
	}
	MDBG_MOVE_OUT();
	return ret;
}

int
ao_lisp_poly_mark(ao_poly p, uint8_t do_note_cons)
{
	uint8_t type;
	void	*addr;

	if (!p)
		return 1;

	type = ao_lisp_poly_base_type(p);
	addr = ao_lisp_ref(p);

	if (!AO_LISP_IS_POOL(addr))
		return 1;

	if (type == AO_LISP_CONS && do_note_cons) {
		note_cons(ao_lisp_ref(p));
		return 1;
	} else {
		const struct ao_lisp_type	*lisp_type;

		if (type == AO_LISP_OTHER) {
			type = ao_lisp_other_type(ao_lisp_poly_other(p));
#if DBG_MEM
			if (type <= AO_LISP_OTHER || AO_LISP_NUM_TYPE <= type)
				ao_lisp_abort();
#endif
		}

		lisp_type = ao_lisp_types[ao_lisp_poly_type(p)];
		if (!lisp_type)
			return 1;
		return ao_lisp_mark(lisp_type, ao_lisp_ref(p));
	}
}

static void *
move_map(void *addr)
{
	uint16_t	offset = pool_offset(addr);
	int		i;

	for (i = chunk_first; i < chunk_last; i++) {
		if (ao_lisp_chunk[i].old_addr == offset) {
			MDBG_MOVE("move %d -> %d\n",
				  ao_lisp_chunk[i].old_addr,
				  ao_lisp_chunk[i].new_addr);
			return ao_lisp_pool + ao_lisp_chunk[i].new_addr;
		}
	}
	return addr;
}

int
ao_lisp_move_memory(const struct ao_lisp_type *type, void **ref)
{
	void		*addr = *ref;
	int		offset;

	if (!AO_LISP_IS_POOL(addr))
		return 1;

	(void) type;

	MDBG_MOVE("move memory %d\n", MDBG_OFFSET(addr));
	addr = move_map(addr);
	if (addr != *ref) {
		MDBG_MOVE("update ref %d %d -> %d\n",
			  AO_LISP_IS_POOL(ref) ? MDBG_OFFSET(ref) : -1,
			  MDBG_OFFSET(*ref), MDBG_OFFSET(addr));
		*ref = addr;
	}
	offset = pool_offset(addr);
	if (busy(ao_lisp_busy, offset)) {
		MDBG_MOVE("already moved\n");
		return 1;
	}
	mark(ao_lisp_busy, offset);
	MDBG_DO(ao_lisp_record(type, addr, ao_lisp_size(type, addr)));
	return 0;
}

int
ao_lisp_move(const struct ao_lisp_type *type, void **ref)
{
	int ret;
	MDBG_MOVE("move object %d\n", MDBG_OFFSET(*ref));
	MDBG_MOVE_IN();
	ret = ao_lisp_move_memory(type, ref);
	if (!ret) {
		MDBG_MOVE("move recurse\n");
		type->move(*ref);
	}
	MDBG_MOVE_OUT();
	return ret;
}

int
ao_lisp_poly_move(ao_poly *ref, uint8_t do_note_cons)
{
	uint8_t				type;
	ao_poly				p = *ref;
	int				ret;
	void				*addr;

	if (!p)
		return 1;

	addr = ao_lisp_ref(p);

	if (!AO_LISP_IS_POOL(addr))
		return 1;

	type = ao_lisp_poly_base_type(p);

	if (type == AO_LISP_CONS && do_note_cons) {
		note_cons(addr);
		addr = move_map(addr);
		ret = 1;
	} else {
		const struct ao_lisp_type	*lisp_type;

		if (type == AO_LISP_OTHER) {
			type = ao_lisp_other_type(move_map(ao_lisp_poly_other(p)));
#if DBG_MEM
			if (type <= AO_LISP_OTHER || AO_LISP_NUM_TYPE <= type)
				ao_lisp_abort();
#endif
		}

		lisp_type = ao_lisp_types[type];
		if (!lisp_type)
			return 1;
		ret = ao_lisp_move(lisp_type, &addr);
	}

	/* Re-write the poly value */
	if (addr != ao_lisp_ref(p)) {
		ao_poly np = ao_lisp_poly(addr, p & AO_LISP_TYPE_MASK);
		MDBG_MOVE("poly %d moved %d -> %d\n",
			  type, MDBG_OFFSET(ao_lisp_ref(p)), MDBG_OFFSET(ao_lisp_ref(np)));
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

#if DBG_MEM
void
ao_lisp_validate(void)
{
	chunk_low = 0;
	memset(ao_lisp_chunk, '\0', sizeof (ao_lisp_chunk));
	walk(ao_lisp_mark_ref, ao_lisp_poly_mark_ref);
}

int dbg_allocs;

#endif


void *
ao_lisp_alloc(int size)
{
	void	*addr;

	MDBG_DO(++dbg_allocs);
	MDBG_DO(if (dbg_validate) ao_lisp_validate());
	size = ao_lisp_size_round(size);
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

void
ao_lisp_cons_stash(int id, struct ao_lisp_cons *cons)
{
	save_cons[id] = cons;
}

struct ao_lisp_cons *
ao_lisp_cons_fetch(int id)
{
	struct ao_lisp_cons *cons = save_cons[id];
	save_cons[id] = NULL;
	return cons;
}

void
ao_lisp_string_stash(int id, char *string)
{
	save_string[id] = string;
}

char *
ao_lisp_string_fetch(int id)
{
	char *string = save_string[id];
	save_string[id] = NULL;
	return string;
}
void
ao_lisp_poly_stash(int id, ao_poly poly)
{
	save_poly[id] = poly;
}

ao_poly
ao_lisp_poly_fetch(int id)
{
	ao_poly poly = save_poly[id];
	save_poly[id] = AO_LISP_NIL;
	return poly;
}
