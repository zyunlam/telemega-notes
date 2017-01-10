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

#ifndef DBG_MEM_STATS
#define DBG_MEM_STATS	DBG_MEM
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
static ao_poly			save_poly[3];

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
		.addr = (void **) &save_string[0],
	},
	{
		.type = &ao_lisp_string_type,
		.addr = (void **) &save_string[1],
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &save_poly[0]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &save_poly[1]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &save_poly[2]
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
		.addr = (void **) (void *) &ao_lisp_v,
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
	(const void **) &ao_lisp_frame_free_list[0],
	(const void **) &ao_lisp_frame_free_list[1],
	(const void **) &ao_lisp_frame_free_list[2],
	(const void **) &ao_lisp_frame_free_list[3],
	(const void **) &ao_lisp_frame_free_list[4],
	(const void **) &ao_lisp_frame_free_list[5],
};

#if AO_LISP_FRAME_FREE != 6
#error Unexpected AO_LISP_FRAME_FREE value
#endif

#define AO_LISP_CACHE	(sizeof (ao_lisp_cache) / sizeof (ao_lisp_cache[0]))

#define AO_LISP_BUSY_SIZE	((AO_LISP_POOL + 31) / 32)

static uint8_t	ao_lisp_busy[AO_LISP_BUSY_SIZE];
static uint8_t	ao_lisp_cons_note[AO_LISP_BUSY_SIZE];
static uint8_t	ao_lisp_cons_last[AO_LISP_BUSY_SIZE];
static uint8_t	ao_lisp_cons_noted;

uint16_t	ao_lisp_top;

struct ao_lisp_chunk {
	uint16_t		old_offset;
	union {
		uint16_t	size;
		uint16_t	new_offset;
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

static void
note_cons(uint16_t offset)
{
	MDBG_MOVE("note cons %d\n", offset);
	ao_lisp_cons_noted = 1;
	mark(ao_lisp_cons_note, offset);
}

static uint16_t	chunk_low, chunk_high;
static uint16_t	chunk_first, chunk_last;

static int
find_chunk(uint16_t offset)
{
	int l, r;
	/* Binary search for the location */
	l = chunk_first;
	r = chunk_last - 1;
	while (l <= r) {
		int m = (l + r) >> 1;
		if (ao_lisp_chunk[m].old_offset < offset)
			l = m + 1;
		else
			r = m - 1;
	}
	return l;
}

static void
note_chunk(uint16_t offset, uint16_t size)
{
	int l;

	if (offset < chunk_low || chunk_high <= offset)
		return;

	l = find_chunk(offset);

	/*
	 * The correct location is always in 'l', with r = l-1 being
	 * the entry before the right one
	 */

#if DBG_MEM
	/* Off the right side */
	if (l >= AO_LISP_NCHUNK)
		ao_lisp_abort();

	/* Off the left side */
	if (l == 0 && chunk_last && offset > ao_lisp_chunk[0].old_offset)
		ao_lisp_abort();
#endif

	/* Shuffle existing entries right */
	int end = min(AO_LISP_NCHUNK, chunk_last + 1);

	memmove(&ao_lisp_chunk[l+1],
		&ao_lisp_chunk[l],
		(end - (l+1)) * sizeof (struct ao_lisp_chunk));

	/* Add new entry */
	ao_lisp_chunk[l].old_offset = offset;
	ao_lisp_chunk[l].size = size;

	/* Increment the number of elements up to the size of the array */
	if (chunk_last < AO_LISP_NCHUNK)
		chunk_last++;

	/* Set the top address if the array is full */
	if (chunk_last == AO_LISP_NCHUNK)
		chunk_high = ao_lisp_chunk[AO_LISP_NCHUNK-1].old_offset +
			ao_lisp_chunk[AO_LISP_NCHUNK-1].size;
}

static void
reset_chunks(void)
{
	chunk_high = ao_lisp_top;
	chunk_last = 0;
	chunk_first = 0;
}

/*
 * Walk all referenced objects calling functions on each one
 */

static void
walk(int (*visit_addr)(const struct ao_lisp_type *type, void **addr),
     int (*visit_poly)(ao_poly *p, uint8_t do_note_cons))
{
	int i;

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
	[AO_LISP_STACK] = &ao_lisp_stack_type,
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

#if DBG_MEM_STATS
int ao_lisp_collects[2];
int ao_lisp_freed[2];
int ao_lisp_loops[2];
#endif

int ao_lisp_last_top;

int
ao_lisp_collect(uint8_t style)
{
	int	i;
	int	top;
#if DBG_MEM_STATS
	int	loops = 0;
#endif
#if DBG_MEM
	struct ao_lisp_record	*mark_record = NULL, *move_record = NULL;

	MDBG_MOVE("collect %d\n", ao_lisp_collects[style]);
#endif

	/* The first time through, we're doing a full collect */
	if (ao_lisp_last_top == 0)
		style = AO_LISP_COLLECT_FULL;

	/* Clear references to all caches */
	for (i = 0; i < (int) AO_LISP_CACHE; i++)
		*ao_lisp_cache[i] = NULL;
	if (style == AO_LISP_COLLECT_FULL) {
		chunk_low = top = 0;
	} else {
		chunk_low = top = ao_lisp_last_top;
	}
	for (;;) {
#if DBG_MEM_STATS
		loops++;
#endif
		MDBG_MOVE("move chunks from %d to %d\n", chunk_low, top);
		/* Find the sizes of the first chunk of objects to move */
		reset_chunks();
		walk(ao_lisp_mark_ref, ao_lisp_poly_mark_ref);
#if DBG_MEM

		ao_lisp_record_free(mark_record);
		mark_record = ao_lisp_record_save();
		if (mark_record && move_record)
			ao_lisp_record_compare("mark", move_record, mark_record);
#endif

		DUMP_BUSY();

		/* Find the first moving object */
		for (i = 0; i < chunk_last; i++) {
			uint16_t	size = ao_lisp_chunk[i].size;

#if DBG_MEM
			if (!size)
				ao_lisp_abort();
#endif

			if (ao_lisp_chunk[i].old_offset > top)
				break;

			MDBG_MOVE("chunk %d %d not moving\n",
				  ao_lisp_chunk[i].old_offset,
				  ao_lisp_chunk[i].size);
#if DBG_MEM
			if (ao_lisp_chunk[i].old_offset != top)
				ao_lisp_abort();
#endif
			top += size;
		}

		/*
		 * Limit amount of chunk array used in mapping moves
		 * to the active region
		 */
		chunk_first = i;
		chunk_low = ao_lisp_chunk[i].old_offset;

		/* Copy all of the objects */
		for (; i < chunk_last; i++) {
			uint16_t	size = ao_lisp_chunk[i].size;

#if DBG_MEM
			if (!size)
				ao_lisp_abort();
#endif

			MDBG_MOVE("chunk %d %d -> %d\n",
				  ao_lisp_chunk[i].old_offset,
				  size,
				  top);
			ao_lisp_chunk[i].new_offset = top;

			memmove(&ao_lisp_pool[top],
				&ao_lisp_pool[ao_lisp_chunk[i].old_offset],
				size);

			top += size;
		}

		if (chunk_first < chunk_last) {
			/* Relocate all references to the objects */
			walk(ao_lisp_move, ao_lisp_poly_move);

#if DBG_MEM
			ao_lisp_record_free(move_record);
			move_record = ao_lisp_record_save();
			if (mark_record && move_record)
				ao_lisp_record_compare("move", mark_record, move_record);
#endif
		}

		/* If we ran into the end of the heap, then
		 * there's no need to keep walking
		 */
		if (chunk_last != AO_LISP_NCHUNK)
			break;

		/* Next loop starts right above this loop */
		chunk_low = chunk_high;
	}

#if DBG_MEM_STATS
	/* Collect stats */
	++ao_lisp_collects[style];
	ao_lisp_freed[style] += ao_lisp_top - top;
	ao_lisp_loops[style] += loops;
#endif

	ao_lisp_top = top;
	if (style == AO_LISP_COLLECT_FULL)
		ao_lisp_last_top = top;

	MDBG_DO(memset(ao_lisp_chunk, '\0', sizeof (ao_lisp_chunk));
		walk(ao_lisp_mark_ref, ao_lisp_poly_mark_ref));

	return AO_LISP_POOL - ao_lisp_top;
}

/*
 * Mark interfaces for objects
 */

/*
 * Note a reference to memory and collect information about a few
 * object sizes at a time
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

/*
 * Mark an object and all that it refereces
 */
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

/*
 * Mark an object, unless it is a cons cell and
 * do_note_cons is set. In that case, just
 * set a bit in the cons note array; those
 * will be marked in a separate pass to avoid
 * deep recursion in the collector
 */
int
ao_lisp_poly_mark(ao_poly p, uint8_t do_note_cons)
{
	uint8_t type;
	void	*addr;

	type = ao_lisp_poly_base_type(p);

	if (type == AO_LISP_INT)
		return 1;

	addr = ao_lisp_ref(p);
	if (!AO_LISP_IS_POOL(addr))
		return 1;

	if (type == AO_LISP_CONS && do_note_cons) {
		note_cons(pool_offset(addr));
		return 1;
	} else {
		if (type == AO_LISP_OTHER)
			type = ao_lisp_other_type(addr);

		const struct ao_lisp_type *lisp_type = ao_lisp_types[type];
#if DBG_MEM
		if (!lisp_type)
			ao_lisp_abort();
#endif

		return ao_lisp_mark(lisp_type, addr);
	}
}

/*
 * Find the current location of an object
 * based on the original location. For unmoved
 * objects, this is simple. For moved objects,
 * go search for it
 */

static uint16_t
move_map(uint16_t offset)
{
	int		l;

	if (offset < chunk_low || chunk_high <= offset)
		return offset;

	l = find_chunk(offset);

#if DBG_MEM
	if (ao_lisp_chunk[l].old_offset != offset)
		ao_lisp_abort();
#endif
	return ao_lisp_chunk[l].new_offset;
}

int
ao_lisp_move_memory(const struct ao_lisp_type *type, void **ref)
{
	void		*addr = *ref;
	uint16_t	offset, orig_offset;

	if (!AO_LISP_IS_POOL(addr))
		return 1;

	(void) type;

	MDBG_MOVE("move memory %d\n", MDBG_OFFSET(addr));
	orig_offset = pool_offset(addr);
	offset = move_map(orig_offset);
	if (offset != orig_offset) {
		MDBG_MOVE("update ref %d %d -> %d\n",
			  AO_LISP_IS_POOL(ref) ? MDBG_OFFSET(ref) : -1,
			  orig_offset, offset);
		*ref = ao_lisp_pool + offset;
	}
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
	uint8_t		type;
	ao_poly		p = *ref;
	int		ret;
	void		*addr;
	uint16_t	offset, orig_offset;
	uint8_t		base_type;

	base_type = type = ao_lisp_poly_base_type(p);

	if (type == AO_LISP_INT)
		return 1;

	addr = ao_lisp_ref(p);
	if (!AO_LISP_IS_POOL(addr))
		return 1;

	orig_offset = pool_offset(addr);
	offset = move_map(orig_offset);

	if (type == AO_LISP_CONS && do_note_cons) {
		note_cons(orig_offset);
		ret = 1;
	} else {
		if (type == AO_LISP_OTHER)
			type = ao_lisp_other_type(ao_lisp_pool + offset);

		const struct ao_lisp_type *lisp_type = ao_lisp_types[type];
#if DBG_MEM
		if (!lisp_type)
			ao_lisp_abort();
#endif

		ret = ao_lisp_move(lisp_type, &addr);
	}

	/* Re-write the poly value */
	if (offset != orig_offset) {
		ao_poly np = ao_lisp_poly(ao_lisp_pool + offset, base_type);
		MDBG_MOVE("poly %d moved %d -> %d\n",
			  type, orig_offset, offset);
		*ref = np;
	}
	return ret;
}

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
	if (AO_LISP_POOL - ao_lisp_top < size &&
	    ao_lisp_collect(AO_LISP_COLLECT_INCREMENTAL) < size &&
	    ao_lisp_collect(AO_LISP_COLLECT_FULL) < size)
	{
		ao_lisp_error(AO_LISP_OOM, "out of memory");
		return NULL;
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

