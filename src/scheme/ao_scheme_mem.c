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

#define AO_SCHEME_CONST_BITS

#include "ao_scheme.h"
#include <stdio.h>
#include <assert.h>

#ifdef AO_SCHEME_MAKE_CONST

/*
 * When building the constant table, it is the
 * pool for allocations.
 */

#include <stdlib.h>
uint8_t ao_scheme_const[AO_SCHEME_POOL_CONST] __attribute__((aligned(4)));
#define ao_scheme_pool ao_scheme_const
#undef AO_SCHEME_POOL
#define AO_SCHEME_POOL AO_SCHEME_POOL_CONST

#else

uint8_t	ao_scheme_pool[AO_SCHEME_POOL + AO_SCHEME_POOL_EXTRA] __attribute__((aligned(4)));

#endif

#ifndef DBG_MEM_STATS
#define DBG_MEM_STATS	DBG_MEM
#endif

#define DBG_MEM_STACK	0
#if DBG_MEM_STACK
char	*mem_collect_stack;
int64_t	mem_collect_max_depth;

static void
ao_scheme_check_stack(void)
{
	char	x;
	int64_t	depth;

	depth = mem_collect_stack - &x;
	if (depth > mem_collect_max_depth)
		mem_collect_max_depth = depth;
}

static void
_ao_scheme_reset_stack(char *x)
{
	mem_collect_stack = x;
//	mem_collect_max_depth = 0;
}
#define ao_scheme_declare_stack	char x;
#define ao_scheme_reset_stack() _ao_scheme_reset_stack(&x)
#else
#define ao_scheme_check_stack()
#define ao_scheme_declare_stack
#define ao_scheme_reset_stack()
#endif

#if DBG_MEM
#define DBG_MEM_RECORD	1
#endif

#if DBG_MEM
int dbg_move_depth;
int dbg_mem = DBG_MEM_START;
int dbg_validate = 0;
#endif

#if DBG_MEM_RECORD
struct ao_scheme_record {
	struct ao_scheme_record		*next;
	const struct ao_scheme_type	*type;
	void				*addr;
	int				size;
};

static struct ao_scheme_record	*record_head, **record_tail;

static void
ao_scheme_record_free(struct ao_scheme_record *record)
{
	while (record) {
		struct ao_scheme_record *next = record->next;
		free(record);
		record = next;
	}
}

static void
ao_scheme_record_reset(void)
{
	ao_scheme_record_free(record_head);
	record_head = NULL;
	record_tail = &record_head;
}

static void
ao_scheme_record(const struct ao_scheme_type	*type,
	       void				*addr,
	       int				size)
{
	struct ao_scheme_record	*r = malloc(sizeof (struct ao_scheme_record));

	r->next = NULL;
	r->type = type;
	r->addr = addr;
	r->size = size;
	*record_tail = r;
	record_tail = &r->next;
}

static struct ao_scheme_record *
ao_scheme_record_save(void)
{
	struct ao_scheme_record *r = record_head;

	record_head = NULL;
	record_tail = &record_head;
	return r;
}

static void
ao_scheme_record_compare(const char *where,
			 struct ao_scheme_record *a,
			 struct ao_scheme_record *b)
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
			ao_scheme_abort();
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
		ao_scheme_abort();
	}
	if (b) {
		printf("%s record differs NULL -> %d %s %d\n",
		       where,
		       MDBG_OFFSET(b->addr),
		       b->type->name,
		       b->size);
		ao_scheme_abort();
	}
}

#else
#define ao_scheme_record_reset()
#define ao_scheme_record(t,a,s)
#endif

uint8_t	ao_scheme_exception;

struct ao_scheme_root {
	const struct ao_scheme_type	*type;
	void				**addr;
};

#define AO_SCHEME_NUM_STASH	6
static ao_poly			stash_poly[AO_SCHEME_NUM_STASH];
static int			stash_poly_ptr;

static const struct ao_scheme_root	ao_scheme_root[] = {
	{
		.type = NULL,
		.addr = (void **) (void *) &stash_poly[0]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &stash_poly[1]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &stash_poly[2]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &stash_poly[3]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &stash_poly[4]
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &stash_poly[5]
	},
	{
		.type = &ao_scheme_frame_type,
		.addr = (void **) &ao_scheme_frame_global,
	},
	{
		.type = &ao_scheme_frame_type,
		.addr = (void **) &ao_scheme_frame_current,
	},
	{
		.type = &ao_scheme_stack_type,
		.addr = (void **) &ao_scheme_stack,
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &ao_scheme_v,
	},
	{
		.type = &ao_scheme_cons_type,
		.addr = (void **) &ao_scheme_read_cons,
	},
	{
		.type = &ao_scheme_cons_type,
		.addr = (void **) &ao_scheme_read_cons_tail,
	},
	{
		.type = &ao_scheme_cons_type,
		.addr = (void **) &ao_scheme_read_stack,
	},
#ifdef AO_SCHEME_FEATURE_PORT
	{
		.type = NULL,
		.addr = (void **) (void *) &ao_scheme_stdin,
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &ao_scheme_stdout,
	},
	{
		.type = NULL,
		.addr = (void **) (void *) &ao_scheme_stderr,
	},
#endif
#ifdef AO_SCHEME_MAKE_CONST
	{
		.type = &ao_scheme_bool_type,
		.addr = (void **) &ao_scheme_false,
	},
	{
		.type = &ao_scheme_bool_type,
		.addr = (void **) &ao_scheme_true,
	},
#endif
};

#define AO_SCHEME_ROOT	(sizeof (ao_scheme_root) / sizeof (ao_scheme_root[0]))

static const void ** const ao_scheme_cache[] = {
	(const void **) &ao_scheme_cons_free_list,
	(const void **) &ao_scheme_stack_free_list,
	(const void **) &ao_scheme_frame_free_list[0],
	(const void **) &ao_scheme_frame_free_list[1],
	(const void **) &ao_scheme_frame_free_list[2],
	(const void **) &ao_scheme_frame_free_list[3],
	(const void **) &ao_scheme_frame_free_list[4],
	(const void **) &ao_scheme_frame_free_list[5],
};

#if AO_SCHEME_FRAME_FREE != 6
#error Unexpected AO_SCHEME_FRAME_FREE value
#endif

#define AO_SCHEME_CACHE	(sizeof (ao_scheme_cache) / sizeof (ao_scheme_cache[0]))

#define AO_SCHEME_BUSY_SIZE	((AO_SCHEME_POOL + 31) / 32)

static int	ao_scheme_printing, ao_scheme_print_cleared;
#if DBG_MEM
static int	ao_scheme_collecting;
#endif
static uint8_t	ao_scheme_busy[AO_SCHEME_BUSY_SIZE];
static uint8_t	ao_scheme_cons_note[AO_SCHEME_BUSY_SIZE];
static uint8_t	ao_scheme_cons_last[AO_SCHEME_BUSY_SIZE];
static uint8_t	ao_scheme_cons_noted;

uint16_t	ao_scheme_top;

struct ao_scheme_chunk {
	uint16_t		old_offset;
	union {
		uint16_t	size;
		uint16_t	new_offset;
	};
};

#define AO_SCHEME_NCHUNK	(AO_SCHEME_POOL / 64)

static struct ao_scheme_chunk ao_scheme_chunk[AO_SCHEME_NCHUNK];

/* Offset of an address within the pool. */
static inline uint16_t pool_offset(void *addr) {
#if DBG_MEM
	if (!ao_scheme_is_pool_addr(addr))
		ao_scheme_abort();
#endif
	return ((uint8_t *) addr) - ao_scheme_pool;
}

static inline void mark(uint8_t *tag, int offset) {
	int	byte = offset >> 5;
	int	bit = (offset >> 2) & 7;
	ao_scheme_check_stack();
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
	return min(AO_SCHEME_POOL, max(offset, 0));
}

static inline void
note_cons(uint16_t offset)
{
	MDBG_MOVE("note cons %d\n", offset);
	ao_scheme_cons_noted = 1;
	mark(ao_scheme_cons_note, offset);
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
		if (ao_scheme_chunk[m].old_offset < offset)
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
	int end;

	if (offset < chunk_low || chunk_high <= offset)
		return;

	l = find_chunk(offset);

	/*
	 * The correct location is always in 'l', with r = l-1 being
	 * the entry before the right one
	 */

#if DBG_MEM
	/* Off the right side */
	if (l >= AO_SCHEME_NCHUNK)
		ao_scheme_abort();

	/* Off the left side */
	if (l == 0 && chunk_last && offset > ao_scheme_chunk[0].old_offset)
		ao_scheme_abort();

	if (l < chunk_last && ao_scheme_chunk[l].old_offset == offset)
		ao_scheme_abort();
#endif

	/* Shuffle existing entries right */
	end = min(AO_SCHEME_NCHUNK, chunk_last + 1);

	memmove(&ao_scheme_chunk[l+1],
		&ao_scheme_chunk[l],
		(end - (l+1)) * sizeof (struct ao_scheme_chunk));

	/* Add new entry */
	ao_scheme_chunk[l].old_offset = offset;
	ao_scheme_chunk[l].size = size;

	/* Increment the number of elements up to the size of the array */
	if (chunk_last < AO_SCHEME_NCHUNK)
		chunk_last++;

	/* Set the top address if the array is full */
	if (chunk_last == AO_SCHEME_NCHUNK)
		chunk_high = ao_scheme_chunk[AO_SCHEME_NCHUNK-1].old_offset +
			ao_scheme_chunk[AO_SCHEME_NCHUNK-1].size;
}

static void
reset_chunks(void)
{
	chunk_high = ao_scheme_top;
	chunk_last = 0;
	chunk_first = 0;
}

/*
 * Walk all referenced objects calling functions on each one
 */

static void
walk(int (*visit_addr)(const struct ao_scheme_type *type, void **addr),
     int (*visit_poly)(ao_poly *p, uint8_t do_note_cons))
{
	int i;

	ao_scheme_record_reset();
	memset(ao_scheme_busy, '\0', sizeof (ao_scheme_busy));
	memset(ao_scheme_cons_note, '\0', sizeof (ao_scheme_cons_note));
	ao_scheme_cons_noted = 0;
	for (i = 0; i < (int) AO_SCHEME_ROOT; i++) {
		if (ao_scheme_root[i].type) {
			void **a = ao_scheme_root[i].addr, *v;
			if (a && (v = *a)) {
				MDBG_MOVE("root ptr %d\n", MDBG_OFFSET(v));
				visit_addr(ao_scheme_root[i].type, a);
			}
		} else {
			ao_poly *a = (ao_poly *) ao_scheme_root[i].addr, p;
			if (a && (p = *a)) {
				MDBG_MOVE("root poly %d\n", MDBG_OFFSET(ao_scheme_ref(p)));
				visit_poly(a, 0);
			}
		}
	}
	while (ao_scheme_cons_noted) {
		memcpy(ao_scheme_cons_last, ao_scheme_cons_note, sizeof (ao_scheme_cons_note));
		memset(ao_scheme_cons_note, '\0', sizeof (ao_scheme_cons_note));
		ao_scheme_cons_noted = 0;
		for (i = 0; i < AO_SCHEME_POOL; i += 4) {
			if (busy(ao_scheme_cons_last, i)) {
				void *v = ao_scheme_pool + i;
				MDBG_MOVE("root cons %d\n", MDBG_OFFSET(v));
				visit_addr(&ao_scheme_cons_type, &v);
			}
		}
	}
}

#if MDBG_DUMP
static void
dump_busy(void)
{
	int	i;
	printf("busy:");
	for (i = 0; i < ao_scheme_top; i += 4) {
		if ((i & 0xff) == 0) {
			printf("\n\t");
		}
		else if ((i & 0x1f) == 0)
			printf(" ");
		if (busy(ao_scheme_busy, i))
			printf("*");
		else
			printf("-");
	}
	printf ("\n");
}
#define DUMP_BUSY()	dump_busy()
#else
#define DUMP_BUSY()
#endif

#if MDBG_DUMP
static void
dump_atoms(int show_marked)
{
	struct ao_scheme_atom	*atom;

	printf("atoms {\n");
	for (atom = ao_scheme_atoms; atom; atom = ao_scheme_poly_atom(atom->next)) {
		printf("\t%d: %s", MDBG_OFFSET(atom), atom->name);
		if (show_marked)
			printf(" %s", ao_scheme_marked(atom) ? "referenced" : "unreferenced");
		printf("\n");
	}
	printf("}\n");

}
#define DUMP_ATOMS(a)	dump_atoms(a)
#else
#define DUMP_ATOMS(a)
#endif

static const struct ao_scheme_type * const ao_scheme_types[AO_SCHEME_NUM_TYPE] = {
	[AO_SCHEME_CONS] = &ao_scheme_cons_type,
	[AO_SCHEME_INT] = NULL,
#ifdef AO_SCHEME_FEATURE_BIGINT
	[AO_SCHEME_BIGINT] = &ao_scheme_bigint_type,
#endif
	[AO_SCHEME_OTHER] = (void *) 0x1,
	[AO_SCHEME_ATOM] = &ao_scheme_atom_type,
	[AO_SCHEME_BUILTIN] = &ao_scheme_builtin_type,
	[AO_SCHEME_FRAME] = &ao_scheme_frame_type,
	[AO_SCHEME_FRAME_VALS] = &ao_scheme_frame_vals_type,
	[AO_SCHEME_LAMBDA] = &ao_scheme_lambda_type,
	[AO_SCHEME_STACK] = &ao_scheme_stack_type,
	[AO_SCHEME_BOOL] = &ao_scheme_bool_type,
	[AO_SCHEME_STRING] = &ao_scheme_string_type,
#ifdef AO_SCHEME_FEATURE_FLOAT
	[AO_SCHEME_FLOAT] = &ao_scheme_float_type,
#endif
#ifdef AO_SCHEME_FEATURE_VECTOR
	[AO_SCHEME_VECTOR] = &ao_scheme_vector_type,
#endif
#ifdef AO_SCHEME_FEATURE_PORT
	[AO_SCHEME_PORT] = &ao_scheme_port_type,
#endif
};

static int
ao_scheme_mark(const struct ao_scheme_type *type, void *addr);

static int
ao_scheme_move(const struct ao_scheme_type *type, void **ref);

static int
ao_scheme_mark_ref(const struct ao_scheme_type *type, void **ref)
{
	return ao_scheme_mark(type, *ref);
}

static int
ao_scheme_poly_mark_ref(ao_poly *p, uint8_t do_note_cons)
{
	return ao_scheme_poly_mark(*p, do_note_cons);
}

#if DBG_MEM_STATS
uint64_t ao_scheme_collects[2];
uint64_t ao_scheme_freed[2];
uint64_t ao_scheme_loops[2];
#endif

int ao_scheme_last_top;
int ao_scheme_collect_counts;

int
ao_scheme_collect(uint8_t style)
{
	ao_scheme_declare_stack
	int	i;
	int	top;
#if DBG_MEM_STATS
	int	loops = 0;
#endif
#if DBG_MEM_RECORD
	struct ao_scheme_record	*mark_record = NULL, *move_record = NULL;
#endif
	MDBG_MOVE("collect %lu\n", ao_scheme_collects[style]);

	MDBG_DO(ao_scheme_frame_write(stdout, ao_scheme_frame_poly(ao_scheme_frame_global), true));
	MDBG_DO(++ao_scheme_collecting);

	ao_scheme_reset_stack();

	/* The first time through, we're doing a full collect */
	if (ao_scheme_last_top == 0)
		style = AO_SCHEME_COLLECT_FULL;

	/* One in a while, just do a full collect */

	if (ao_scheme_collect_counts >= 128)
		style = AO_SCHEME_COLLECT_FULL;

	if (style == AO_SCHEME_COLLECT_FULL)
		ao_scheme_collect_counts = 0;

	/* Clear references to all caches */
	for (i = 0; i < (int) AO_SCHEME_CACHE; i++)
		*ao_scheme_cache[i] = NULL;
	if (style == AO_SCHEME_COLLECT_FULL) {
		chunk_low = top = 0;
	} else {
		chunk_low = top = ao_scheme_last_top;
	}
	for (;;) {
		MDBG_MOVE("move chunks from %d to %d\n", chunk_low, top);
		/* Find the sizes of the first chunk of objects to move */
		reset_chunks();
		walk(ao_scheme_mark_ref, ao_scheme_poly_mark_ref);

#ifdef AO_SCHEME_FEATURE_PORT
		ao_scheme_port_check_references();
#endif
		ao_scheme_atom_check_references();

#if DBG_MEM_RECORD
		ao_scheme_record_free(mark_record);
		mark_record = ao_scheme_record_save();
		if (mark_record && move_record)
			ao_scheme_record_compare("mark", move_record, mark_record);
#endif

		DUMP_ATOMS(1);
		DUMP_BUSY();

		/* Find the first moving object */
		for (i = 0; i < chunk_last; i++) {
			uint16_t	size = ao_scheme_chunk[i].size;
#if DBG_MEM
			if (!size)
				ao_scheme_abort();
#endif

			if (ao_scheme_chunk[i].old_offset > top)
				break;

			MDBG_MOVE("chunk %d %d not moving\n",
				  ao_scheme_chunk[i].old_offset,
				  ao_scheme_chunk[i].size);
#if DBG_MEM
			if (ao_scheme_chunk[i].old_offset != top)
				ao_scheme_abort();
#endif
			top += size;
		}

		/* Short-circuit the rest of the loop when all of the
		 * found objects aren't moving. This isn't strictly
		 * necessary as the rest of the loop is structured to
		 * work in this case, but GCC 7.2.0 with optimization
		 * greater than 2 generates incorrect code for this...
		 */
		if (i == AO_SCHEME_NCHUNK) {
			chunk_low = chunk_high;
#if DBG_MEM_STATS
			loops++;
#endif
			continue;
		}

		/*
		 * Limit amount of chunk array used in mapping moves
		 * to the active region
		 */
		chunk_first = i;
		chunk_low = ao_scheme_chunk[i].old_offset;

		/* Copy all of the objects */
		for (; i < chunk_last; i++) {
			uint16_t	size = ao_scheme_chunk[i].size;

#if DBG_MEM
			if (!size)
				ao_scheme_abort();
#endif

			MDBG_MOVE("chunk %d %d -> %d\n",
				  ao_scheme_chunk[i].old_offset,
				  size,
				  top);
			ao_scheme_chunk[i].new_offset = top;

			memmove(&ao_scheme_pool[top],
				&ao_scheme_pool[ao_scheme_chunk[i].old_offset],
				size);

			top += size;
		}

		if (chunk_first < chunk_last) {
			/* Relocate all references to the objects */
			walk(ao_scheme_move, ao_scheme_poly_move);
			ao_scheme_atom_move();
#ifdef AO_SCHEME_FEATURE_PORT
			/* the set of open ports gets relocated but not marked, so
			 * just deal with it separately
			 */
			ao_scheme_poly_move(&ao_scheme_open_ports, 0);
#endif

#if DBG_MEM_RECORD
			ao_scheme_record_free(move_record);
			move_record = ao_scheme_record_save();
			if (mark_record && move_record)
				ao_scheme_record_compare("move", mark_record, move_record);
#endif
			DUMP_ATOMS(0);
		}

#if DBG_MEM_STATS
		loops++;
#endif
		/* If we ran into the end of the heap, then
		 * there's no need to keep walking
		 */
		if (chunk_last != AO_SCHEME_NCHUNK)
			break;

		/* Next loop starts right above this loop */
		chunk_low = chunk_high;
	}

#if DBG_MEM_STATS
	/* Collect stats */
	++ao_scheme_collects[style];
	ao_scheme_freed[style] += ao_scheme_top - top;
	ao_scheme_loops[style] += loops;
#endif

	ao_scheme_top = top;
	if (style == AO_SCHEME_COLLECT_FULL)
		ao_scheme_last_top = top;

	MDBG_DO(memset(ao_scheme_chunk, '\0', sizeof (ao_scheme_chunk));
		walk(ao_scheme_mark_ref, ao_scheme_poly_mark_ref));

#if DBG_MEM_STACK
	fprintf(stderr, "max collect stack depth %lu\n", mem_collect_max_depth);
#endif
	MDBG_DO(--ao_scheme_collecting);
	return AO_SCHEME_POOL - ao_scheme_top;
}

#if DBG_FREE_CONS
void
ao_scheme_cons_check(struct ao_scheme_cons *cons)
{
	ao_poly	cdr;
	int offset;

	chunk_low = 0;
	reset_chunks();
	walk(ao_scheme_mark_ref, ao_scheme_poly_mark_ref);
	while (cons) {
		if (!ao_scheme_is_pool_addr(cons))
			break;
		offset = pool_offset(cons);
		if (busy(ao_scheme_busy, offset)) {
			ao_scheme_printf("cons at %p offset %d poly %d is busy\n\t%v\n", cons, offset, ao_scheme_cons_poly(cons), ao_scheme_cons_poly(cons));
			abort();
		}
		cdr = cons->cdr;
		if (!ao_scheme_is_pair(cdr))
			break;
		cons = ao_scheme_poly_cons(cdr);
	}
}
#endif

/*
 * Mark interfaces for objects
 */


/*
 * Note a reference to memory and collect information about a few
 * object sizes at a time
 */

int
ao_scheme_mark_memory(const struct ao_scheme_type *type, void *addr)
{
	int offset;
	if (!ao_scheme_is_pool_addr(addr))
		return 1;

	offset = pool_offset(addr);
	MDBG_MOVE("mark memory %d\n", MDBG_OFFSET(addr));
	if (busy(ao_scheme_busy, offset)) {
		MDBG_MOVE("already marked\n");
		return 1;
	}
	mark(ao_scheme_busy, offset);
	note_chunk(offset, ao_scheme_size(type, addr));
	return 0;
}

/*
 * Mark an object and all that it refereces
 */
static int
ao_scheme_mark(const struct ao_scheme_type *type, void *addr)
{
	int ret;
	MDBG_MOVE("mark offset %d\n", MDBG_OFFSET(addr));
	MDBG_MOVE_IN();
	ret = ao_scheme_mark_memory(type, addr);
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
ao_scheme_poly_mark(ao_poly p, uint8_t do_note_cons)
{
	uint8_t type;
	void	*addr;
	int	ret;

	type = ao_scheme_poly_base_type(p);

	if (type == AO_SCHEME_INT)
		return 1;

	addr = ao_scheme_ref(p);
	if (!ao_scheme_is_pool_addr(addr))
		return 1;

	if (type == AO_SCHEME_CONS && do_note_cons) {
		note_cons(pool_offset(addr));
		return 1;
	} else {
		const struct ao_scheme_type *lisp_type;

		if (type == AO_SCHEME_OTHER)
			type = ao_scheme_other_type(addr);

		lisp_type = ao_scheme_types[type];
#if DBG_MEM
		if (!lisp_type)
			ao_scheme_abort();
#endif

		MDBG_MOVE("poly_mark offset %d\n", MDBG_OFFSET(addr));
		MDBG_MOVE_IN();
		ret = ao_scheme_mark_memory(lisp_type, addr);
		if (!ret) {
			MDBG_MOVE("mark recurse\n");
			lisp_type->mark(addr);
		}
		MDBG_MOVE_OUT();
		return ret;
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
	if (ao_scheme_chunk[l].old_offset != offset)
		ao_scheme_abort();
#endif
	return ao_scheme_chunk[l].new_offset;
}

int
ao_scheme_move_memory(const struct ao_scheme_type *type, void **ref)
{
	void		*addr = *ref;
	uint16_t	offset, orig_offset;

	if (!ao_scheme_is_pool_addr(addr))
		return 1;

	(void) type;

	MDBG_MOVE("move memory %d\n", MDBG_OFFSET(addr));
	orig_offset = pool_offset(addr);
	offset = move_map(orig_offset);
	if (offset != orig_offset) {
		MDBG_MOVE("update ref %d %d -> %d\n",
			  ao_scheme_is_pool_addr(ref) ? MDBG_OFFSET(ref) : -1,
			  orig_offset, offset);
		*ref = ao_scheme_pool + offset;
	}
	if (busy(ao_scheme_busy, offset)) {
		MDBG_MOVE("already moved\n");
		return 1;
	}
	mark(ao_scheme_busy, offset);
	ao_scheme_record(type, addr, ao_scheme_size(type, addr));
	return 0;
}

static int
ao_scheme_move(const struct ao_scheme_type *type, void **ref)
{
	int ret;
	MDBG_MOVE("move object %d\n", MDBG_OFFSET(*ref));
	MDBG_MOVE_IN();
	ret = ao_scheme_move_memory(type, ref);
	if (!ret) {
		MDBG_MOVE("move recurse\n");
		type->move(*ref);
	}
	MDBG_MOVE_OUT();
	return ret;
}

int
ao_scheme_poly_move(ao_poly *ref, uint8_t do_note_cons)
{
	ao_poly		p = *ref;
	int		ret;
	void		*addr;
	uint16_t	offset, orig_offset;

	if (ao_scheme_poly_base_type(p) == AO_SCHEME_INT)
		return 1;

	addr = ao_scheme_ref(p);
	if (!ao_scheme_is_pool_addr(addr))
		return 1;

	orig_offset = pool_offset(addr);
	offset = move_map(orig_offset);

	if (ao_scheme_poly_base_type(p) == AO_SCHEME_CONS && do_note_cons) {
		note_cons(orig_offset);
		ret = 1;
	} else {
		uint8_t type = ao_scheme_poly_base_type(p);
		const struct ao_scheme_type *lisp_type;

		if (type == AO_SCHEME_OTHER)
			type = ao_scheme_other_type(ao_scheme_pool + offset);

		lisp_type = ao_scheme_types[type];
#if DBG_MEM
		if (!lisp_type)
			ao_scheme_abort();
#endif
		/* inline ao_scheme_move to save stack space */
		MDBG_MOVE("move object %d\n", MDBG_OFFSET(addr));
		MDBG_MOVE_IN();
		ret = ao_scheme_move_memory(lisp_type, &addr);
		if (!ret) {
			MDBG_MOVE("move recurse\n");
			lisp_type->move(addr);
		}
		MDBG_MOVE_OUT();
	}

	/* Re-write the poly value */
	if (offset != orig_offset) {
		ao_poly np = ao_scheme_poly(ao_scheme_pool + offset, ao_scheme_poly_base_type(p));
		MDBG_MOVE("poly %d moved %d -> %d\n",
			  ao_scheme_poly_type(np), orig_offset, offset);
		*ref = np;
	}
	return ret;
}

int
ao_scheme_marked(void *addr)
{
	if (!ao_scheme_is_pool_addr(addr))
		return 1;
	return busy(ao_scheme_busy, pool_offset(addr));
}

#if DBG_MEM
static void
ao_scheme_validate(void)
{
	chunk_low = 0;
	memset(ao_scheme_chunk, '\0', sizeof (ao_scheme_chunk));
	walk(ao_scheme_mark_ref, ao_scheme_poly_mark_ref);
}

int dbg_allocs;

#endif

void *
ao_scheme_alloc(int size)
{
	void	*addr;

	MDBG_DO(++dbg_allocs);
	MDBG_DO(if (dbg_validate) ao_scheme_validate());
	size = ao_scheme_size_round(size);
	if (AO_SCHEME_POOL - ao_scheme_top < size &&
	    ao_scheme_collect(AO_SCHEME_COLLECT_INCREMENTAL) < size &&
	    ao_scheme_collect(AO_SCHEME_COLLECT_FULL) < size)
	{
		ao_scheme_error(AO_SCHEME_OOM, "out of memory");
		return NULL;
	}
	addr = ao_scheme_pool + ao_scheme_top;
	ao_scheme_top += size;
	MDBG_MOVE("alloc %d size %d\n", MDBG_OFFSET(addr), size);
	return addr;
}

void
ao_scheme_poly_stash(ao_poly p)
{
	assert(stash_poly_ptr < AO_SCHEME_NUM_STASH);
	stash_poly[stash_poly_ptr++] = p;
}

ao_poly
ao_scheme_poly_fetch(void)
{
	ao_poly	p;

	assert (stash_poly_ptr > 0);
	p = stash_poly[--stash_poly_ptr];
	stash_poly[stash_poly_ptr] = AO_SCHEME_NIL;
	return p;
}

int
ao_scheme_print_mark_addr(void *addr)
{
	int	offset;

#if DBG_MEM
	if (ao_scheme_collecting)
		ao_scheme_abort();
#endif

	if (!ao_scheme_is_pool_addr(addr))
		return 0;

	if (!ao_scheme_print_cleared) {
		ao_scheme_print_cleared = 1;
		memset(ao_scheme_busy, '\0', sizeof (ao_scheme_busy));
	}
	offset = pool_offset(addr);
	if (busy(ao_scheme_busy, offset))
		return 1;
	mark(ao_scheme_busy, offset);
	return 0;
}

void
ao_scheme_print_clear_addr(void *addr)
{
	int	offset;

#if DBG_MEM
	if (ao_scheme_collecting)
		ao_scheme_abort();
#endif

	if (!ao_scheme_is_pool_addr(addr))
		return;

	if (!ao_scheme_print_cleared)
		return;
	offset = pool_offset(addr);
	clear(ao_scheme_busy, offset);
}

/* Notes that printing has started */
void
ao_scheme_print_start(void)
{
	ao_scheme_printing++;
}

/* Notes that printing has ended. Returns 1 if printing is still going on */
int
ao_scheme_print_stop(void)
{
	ao_scheme_printing--;
	if (ao_scheme_printing != 0)
		return 1;
	ao_scheme_print_cleared = 0;
	return 0;
}
