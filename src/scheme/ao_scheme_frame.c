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

static inline int
frame_vals_num_size(int num)
{
	return sizeof (struct ao_scheme_frame_vals) + num * sizeof (struct ao_scheme_val);
}

static int
frame_vals_size(void *addr)
{
	struct ao_scheme_frame_vals	*vals = addr;
	return frame_vals_num_size(vals->size);
}

static void
frame_vals_mark(void *addr)
{
	struct ao_scheme_frame_vals	*vals = addr;
	int				f;

	for (f = 0; f < vals->size; f++) {
		struct ao_scheme_val	*v = &vals->vals[f];

		ao_scheme_poly_mark(v->atom, 0);
		ao_scheme_poly_mark(v->val, 0);
		MDBG_MOVE("frame mark atom %s %d val %d at %d\n",
			  ao_scheme_poly_atom(v->atom)->name,
			  MDBG_OFFSET(ao_scheme_ref(v->atom)),
			  MDBG_OFFSET(ao_scheme_ref(v->val)), f);
	}
}

static void
frame_vals_move(void *addr)
{
	struct ao_scheme_frame_vals	*vals = addr;
	int				f;

	for (f = 0; f < vals->size; f++) {
		struct ao_scheme_val	*v = &vals->vals[f];

		ao_scheme_poly_move(&v->atom, 0);
		ao_scheme_poly_move(&v->val, 0);
		MDBG_MOVE("frame move atom %s %d val %d at %d\n",
			  ao_scheme_poly_atom(v->atom)->name,
			  MDBG_OFFSET(ao_scheme_ref(v->atom)),
			  MDBG_OFFSET(ao_scheme_ref(v->val)), f);
	}
}

const struct ao_scheme_type ao_scheme_frame_vals_type = {
	.mark = frame_vals_mark,
	.size = frame_vals_size,
	.move = frame_vals_move,
	.name = "frame_vals"
};

static int
frame_size(void *addr)
{
	(void) addr;
	return sizeof (struct ao_scheme_frame);
}

static void
frame_mark(void *addr)
{
	struct ao_scheme_frame	*frame = addr;

	for (;;) {
		struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);

		MDBG_MOVE("frame mark %d\n", MDBG_OFFSET(frame));
		if (!ao_scheme_mark_memory(&ao_scheme_frame_vals_type, vals))
			frame_vals_mark(vals);
		frame = ao_scheme_poly_frame(frame->prev);
		MDBG_MOVE("frame next %d\n", MDBG_OFFSET(frame));
		if (!frame)
			break;
		if (ao_scheme_mark_memory(&ao_scheme_frame_type, frame))
			break;
	}
}

static void
frame_move(void *addr)
{
	struct ao_scheme_frame	*frame = addr;

	for (;;) {
		struct ao_scheme_frame		*prev;
		struct ao_scheme_frame_vals	*vals;
		int				ret;

		MDBG_MOVE("frame move %d\n", MDBG_OFFSET(frame));
		vals = ao_scheme_poly_frame_vals(frame->vals);
		if (!ao_scheme_move_memory(&ao_scheme_frame_vals_type, (void **) &vals))
			frame_vals_move(vals);
		if (vals != ao_scheme_poly_frame_vals(frame->vals))
			frame->vals = ao_scheme_frame_vals_poly(vals);

		prev = ao_scheme_poly_frame(frame->prev);
		if (!prev)
			break;
		ret = ao_scheme_move_memory(&ao_scheme_frame_type, (void **) &prev);
		if (prev != ao_scheme_poly_frame(frame->prev)) {
			MDBG_MOVE("frame prev moved from %d to %d\n",
				  MDBG_OFFSET(ao_scheme_poly_frame(frame->prev)),
				  MDBG_OFFSET(prev));
			frame->prev = ao_scheme_frame_poly(prev);
		}
		if (ret)
			break;
		frame = prev;
	}
}

const struct ao_scheme_type ao_scheme_frame_type = {
	.mark = frame_mark,
	.size = frame_size,
	.move = frame_move,
	.name = "frame",
};

int ao_scheme_frame_print_indent;

static void
ao_scheme_frame_indent(FILE *out, int extra)
{
	int				i;
	putc('\n', out);
	for (i = 0; i < ao_scheme_frame_print_indent+extra; i++)
		putc('\t', out);
}

void
ao_scheme_frame_write(FILE *out, ao_poly p, bool write)
{
	struct ao_scheme_frame		*frame = ao_scheme_poly_frame(p);
	struct ao_scheme_frame		*clear = frame;
	int				f;
	int				written = 0;

	ao_scheme_print_start();
	while (frame) {
		struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);

		if (written != 0)
			fputs(", ", out);
		if (ao_scheme_print_mark_addr(frame)) {
			fputs("recurse...", out);
			break;
		}

		putc('{', out);
		written++;
		for (f = 0; f < frame->num; f++) {
			ao_scheme_frame_indent(out, 1);
			ao_scheme_poly_write(out, vals->vals[f].atom, write);
			fputs(" = ", out);
			ao_scheme_poly_write(out, vals->vals[f].val, write);
		}
		frame = ao_scheme_poly_frame(frame->prev);
		ao_scheme_frame_indent(out, 0);
		putc('}', out);
	}
	if (ao_scheme_print_stop()) {
		while (written--) {
			ao_scheme_print_clear_addr(clear);
			clear = ao_scheme_poly_frame(clear->prev);
		}
	}
}

static int
ao_scheme_frame_find(struct ao_scheme_frame *frame, int top, ao_poly atom)
{
	struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);
	int 				l = 0;
	int 				r = top - 1;

	while (l <= r) {
		int m = (l + r) >> 1;
		if (vals->vals[m].atom < atom)
			l = m + 1;
		else
			r = m - 1;
	}
	return l;
}

ao_poly *
ao_scheme_frame_ref(struct ao_scheme_frame *frame, ao_poly atom)
{
	struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);
	int				l = ao_scheme_frame_find(frame, frame->num, atom);

	if (l >= frame->num)
		return NULL;

	if (vals->vals[l].atom != atom)
		return NULL;
	return &vals->vals[l].val;
}

struct ao_scheme_frame	*ao_scheme_frame_free_list[AO_SCHEME_FRAME_FREE];

static struct ao_scheme_frame_vals *
ao_scheme_frame_vals_new(int num)
{
	struct ao_scheme_frame_vals	*vals;

	vals = ao_scheme_alloc(frame_vals_num_size(num));
	if (!vals)
		return NULL;
	vals->type = AO_SCHEME_FRAME_VALS;
	vals->size = num;
	memset(vals->vals, '\0', num * sizeof (struct ao_scheme_val));
	return vals;
}

struct ao_scheme_frame *
ao_scheme_frame_new(int num)
{
	struct ao_scheme_frame		*frame;
	struct ao_scheme_frame_vals	*vals;

	if (num < AO_SCHEME_FRAME_FREE && (frame = ao_scheme_frame_free_list[num])) {
		ao_scheme_frame_free_list[num] = ao_scheme_poly_frame(frame->prev);
		vals = ao_scheme_poly_frame_vals(frame->vals);
	} else {
		frame = ao_scheme_alloc(sizeof (struct ao_scheme_frame));
		if (!frame)
			return NULL;
		frame->type = AO_SCHEME_FRAME;
		frame->num = 0;
		frame->prev = AO_SCHEME_NIL;
		frame->vals = AO_SCHEME_NIL;
		ao_scheme_frame_stash(frame);
		vals = ao_scheme_frame_vals_new(num);
		frame = ao_scheme_frame_fetch();
		if (!vals)
			return NULL;
		frame->vals = ao_scheme_frame_vals_poly(vals);
		frame->num = num;
	}
	frame->prev = AO_SCHEME_NIL;
	return frame;
}

ao_poly
ao_scheme_frame_mark(struct ao_scheme_frame *frame)
{
	if (!frame)
		return AO_SCHEME_NIL;
	frame->type |= AO_SCHEME_FRAME_MARK;
	return ao_scheme_frame_poly(frame);
}

void
ao_scheme_frame_free(struct ao_scheme_frame *frame)
{
	if (frame && !ao_scheme_frame_marked(frame)) {
		int	num = frame->num;
		if (num < AO_SCHEME_FRAME_FREE) {
			struct ao_scheme_frame_vals	*vals;

			vals = ao_scheme_poly_frame_vals(frame->vals);
			memset(vals->vals, '\0', vals->size * sizeof (struct ao_scheme_val));
			frame->prev = ao_scheme_frame_poly(ao_scheme_frame_free_list[num]);
			ao_scheme_frame_free_list[num] = frame;
		}
	}
}

static struct ao_scheme_frame *
ao_scheme_frame_realloc(struct ao_scheme_frame *frame, int new_num)
{
	struct ao_scheme_frame_vals	*vals;
	struct ao_scheme_frame_vals	*new_vals;
	int				copy;

	if (new_num == frame->num)
		return frame;
	ao_scheme_frame_stash(frame);
	new_vals = ao_scheme_frame_vals_new(new_num);
	frame = ao_scheme_frame_fetch();
	if (!new_vals)
		return NULL;
	vals = ao_scheme_poly_frame_vals(frame->vals);
	copy = new_num;
	if (copy > frame->num)
		copy = frame->num;
	memcpy(new_vals->vals, vals->vals, copy * sizeof (struct ao_scheme_val));
	frame->vals = ao_scheme_frame_vals_poly(new_vals);
	frame->num = new_num;
	return frame;
}

void
ao_scheme_frame_bind(struct ao_scheme_frame *frame, int num, ao_poly atom, ao_poly val)
{
	struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);
	int 				l = ao_scheme_frame_find(frame, num, atom);

	memmove(&vals->vals[l+1],
		&vals->vals[l],
		(num - l) * sizeof (struct ao_scheme_val));
	vals->vals[l].atom = atom;
	vals->vals[l].val = val;
}

ao_poly
ao_scheme_frame_add(struct ao_scheme_frame *frame, ao_poly atom, ao_poly val)
{
	ao_poly *ref = frame ? ao_scheme_frame_ref(frame, atom) : NULL;

	if (!ref) {
		int f = frame->num;
		ao_scheme_poly_stash(atom);
		ao_scheme_poly_stash(val);
		frame = ao_scheme_frame_realloc(frame, f + 1);
		val = ao_scheme_poly_fetch();
		atom = ao_scheme_poly_fetch();
		if (!frame)
			return AO_SCHEME_NIL;
		ao_scheme_frame_bind(frame, frame->num - 1, atom, val);
	} else
		*ref = val;
	return val;
}

#ifdef AO_SCHEME_FEATURE_UNDEF
ao_poly
ao_scheme_frame_del(struct ao_scheme_frame *frame, ao_poly atom)
{
	struct ao_scheme_frame_vals	*vals = ao_scheme_poly_frame_vals(frame->vals);
	int				l = ao_scheme_frame_find(frame, frame->num, atom);
	int				f = frame->num;
	struct ao_scheme_frame		*moved_frame;

	if (l >= frame->num)
		return _ao_scheme_bool_false;

	if (vals->vals[l].atom != atom)
		return _ao_scheme_bool_false;

	/* squash the deleted entry */
	memmove(&vals->vals[l],
		&vals->vals[l+1],
		(f - l) * sizeof (struct ao_scheme_val));

	/* allocate a smaller vals array */
	ao_scheme_frame_stash(frame);
	moved_frame = ao_scheme_frame_realloc(frame, f - 1);
	frame = ao_scheme_frame_fetch();

	/*
	 * We couldn't allocate a smaller frame, so just
	 * ignore the last value in the array
	 */
	if (!moved_frame)
		frame->num = f - 1;
	return _ao_scheme_bool_true;
}
#endif

struct ao_scheme_frame	*ao_scheme_frame_global;
struct ao_scheme_frame	*ao_scheme_frame_current;

void
ao_scheme_frame_init(void)
{
	if (!ao_scheme_frame_global)
		ao_scheme_frame_global = ao_scheme_frame_new(0);
}
