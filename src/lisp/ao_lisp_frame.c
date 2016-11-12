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

#if 0
#define DBG(...)	printf(__VA_ARGS__)
#else
#define DBG(...)
#endif

static inline int
frame_num_size(int num)
{
	return sizeof (struct ao_lisp_frame) + num * sizeof (struct ao_lisp_val);
}

static int
frame_size(void *addr)
{
	struct ao_lisp_frame	*frame = addr;
	return frame_num_size(frame->num);
}

#define OFFSET(a)	((int) ((uint8_t *) (ao_lisp_ref(a)) - ao_lisp_const))

static void
frame_mark(void *addr)
{
	struct ao_lisp_frame	*frame = addr;
	int			f;

	for (;;) {
		DBG("frame mark %p\n", frame);
		if (!AO_LISP_IS_POOL(frame))
			break;
		for (f = 0; f < frame->num; f++) {
			struct ao_lisp_val	*v = &frame->vals[f];

			ao_lisp_poly_mark(v->val, 0);
			DBG ("\tframe mark atom %s %d val %d at %d\n",
			     ao_lisp_poly_atom(v->atom)->name,
			     OFFSET(v->atom), OFFSET(v->val), f);
		}
		frame = ao_lisp_poly_frame(frame->next);
		DBG("frame next %p\n", frame);
		if (!frame)
			break;
		if (ao_lisp_mark_memory(frame, frame_size(frame)))
			break;
	}
}

static void
frame_move(void *addr)
{
	struct ao_lisp_frame	*frame = addr;
	int			f;

	for (;;) {
		struct ao_lisp_frame	*next;
		int			ret;

		DBG("frame move %p\n", frame);
		if (!AO_LISP_IS_POOL(frame))
			break;
		for (f = 0; f < frame->num; f++) {
			struct ao_lisp_val	*v = &frame->vals[f];

			ao_lisp_poly_move(&v->atom, 0);
			DBG("moved atom %s\n", ao_lisp_poly_atom(v->atom)->name);
			ao_lisp_poly_move(&v->val, 0);
		}
		next = ao_lisp_poly_frame(frame->next);
		if (!next)
			break;
		ret = ao_lisp_move_memory((void **) &next, frame_size(next));
		if (next != ao_lisp_poly_frame(frame->next))
			frame->next = ao_lisp_frame_poly(next);
		if (ret)
			break;
		frame = next;
	}
}

const struct ao_lisp_type ao_lisp_frame_type = {
	.mark = frame_mark,
	.size = frame_size,
	.move = frame_move
};

void
ao_lisp_frame_print(ao_poly p)
{
	struct ao_lisp_frame	*frame = ao_lisp_poly_frame(p);
	int			f;

	printf ("{");
	if (frame) {
		for (f = 0; f < frame->num; f++) {
			if (f != 0)
				printf(", ");
			ao_lisp_poly_print(frame->vals[f].atom);
			printf(" = ");
			ao_lisp_poly_print(frame->vals[f].val);
		}
		if (frame->next)
			ao_lisp_poly_print(frame->next);
	}
	printf("}");
}

ao_poly *
ao_lisp_frame_ref(struct ao_lisp_frame *frame, ao_poly atom)
{
	int f;
	for (f = 0; f < frame->num; f++)
		if (frame->vals[f].atom == atom)
			return &frame->vals[f].val;
	return NULL;
}

int
ao_lisp_frame_set(struct ao_lisp_frame *frame, ao_poly atom, ao_poly val)
{
	while (frame) {
		if (!AO_LISP_IS_CONST(frame)) {
			ao_poly *ref = ao_lisp_frame_ref(frame, atom);
			if (ref) {
				*ref = val;
				return 1;
			}
		}
		frame = ao_lisp_poly_frame(frame->next);
	}
	return 0;
}

ao_poly
ao_lisp_frame_get(struct ao_lisp_frame *frame, ao_poly atom)
{
	while (frame) {
		ao_poly *ref = ao_lisp_frame_ref(frame, atom);
		if (ref)
			return *ref;
		frame = ao_lisp_poly_frame(frame->next);
	}
	return AO_LISP_NIL;
}

struct ao_lisp_frame *
ao_lisp_frame_new(int num)
{
	struct ao_lisp_frame *frame = ao_lisp_alloc(frame_num_size(num));

	if (!frame)
		return NULL;
	frame->type = AO_LISP_FRAME;
	frame->num = num;
	frame->next = AO_LISP_NIL;
	memset(frame->vals, '\0', num * sizeof (struct ao_lisp_val));
	return frame;
}

static struct ao_lisp_frame *
ao_lisp_frame_realloc(struct ao_lisp_frame **frame_ref, int new_num)
{
	struct ao_lisp_frame	*frame = *frame_ref;
	struct ao_lisp_frame	*new;
	int			copy;

	if (new_num == frame->num)
		return frame;
	new = ao_lisp_frame_new(new_num);
	if (!new)
		return NULL;
	/*
	 * Re-fetch the frame as it may have moved
	 * during the allocation
	 */
	frame = *frame_ref;
	copy = new_num;
	if (copy > frame->num)
		copy = frame->num;
	memcpy(new->vals, frame->vals, copy * sizeof (struct ao_lisp_val));
	new->next = frame->next;
	return new;
}

int
ao_lisp_frame_add(struct ao_lisp_frame **frame_ref, ao_poly atom, ao_poly val)
{
	struct ao_lisp_frame *frame = *frame_ref;
	ao_poly *ref = frame ? ao_lisp_frame_ref(frame, atom) : NULL;

	if (!ref) {
		int f;
		ao_lisp_root_poly_add(&atom);
		ao_lisp_root_poly_add(&val);
		if (frame) {
			f = frame->num;
			frame = ao_lisp_frame_realloc(frame_ref, f + 1);
		} else {
			f = 0;
			frame = ao_lisp_frame_new(1);
		}
		ao_lisp_root_clear(&atom);
		ao_lisp_root_clear(&val);
		if (!frame)
			return 0;
		*frame_ref = frame;
		DBG ("add atom %s %d, val %d at %d\n", ao_lisp_poly_atom(atom)->name, OFFSET(atom), OFFSET(val), f);
		frame->vals[f].atom = atom;
		ref = &frame->vals[f].val;
	}
	*ref = val;
	return 1;
}
