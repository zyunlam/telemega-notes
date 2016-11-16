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
//#include <stdio.h>
#include <ao_lisp_os.h>

typedef uint16_t	ao_poly;
typedef int16_t		ao_signed_poly;

#ifdef AO_LISP_SAVE

struct ao_lisp_os_save {
	ao_poly		atoms;
	ao_poly		globals;
	uint16_t	const_checksum;
	uint16_t	const_checksum_inv;
};

#define AO_LISP_POOL_EXTRA	(sizeof(struct ao_lisp_os_save))
#define AO_LISP_POOL	((int) (AO_LISP_POOL_TOTAL - AO_LISP_POOL_EXTRA))

int
ao_lisp_os_save(void);

int
ao_lisp_os_restore_save(struct ao_lisp_os_save *save, int offset);

int
ao_lisp_os_restore(void);

#endif

#ifdef AO_LISP_MAKE_CONST
#define AO_LISP_POOL_CONST	16384
extern uint8_t ao_lisp_const[AO_LISP_POOL_CONST];
#define ao_lisp_pool ao_lisp_const
#define AO_LISP_POOL AO_LISP_POOL_CONST

#define _atom(n) ao_lisp_atom_poly(ao_lisp_atom_intern(n))

#define _ao_lisp_atom_quote	_atom("quote")
#define _ao_lisp_atom_set 	_atom("set")
#define _ao_lisp_atom_setq 	_atom("setq")
#define _ao_lisp_atom_t 	_atom("t")
#define _ao_lisp_atom_car 	_atom("car")
#define _ao_lisp_atom_cdr	_atom("cdr")
#define _ao_lisp_atom_cons	_atom("cons")
#define _ao_lisp_atom_last	_atom("last")
#define _ao_lisp_atom_length	_atom("length")
#define _ao_lisp_atom_cond	_atom("cond")
#define _ao_lisp_atom_lambda	_atom("lambda")
#define _ao_lisp_atom_led	_atom("led")
#define _ao_lisp_atom_delay	_atom("delay")
#define _ao_lisp_atom_pack	_atom("pack")
#define _ao_lisp_atom_unpack	_atom("unpack")
#define _ao_lisp_atom_flush	_atom("flush")
#define _ao_lisp_atom_eval	_atom("eval")
#define _ao_lisp_atom_read	_atom("read")
#define _ao_lisp_atom_eof	_atom("eof")
#define _ao_lisp_atom_save	_atom("save")
#define _ao_lisp_atom_restore	_atom("restore")
#else
#include "ao_lisp_const.h"
#ifndef AO_LISP_POOL
#define AO_LISP_POOL	3072
#endif
extern uint8_t		ao_lisp_pool[AO_LISP_POOL + AO_LISP_POOL_EXTRA];
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
#define AO_LISP_FRAME		6
#define AO_LISP_LAMBDA		7
#define AO_LISP_NUM_TYPE	8

#define AO_LISP_NIL	0

extern uint16_t		ao_lisp_top;

#define AO_LISP_OOM		0x01
#define AO_LISP_DIVIDE_BY_ZERO	0x02
#define AO_LISP_INVALID		0x04
#define AO_LISP_UNDEFINED	0x08
#define AO_LISP_EOF		0x10

extern uint8_t		ao_lisp_exception;

static inline int
ao_lisp_is_const(ao_poly poly) {
	return poly & AO_LISP_CONST;
}

#define AO_LISP_IS_CONST(a)	(ao_lisp_const <= ((uint8_t *) (a)) && ((uint8_t *) (a)) < ao_lisp_const + AO_LISP_POOL_CONST)
#define AO_LISP_IS_POOL(a)	(ao_lisp_pool <= ((uint8_t *) (a)) && ((uint8_t *) (a)) < ao_lisp_pool + AO_LISP_POOL)

void *
ao_lisp_ref(ao_poly poly);

ao_poly
ao_lisp_poly(const void *addr, ao_poly type);

struct ao_lisp_type {
	int	(*size)(void *addr);
	void	(*mark)(void *addr);
	void	(*move)(void *addr);
	char	name[];
};

struct ao_lisp_cons {
	ao_poly		car;
	ao_poly		cdr;
};

struct ao_lisp_atom {
	uint8_t		type;
	uint8_t		pad[1];
	ao_poly		next;
	char		name[];
};

struct ao_lisp_val {
	ao_poly		atom;
	ao_poly		val;
};

struct ao_lisp_frame {
	uint8_t			type;
	uint8_t			_num;
	ao_poly			prev;
	struct ao_lisp_val	vals[];
};

#define AO_LISP_FRAME_NUM_MASK	0x7f

/* Set when the frame escapes the lambda */
#define AO_LISP_FRAME_MARK	0x80

static inline int ao_lisp_frame_num(struct ao_lisp_frame *f) {
	if (f->_num == 0xff)
		ao_lisp_abort();
	return f->_num & AO_LISP_FRAME_NUM_MASK;
}

static inline int ao_lisp_frame_marked(struct ao_lisp_frame *f) {
	if (f->_num == 0xff)
		ao_lisp_abort();
	return f->_num & AO_LISP_FRAME_MARK;
}

static inline struct ao_lisp_frame *
ao_lisp_poly_frame(ao_poly poly) {
	struct ao_lisp_frame *frame = ao_lisp_ref(poly);
	if (frame && frame->_num == 0xff)
		ao_lisp_abort();
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_frame_poly(struct ao_lisp_frame *frame) {
	return ao_lisp_poly(frame, AO_LISP_OTHER);
}

enum eval_state {
	eval_sexpr,		/* Evaluate an sexpr */
	eval_val,		/* Value computed */
	eval_formal,		/* Formal computed */
	eval_exec,		/* Start a lambda evaluation */
	eval_cond,		/* Start next cond clause */
	eval_cond_test,		/* Check cond condition */
	eval_progn,		/* Start next progn entry */
	eval_while,		/* Start while condition */
	eval_while_test,	/* Check while condition */
	eval_macro,		/* Finished with macro generation */
};

struct ao_lisp_stack {
	uint8_t			state;		/* enum eval_state */
	ao_poly			prev;		/* previous stack frame */
	ao_poly			sexprs;		/* expressions to evaluate */
	ao_poly			values;		/* values computed */
	ao_poly			values_tail;	/* end of the values list for easy appending */
	ao_poly			frame;		/* current lookup frame */
	ao_poly			list;		/* most recent function call */
};

static inline struct ao_lisp_stack *
ao_lisp_poly_stack(ao_poly p)
{
	return ao_lisp_ref(p);
}

static inline ao_poly
ao_lisp_stack_poly(struct ao_lisp_stack *stack)
{
	return ao_lisp_poly(stack, AO_LISP_OTHER);
}

extern struct ao_lisp_stack	*ao_lisp_stack;
extern struct ao_lisp_stack	*ao_lisp_stack_free_list;
extern ao_poly			ao_lisp_v;

#define AO_LISP_FUNC_LAMBDA	0
#define AO_LISP_FUNC_NLAMBDA	1
#define AO_LISP_FUNC_MACRO	2
#define AO_LISP_FUNC_LEXPR	3

#define AO_LISP_FUNC_FREE_ARGS	0x80
#define AO_LISP_FUNC_MASK	0x7f

#define AO_LISP_FUNC_F_LAMBDA	(AO_LISP_FUNC_FREE_ARGS | AO_LISP_FUNC_LAMBDA)
#define AO_LISP_FUNC_F_NLAMBDA	(AO_LISP_FUNC_FREE_ARGS | AO_LISP_FUNC_NLAMBDA)
#define AO_LISP_FUNC_F_MACRO	(AO_LISP_FUNC_FREE_ARGS | AO_LISP_FUNC_MACRO)
#define AO_LISP_FUNC_F_LEXPR	(AO_LISP_FUNC_FREE_ARGS | AO_LISP_FUNC_LEXPR)

struct ao_lisp_builtin {
	uint8_t		type;
	uint8_t		args;
	uint16_t	func;
};

enum ao_lisp_builtin_id {
	builtin_eval,
	builtin_read,
	builtin_lambda,
	builtin_lexpr,
	builtin_nlambda,
	builtin_macro,
	builtin_car,
	builtin_cdr,
	builtin_cons,
	builtin_last,
	builtin_length,
	builtin_quote,
	builtin_set,
	builtin_setq,
	builtin_cond,
	builtin_progn,
	builtin_while,
	builtin_print,
	builtin_patom,
	builtin_plus,
	builtin_minus,
	builtin_times,
	builtin_divide,
	builtin_mod,
	builtin_equal,
	builtin_less,
	builtin_greater,
	builtin_less_equal,
	builtin_greater_equal,
	builtin_pack,
	builtin_unpack,
	builtin_flush,
	builtin_delay,
	builtin_led,
	builtin_save,
	builtin_restore,
	_builtin_last
};

typedef ao_poly (*ao_lisp_func_t)(struct ao_lisp_cons *cons);

extern const ao_lisp_func_t	ao_lisp_builtins[];

static inline ao_lisp_func_t
ao_lisp_func(struct ao_lisp_builtin *b)
{
	return ao_lisp_builtins[b->func];
}

struct ao_lisp_lambda {
	uint8_t		type;
	uint8_t		args;
	ao_poly		code;
	ao_poly		frame;
};

static inline struct ao_lisp_lambda *
ao_lisp_poly_lambda(ao_poly poly)
{
	return ao_lisp_ref(poly);
}

static inline ao_poly
ao_lisp_lambda_poly(struct ao_lisp_lambda *lambda)
{
	return ao_lisp_poly(lambda, AO_LISP_OTHER);
}

static inline void *
ao_lisp_poly_other(ao_poly poly) {
	return ao_lisp_ref(poly);
}

static inline uint8_t
ao_lisp_other_type(void *other) {
	return *((uint8_t *) other);
}

static inline ao_poly
ao_lisp_other_poly(const void *other)
{
	return ao_lisp_poly(other, AO_LISP_OTHER);
}

static inline int
ao_lisp_size_round(int size)
{
	return (size + 3) & ~3;
}

static inline int
ao_lisp_size(const struct ao_lisp_type *type, void *addr)
{
	return ao_lisp_size_round(type->size(addr));
}

#define AO_LISP_OTHER_POLY(other) ((ao_poly)(other) + AO_LISP_OTHER)

static inline int ao_lisp_poly_base_type(ao_poly poly) {
	return poly & AO_LISP_TYPE_MASK;
}

static inline int ao_lisp_poly_type(ao_poly poly) {
	int	type = poly & AO_LISP_TYPE_MASK;
	if (type == AO_LISP_OTHER)
		return ao_lisp_other_type(ao_lisp_poly_other(poly));
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
	return (int) ((ao_signed_poly) poly >> AO_LISP_TYPE_SHIFT);
}

static inline ao_poly
ao_lisp_int_poly(int i)
{
	return ((ao_poly) i << 2) | AO_LISP_INT;
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

extern int ao_lisp_collects[2];
extern int ao_lisp_freed[2];
extern int ao_lisp_loops[2];

/* returns 1 if the object was already marked */
int
ao_lisp_mark(const struct ao_lisp_type *type, void *addr);

/* returns 1 if the object was already marked */
int
ao_lisp_mark_memory(const struct ao_lisp_type *type, void *addr);

void *
ao_lisp_move_map(void *addr);

/* returns 1 if the object was already moved */
int
ao_lisp_move(const struct ao_lisp_type *type, void **ref);

/* returns 1 if the object was already moved */
int
ao_lisp_move_memory(const struct ao_lisp_type *type, void **ref);

void *
ao_lisp_alloc(int size);

#define AO_LISP_COLLECT_FULL		1
#define AO_LISP_COLLECT_INCREMENTAL	0

int
ao_lisp_collect(uint8_t style);

void
ao_lisp_cons_stash(int id, struct ao_lisp_cons *cons);

struct ao_lisp_cons *
ao_lisp_cons_fetch(int id);

void
ao_lisp_string_stash(int id, char *string);

char *
ao_lisp_string_fetch(int id);

void
ao_lisp_poly_stash(int id, ao_poly poly);

ao_poly
ao_lisp_poly_fetch(int id);

/* cons */
extern const struct ao_lisp_type ao_lisp_cons_type;

struct ao_lisp_cons *
ao_lisp_cons_cons(ao_poly car, struct ao_lisp_cons *cdr);

extern struct ao_lisp_cons *ao_lisp_cons_free_list;

void
ao_lisp_cons_free(struct ao_lisp_cons *cons);

void
ao_lisp_cons_print(ao_poly);

void
ao_lisp_cons_patom(ao_poly);

int
ao_lisp_cons_length(struct ao_lisp_cons *cons);

/* string */
extern const struct ao_lisp_type ao_lisp_string_type;

char *
ao_lisp_string_copy(char *a);

char *
ao_lisp_string_cat(char *a, char *b);

ao_poly
ao_lisp_string_pack(struct ao_lisp_cons *cons);

ao_poly
ao_lisp_string_unpack(char *a);

void
ao_lisp_string_print(ao_poly s);

void
ao_lisp_string_patom(ao_poly s);

/* atom */
extern const struct ao_lisp_type ao_lisp_atom_type;

extern struct ao_lisp_atom	*ao_lisp_atoms;
extern struct ao_lisp_frame	*ao_lisp_frame_global;
extern struct ao_lisp_frame	*ao_lisp_frame_current;

void
ao_lisp_atom_print(ao_poly a);

struct ao_lisp_atom *
ao_lisp_atom_intern(char *name);

ao_poly *
ao_lisp_atom_ref(struct ao_lisp_frame *frame, ao_poly atom);

ao_poly
ao_lisp_atom_get(ao_poly atom);

ao_poly
ao_lisp_atom_set(ao_poly atom, ao_poly val);

/* int */
void
ao_lisp_int_print(ao_poly i);

/* prim */
void
ao_lisp_poly_print(ao_poly p);

void
ao_lisp_poly_patom(ao_poly p);

int
ao_lisp_poly_mark(ao_poly p, uint8_t note_cons);

/* returns 1 if the object has already been moved */
int
ao_lisp_poly_move(ao_poly *p, uint8_t note_cons);

/* eval */

void
ao_lisp_eval_clear_globals(void);

int
ao_lisp_eval_restart(void);

ao_poly
ao_lisp_eval(ao_poly p);

ao_poly
ao_lisp_set_cond(struct ao_lisp_cons *cons);

/* builtin */
void
ao_lisp_builtin_print(ao_poly b);

extern const struct ao_lisp_type ao_lisp_builtin_type;

/* Check argument count */
ao_poly
ao_lisp_check_argc(ao_poly name, struct ao_lisp_cons *cons, int min, int max);

/* Check argument type */
ao_poly
ao_lisp_check_argt(ao_poly name, struct ao_lisp_cons *cons, int argc, int type, int nil_ok);

/* Fetch an arg (nil if off the end) */
ao_poly
ao_lisp_arg(struct ao_lisp_cons *cons, int argc);

char *
ao_lisp_args_name(uint8_t args);

/* read */
extern struct ao_lisp_cons	*ao_lisp_read_cons;
extern struct ao_lisp_cons	*ao_lisp_read_cons_tail;
extern struct ao_lisp_cons	*ao_lisp_read_stack;

ao_poly
ao_lisp_read(void);

/* rep */
ao_poly
ao_lisp_read_eval_print(void);

/* frame */
extern const struct ao_lisp_type ao_lisp_frame_type;

#define AO_LISP_FRAME_FREE	4

extern struct ao_lisp_frame	*ao_lisp_frame_free_list[AO_LISP_FRAME_FREE];

ao_poly
ao_lisp_frame_mark(struct ao_lisp_frame *frame);

ao_poly *
ao_lisp_frame_ref(struct ao_lisp_frame *frame, ao_poly atom);

struct ao_lisp_frame *
ao_lisp_frame_new(int num);

void
ao_lisp_frame_free(struct ao_lisp_frame *frame);

int
ao_lisp_frame_add(struct ao_lisp_frame **frame, ao_poly atom, ao_poly val);

void
ao_lisp_frame_print(ao_poly p);

/* lambda */
extern const struct ao_lisp_type ao_lisp_lambda_type;

struct ao_lisp_lambda *
ao_lisp_lambda_new(ao_poly cons);

void
ao_lisp_lambda_print(ao_poly lambda);

ao_poly
ao_lisp_lambda(struct ao_lisp_cons *cons);

ao_poly
ao_lisp_lexpr(struct ao_lisp_cons *cons);

ao_poly
ao_lisp_nlambda(struct ao_lisp_cons *cons);

ao_poly
ao_lisp_macro(struct ao_lisp_cons *cons);

ao_poly
ao_lisp_lambda_eval(void);

/* save */

ao_poly
ao_lisp_save(struct ao_lisp_cons *cons);

ao_poly
ao_lisp_restore(struct ao_lisp_cons *cons);

/* error */

extern const struct ao_lisp_type ao_lisp_stack_type;

void
ao_lisp_stack_print(void);

ao_poly
ao_lisp_error(int error, char *format, ...);

/* debugging macros */

#if DBG_EVAL
#define DBG_CODE	1
int ao_lisp_stack_depth;
#define DBG_DO(a)	a
#define DBG_INDENT()	do { int _s; for(_s = 0; _s < ao_lisp_stack_depth; _s++) printf("  "); } while(0)
#define DBG_IN()	(++ao_lisp_stack_depth)
#define DBG_OUT()	(--ao_lisp_stack_depth)
#define DBG_RESET()	(ao_lisp_stack_depth = 0)
#define DBG(...) 	printf(__VA_ARGS__)
#define DBGI(...)	do { DBG("%4d: ", __LINE__); DBG_INDENT(); DBG(__VA_ARGS__); } while (0)
#define DBG_CONS(a)	ao_lisp_cons_print(ao_lisp_cons_poly(a))
#define DBG_POLY(a)	ao_lisp_poly_print(a)
#define OFFSET(a)	((a) ? (int) ((uint8_t *) a - ao_lisp_pool) : -1)
#define DBG_STACK()	ao_lisp_stack_print()
static inline void
ao_lisp_frames_dump(void)
{
	struct ao_lisp_stack *s;
	DBGI(".. current frame: "); DBG_POLY(ao_lisp_frame_poly(ao_lisp_frame_current)); DBG("\n");
	for (s = ao_lisp_stack; s; s = ao_lisp_poly_stack(s->prev)) {
		DBGI(".. stack frame: "); DBG_POLY(s->frame); DBG("\n");
	}
}
#define DBG_FRAMES()	ao_lisp_frames_dump()
#else
#define DBG_DO(a)
#define DBG_INDENT()
#define DBG_IN()
#define DBG_OUT()
#define DBG(...)
#define DBGI(...)
#define DBG_CONS(a)
#define DBG_POLY(a)
#define DBG_RESET()
#define DBG_STACK()
#define DBG_FRAMES()
#endif

#define DBG_MEM		0
#define DBG_MEM_START	1

#if DBG_MEM

#include <assert.h>
extern int dbg_move_depth;
#define MDBG_DUMP 1
#define MDBG_OFFSET(a)	((int) ((uint8_t *) (a) - ao_lisp_pool))

extern int dbg_mem;

#define MDBG_DO(a)	a
#define MDBG_MOVE(...) do { if (dbg_mem) { int d; for (d = 0; d < dbg_move_depth; d++) printf ("  "); printf(__VA_ARGS__); } } while (0)
#define MDBG_MORE(...) do { if (dbg_mem) printf(__VA_ARGS__); } while (0)
#define MDBG_MOVE_IN()	(dbg_move_depth++)
#define MDBG_MOVE_OUT()	(assert(--dbg_move_depth >= 0))

#else

#define MDBG_DO(a)
#define MDBG_MOVE(...)
#define MDBG_MORE(...)
#define MDBG_MOVE_IN()
#define MDBG_MOVE_OUT()

#endif

#endif /* _AO_LISP_H_ */
