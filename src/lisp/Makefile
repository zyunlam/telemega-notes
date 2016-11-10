all: ao_lisp_const.h

clean:
	rm -f ao_lisp_const.h $(OBJS) ao_lisp_make_const

ao_lisp_const.h: ao_lisp_const.lisp ao_lisp_make_const
	./ao_lisp_make_const < ao_lisp_const.lisp > $@

SRCS=\
	ao_lisp_make_const.c\
	ao_lisp_mem.c \
	ao_lisp_cons.c \
	ao_lisp_string.c \
	ao_lisp_atom.c \
	ao_lisp_int.c \
	ao_lisp_poly.c \
	ao_lisp_builtin.c \
	ao_lisp_read.c \
	ao_lisp_frame.c \
	ao_lisp_lambda.c \
	ao_lisp_eval.c \
	ao_lisp_error.c 

OBJS=$(SRCS:.c=.o)

CFLAGS=-DAO_LISP_MAKE_CONST -O0 -g -I.

HDRS=\
	ao_lisp.h \
	ao_lisp_read.h

ao_lisp_make_const:  $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

$(OBJS): $(HDRS)
