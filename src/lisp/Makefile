all: ao_lisp_builtin.h ao_lisp_const.h

clean:
	rm -f ao_lisp_const.h ao_lisp_builtin.h $(OBJS) ao_lisp_make_const

ao_lisp_const.h: ao_lisp_const.lisp ao_lisp_make_const
	./ao_lisp_make_const -o $@ ao_lisp_const.lisp

ao_lisp_builtin.h: ao_lisp_make_builtin ao_lisp_builtin.txt
	nickle ./ao_lisp_make_builtin ao_lisp_builtin.txt > $@

include Makefile-inc
SRCS=$(LISP_SRCS) ao_lisp_make_const.c

HDRS=$(LISP_HDRS)

OBJS=$(SRCS:.c=.o)

CFLAGS=-DAO_LISP_MAKE_CONST -O0 -g -I. -Wall -Wextra -no-pie

ao_lisp_make_const:  $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

$(OBJS): $(HDRS)
