include ../Makefile-inc

vpath %.o .
vpath %.c ..
vpath %.h ..

SRCS=$(SCHEME_SRCS) ao_scheme_test.c

OBJS=$(SRCS:.c=.o)

CFLAGS=-O2 -g -Wall -Wextra -I. -I..

ao_scheme_test: $(OBJS)
	cc $(CFLAGS) -o $@ $(OBJS) -lm

$(OBJS): $(SCHEME_HDRS)

clean::
	rm -f $(OBJS) ao_scheme_test
