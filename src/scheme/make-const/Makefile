include ../Makefile-inc

vpath %.o .
vpath %.c ..
vpath %.h ..

SRCS=$(SCHEME_SRCS) ao_scheme_make_const.c
HDRS=$(SCHEME_HDRS) ao_scheme_os.h

OBJS=$(SRCS:.c=.o)

CC=cc
CFLAGS=-DAO_SCHEME_MAKE_CONST -O0 -g -I. -Wall -Wextra

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@

all: ao_scheme_make_const

ao_scheme_make_const: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ -lm

clean:
	rm -f $(OBJS) ao_scheme_make_const

$(OBJS): $(SCHEME_HDRS)
