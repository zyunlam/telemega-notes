include ../Makefile-inc

vpath %.o .
vpath %.c ..
vpath %.h ..
vpath %.scheme ..
vpath ao_scheme_make_const ../make-const

SRCS=$(SCHEME_SRCS) ao_scheme_test.c
HDRS=$(SCHEME_HDRS) ao_scheme_const.h

OBJS=$(SRCS:.c=.o)

#PGFLAGS=-pg -no-pie
OFLAGS=-O3
#DFLAGS=-O0

CFLAGS=$(DFLAGS) $(OFLAGS) $(PGFLAGS) -g -Wall -Wextra -I. -I.. -Wpointer-arith -Wmissing-declarations -Wformat=2 -Wstrict-prototypes -Wmissing-prototypes -Wnested-externs -Wbad-function-cast -Wold-style-definition -Wdeclaration-after-statement -Wunused -Wuninitialized -Wshadow -Wmissing-noreturn -Wmissing-format-attribute -Wredundant-decls -Wlogical-op -Werror=implicit -Werror=nonnull -Werror=init-self -Werror=main -Werror=missing-braces -Werror=sequence-point -Werror=return-type -Werror=trigraphs -Werror=array-bounds -Werror=write-strings -Werror=address -Werror=int-to-pointer-cast -Werror=pointer-to-int-cast

ao-scheme: $(OBJS)
	cc $(CFLAGS) -o $@ $(OBJS) -lm

$(OBJS): $(HDRS)

ao_scheme_const.h: ao_scheme_make_const $(SCHEME_SCHEME)
	$^ -o $@

clean::
	rm -f $(OBJS) ao-scheme ao_scheme_const.h

install: ao-scheme
	install -t $$HOME/bin $^
