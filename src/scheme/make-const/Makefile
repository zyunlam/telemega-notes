include ../Makefile-inc

vpath %.o .
vpath %.c ..
vpath %.h ..

SRCS=$(SCHEME_SRCS) ao_scheme_make_const.c
HDRS=$(SCHEME_HDRS) ao_scheme_os.h

OBJS=$(SRCS:.c=.o)

CC=cc
CFLAGS=-DAO_SCHEME_MAKE_CONST -O0 -g -I. -Wall -Wextra -Wpointer-arith -Wmissing-declarations -Wformat=2 -Wstrict-prototypes -Wmissing-prototypes -Wnested-externs -Wbad-function-cast -Wold-style-definition -Wdeclaration-after-statement -Wunused -Wuninitialized -Wshadow -Wmissing-noreturn -Wmissing-format-attribute -Wredundant-decls -Wlogical-op -Werror=implicit -Werror=nonnull -Werror=init-self -Werror=main -Werror=missing-braces -Werror=sequence-point -Werror=return-type -Werror=trigraphs -Werror=array-bounds -Werror=write-strings -Werror=address -Werror=int-to-pointer-cast -Werror=pointer-to-int-cast

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@

all: ao_scheme_make_const

ao_scheme_make_const: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ -lm

clean:
	rm -f $(OBJS) ao_scheme_make_const

$(OBJS): $(SCHEME_HDRS)
