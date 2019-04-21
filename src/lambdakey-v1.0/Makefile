#
# AltOS build
#
#

include ../stmf0/Makefile.defs

aoschemelib=$(shell pkg-config --variable=aoschemelib ao-scheme)

include $(aoschemelib)/Makefile-scheme

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	$(SCHEME_HDRS) \
	ao_scheme_const.h \
	stm32f0.h \
	Makefile

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_cmd.c \
	ao_led_stmf0.c \
	ao_notask.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_usb_stm.c \
	ao_romconfig.c \
	$(SCHEME_SRCS)

PRODUCT=LambdaKey-v1.0
PRODUCT_DEF=-DLAMBDAKEY
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) -I. -I$(aoschemelib) $(STMF0_CFLAGS)

PROGNAME=lambdakey-v1.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_lambdakey.c
OBJ=$(SRC:.c=.o)

bletch:
	echo lib is $(aoschemelib)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS) -Wl,-M=$(PROGNAME).map

$(OBJ): $(INC)

ao_scheme_const.h: ao-scheme-make-const ao_scheme_basic_syntax.scheme
	$^ -o $@ -d FLOAT,VECTOR,QUASI,BIGINT,POSIX,PORT,SAVE,UNDEF

load: $(PROG)
	stm-load $(PROG)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx ao_scheme_const.h
	rm -f ao_product.h

install:

uninstall:
