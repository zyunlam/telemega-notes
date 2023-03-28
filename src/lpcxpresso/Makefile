#
# AltOS build
#
#

include ../lpc/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_pins.h \
	ao_product.h \
	lpc.h

#
# Common AltOS sources
#
ALTOS_SRC = \
	ao_interrupt.c \
	ao_boot_chain.c \
	ao_romconfig.c \
	ao_product.c \
	ao_panic.c \
	ao_led_lpc.c \
	ao_task.c \
	ao_cmd.c \
	ao_timer_lpc.c \
	ao_serial_lpc.c \
	ao_usb_lpc.c \
	ao_stdio.c

PRODUCT=LpcDemo-v0.0
PRODUCT_DEF=-DLPC_DEMO
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(LPC_CFLAGS)

PROG=lpc-demo.elf

SRC=$(ALTOS_SRC) ao_demo.c
OBJ=$(SRC:.c=.o)

all: $(PROG)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS) -Wl,-Map=$(PROGNAME)-$(VERSION).map

$(OBJ): $(INC)

load: $(PROG)
	lpc-load $(PROG)

distclean:	clean

clean:
	rm -f *.o $(PROG)
	rm -f ao_product.h

install:

uninstall:
