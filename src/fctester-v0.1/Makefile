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
	ao_product.c \
	ao_romconfig.c \
	ao_mutex.c \
	ao_panic.c \
	ao_stdio.c \
	ao_task.c \
	ao_cmd.c \
	ao_timer_lpc.c \
	ao_exti_lpc.c \
	ao_usb_lpc.c \
	ao_led_lpc.c

PRODUCT=fctester-v0.1
PRODUCT_DEF=-DFCTESTER_V_0_1
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(LPC_CFLAGS)

PROGNAME=fctester-v0.1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_fctester.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

load: $(PROG)
	lpc-load $(PROG)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map ao_product.h

install:

uninstall:
