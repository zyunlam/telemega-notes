#
# AltOS build
#
#

include ../stmf0/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	ao_whiten.h \
	stm32f0.h \
	Makefile

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_dma_stm.c \
	ao_task.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_usb_stm.c \
	ao_power.c \
	ao_serial_stm.c

PRODUCT=MicroPeakUSB-v3.0
PRODUCT_DEF=-DMPUSB
IDPRODUCT=0x002b

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS) -Os -g

PROGNAME=mpusb-v3.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_mpusb.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS)

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h

install:

uninstall:
