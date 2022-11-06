#
# AltOS build
#
#

include ../samd21/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	samd21.h \
	Makefile

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_romconfig.c \
	ao_interrupt.c \
	ao_product.c \
	ao_cmd.c \
	ao_task.c \
	ao_led.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_dma_samd21.c \
	ao_usb_samd21.c \
	ao_spi_samd21.c

# SAMD21G18A

SAMD21_ROM=256
SAMD21_RAM=32

PRODUCT=SnekBoard
PRODUCT_DEF=-DSNEKBOARD
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(SAMD21_CFLAGS)

PROGNAME=snekboard
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) snekboard.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) 
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

load: $(PROG)
	stm-load $(PROG)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
