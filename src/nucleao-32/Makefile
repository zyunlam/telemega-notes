#
# AltOS build
#
#

TOPDIR=..

include $(TOPDIR)/stmf0/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	stm32f0.h \
	Makefile

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_data.c \
	ao_task.c \
	ao_led_stmf0.c \
	ao_dma_stm.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_usb_stm.c \
	ao_flash_stm.c \
	ao_spi_stm.c \
	ao_bmi088.c

PRODUCT=Nucleo-32
PRODUCT_DEF=-DNUCLEO
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS) $(PROFILE_DEF)

PROGNAME=nucleo-32
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_nucleo.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS) -Wl,-Map=$(PROGNAME)-$(VERSION).map

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
