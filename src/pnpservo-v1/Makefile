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
	stm32f0.h \
	Makefile

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_task.c \
	ao_led_stmf0.c \
	ao_dma_stm.c \
	ao_stdio.c \
	ao_mutex.c \
	ao_panic.c \
	ao_timer.c \
	ao_usb_stm.c \
	ao_flash_stm.c 

PRODUCT=PNPservo-v1
PRODUCT_DEF=-DPNPSERVO
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) -I. $(STMF0_CFLAGS)

LDFLAGS=$(CFLAGS) -L$(TOPDIR)/stmf0 -Wl,-Tlambda.ld

PROGNAME=pnpservo-v1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_pnpservo.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) lambda.ld altos.ld
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

load: $(PROG)
	stm-load $(PROG)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h

install:

uninstall:
