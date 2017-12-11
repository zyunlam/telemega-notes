#
# AltOS build
#
#

include ../stmf0/Makefile.defs

include ../scheme/Makefile-inc

NEWLIB_FULL=-lm -lc -lgcc

LIBS=$(NEWLIB_FULL)

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	$(SCHEME_HDRS) \
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
	ao_led.c \
	ao_dma_stm.c \
	ao_stdio.c \
	ao_mutex.c \
	ao_panic.c \
	ao_timer.c \
	ao_usb_stm.c \
	ao_flash_stm.c \
	$(SCHEME_SRCS) \
	ao_scheme_os_save.c

PRODUCT=LambdaKey-v1.0
PRODUCT_DEF=-DLAMBDAKEY
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) -I. $(STMF0_CFLAGS) -Os -g

LDFLAGS=$(CFLAGS) -L$(TOPDIR)/stmf0 -Wl,-Tlambda.ld

MAP=$(PROG).map
NEWLIB=/local/newlib-mini
MAPFILE=-Wl,-M=$(MAP)
LDFLAGS=-L../stmf0 -L$(NEWLIB)/arm-none-eabi/lib/thumb/v6-m/ -Wl,-Tlambda.ld $(MAPFILE) -nostartfiles
AO_CFLAGS=-I. -I../stmf0 -I../kernel -I../drivers -I.. -I../scheme -isystem $(NEWLIB)/arm-none-eabi/include

PROGNAME=lambdakey-v1.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_lambdakey.c
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
