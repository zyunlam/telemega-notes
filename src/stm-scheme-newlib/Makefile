#
# AltOS build
#
#

include ../stm/Makefile.defs
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
	$(SCHEME_HDRS)

#
# Common AltOS sources
#
ALTOS_SRC = \
	ao_interrupt.c \
	ao_boot_chain.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_task.c \
	ao_led_stm.c \
	ao_stdio_newlib.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_dma_stm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	$(SCHEME_SRCS)

PRODUCT=StmScheme-v0.0
PRODUCT_DEF=-DSTM_SCHEME
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS)

PROG=stm-scheme-$(VERSION)
ELF=$(PROG).elf
IHX=$(PROG).ihx
LIBSYMS=$(PROG).syms
MAP=$(PROG).map

NEWLIB=/local/newlib-mini
MAPFILE=-Wl,-M=$(MAP)
LDFLAGS=-L../stm -L$(NEWLIB)/arm-none-eabi/lib/thumb/v7-m/ -Wl,-Taltos.ld $(MAPFILE) -nostartfiles
AO_CFLAGS=-I. -I../stm -I../kernel -I../drivers -I.. -I../scheme -isystem $(NEWLIB)/arm-none-eabi/include -DNEWLIB

SRC=$(ALTOS_SRC) ao_demo.c
OBJ=$(SRC:.c=.o)

all: $(ELF) $(IHX) $(LIBSYMS)

$(ELF): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $@ $(OBJ) $(LIBS)

$(LIBSYMS): $(ELF)
	grep '^                         ' $(MAP) | grep -v 'size before relaxing' > $@

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o *.elf *.ihx *.map *.syms
	rm -f ao_product.h

install:

uninstall:
