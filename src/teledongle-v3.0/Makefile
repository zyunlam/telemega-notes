#
# AltOS build
#
#

include ../lpc/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	ao_whiten.h \
	lpc.h \
	ao_cc1200.h \
	ao_cc1200_CC1200.h \
	Makefile

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cc1200.c \
	ao_cmd.c \
	ao_config.c \
	ao_task.c \
	ao_led_lpc.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer_lpc.c \
	ao_mutex.c \
	ao_freq.c \
	ao_spi_lpc.c \
	ao_usb_lpc.c \
	ao_exti_lpc.c \
	ao_send_packet.c \
	ao_monitor.c \
	ao_packet_master.c \
	ao_packet.c

PRODUCT=TeleDongle-v3.0
PRODUCT_DEF=-DTELEDONGLE
IDPRODUCT=0x000c

CFLAGS = $(PRODUCT_DEF) $(LPC_CFLAGS)

PROGNAME=teledongle-v3.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_teledongle.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

load: $(PROG)
	lpc-load $(PROG)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
