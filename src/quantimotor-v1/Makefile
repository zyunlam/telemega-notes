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
	ao_mutex.c \
	ao_panic.c \
	ao_stdio.c \
	ao_storage.c \
	ao_data.c \
	ao_convert_volt.c \
	ao_task.c \
	ao_cmd.c \
	ao_timer_lpc.c \
	ao_romconfig.c \
	ao_adc_lpc.c \
	ao_serial_lpc.c \
	ao_usb_lpc.c \
	ao_led.c

PRODUCT=QuantiMotor-v1
PRODUCT_DEF=-DQUANTIMOTOR_V_1
IDPRODUCT=0x002d

CFLAGS = $(PRODUCT_DEF) $(LPC_CFLAGS)

PROGNAME=quantimotor-v3
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_quantimotor.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h ao_serial_lpc.h

install:

uninstall:
