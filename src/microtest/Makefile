#
# Tiny AltOS build
#
#
TOPDIR=..
include $(TOPDIR)/attiny/Makefile.defs

PROGNAME=microtest-v1.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

ALTOS_SRC = \
	ao_microtest.c \
	ao_spi_attiny.c \
	ao_led_tiny.c \
	ao_clock.c \
	ao_ms5607.c \
	ao_exti.c \
	ao_notask.c \
	ao_eeprom_tiny.c \
	ao_panic.c \
	ao_log_micro.c \
	ao_async.c \
	ao_microflight.c \
	ao_microkalman.c

INC=\
	ao.h \
	ao_pins.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_exti.h \
	ao_ms5607.h \
	ao_log_micro.h \
	ao_micropeak.h \
	ao_product.h \
	altitude-pa.h

IDPRODUCT=0
PRODUCT=MicroTest-v1.0
PRODUCT_DEF=-DMICROPEAK
CFLAGS = $(PRODUCT_DEF) $(ATTINY_CFLAGS)

SRC=$(ALTOS_SRC)
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

CHECK=sh ../util/check-avr-mem

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ)
	$(call quiet,CHECK) $(PROG) || ($(RM) -f $(PROG); exit 1)

$(HEX): $(PROG)
	avr-size $(PROG)
	$(OBJCOPY) -R .eeprom -O ihex $(PROG) $@

load: $(HEX)
	$(LOADCMD) $(LOADARG)$(HEX)

load-slow: $(HEX)
	$(LOADCMD) $(LOADSLOW) $(LOADARG)$(HEX)

distclean:	clean

clean:
	rm -f *.o *.elf *.ihx *.map
	rm -f ao_product.h

load-product:
	./$(SCRIPT) fast

load-product-slow:
	./$(SCRIPT) slow

install:

uninstall:

$(OBJ): $(INC)
