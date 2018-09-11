#
# Tiny AltOS build
#
#
vpath % ../attiny:../drivers:../kernel:../product:..
vpath ao-make-product.5c ../util
vpath make-altitude-pa ../util

include ../avr/Makefile.defs

PROGNAME=microkite-v0.1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SCRIPT=microkite-load

PUBLISH_DIR=$(HOME)/altusmetrumllc/Binaries
PUBLISH_HEX=$(PUBLISH_DIR)/$(HEX)
PUBLISH_SCRIPT=$(PUBLISH_DIR)/$(SCRIPT)

MCU=attiny85
DUDECPUTYPE=t85
#PROGRAMMER=stk500v2 -P usb
LOADSLOW=-i 32 -B 32
LOADARG=-p $(DUDECPUTYPE) -c $(PROGRAMMER) -e -U flash:w:

#LDFLAGS=-L$(LDSCRIPTS) -Tavr25.x

ALTOS_SRC = \
	ao_micropeak.c \
	ao_spi_attiny.c \
	ao_led_tiny.c \
	ao_clock.c \
	ao_ms5607.c \
	ao_exti.c \
	ao_convert_pa.c \
	ao_report_micro.c \
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
	altitude-pa.h

IDPRODUCT=0
PRODUCT=MicroKite-v0.1
PRODUCT_DEF=-DMICROPEAK
CFLAGS = $(PRODUCT_DEF) -I. -I../attiny -I../kernel -I.. -I../drivers -I../product
CFLAGS += -g -mmcu=$(MCU) -Wall -Wstrict-prototypes -O2 -mcall-prologues -DATTINY

NICKLE=nickle

SRC=$(ALTOS_SRC)
OBJ=$(SRC:.c=.o)

V=0
# The user has explicitly enabled quiet compilation.
ifeq ($(V),0)
quiet = @printf "  $1 $2 $@\n"; $($1)
endif
# Otherwise, print the full command line.
quiet ?= $($1)

all: $(PROG) $(HEX) microkite-load

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

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

ao_product.o: ao_product.c ao_product.h

%.o : %.c $(INC)
	$(call quiet,CC) -c $(CFLAGS) $<

distclean:	clean

clean:
	rm -f *.o $(PROG) $(HEX) $(SCRIPT)
	rm -f ao_product.h

publish: $(PUBLISH_HEX) $(PUBLISH_SCRIPT)

$(PUBLISH_HEX): $(HEX)
	cp -a $(HEX) $@

$(PUBLISH_SCRIPT): $(SCRIPT)
	cp -a $(SCRIPT) $@

load-product:
	./$(SCRIPT) fast

load-product-slow:
	./$(SCRIPT) slow

../altitude-pa.h: make-altitude-pa
	nickle $< > $@

$(SCRIPT): $(SCRIPT).tmpl Makefile ../Version
	sed -e 's/%HEX%/$(HEX)/' -e 's/%LOADCMD%/$(LOADCMD)/' -e 's/%LOADARG%/$(LOADARG)/' -e 's/%LOADSLOW%/$(LOADSLOW)/' $(SCRIPT).tmpl > $@ || (rm $@ && exit 1)
	chmod +x $@

install:

uninstall:

$(OBJ): ao_product.h $(INC)
