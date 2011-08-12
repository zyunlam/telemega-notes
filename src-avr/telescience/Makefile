#
# AltOS build
#
#
vpath %.c ..
vpath %.h ..
vpath make-altitude ..
vpath make-kalman ..
vpath kalman.5c ../kalman
vpath kalman_filter.5c ../kalman
vpath load_csv.5c ../kalman
vpath matrix.5c ../kalman
vpath ao-make-product.5c ..

MCU=atmega32u4
DUDECPUTYPE=m32u4
#PROGRAMMER=stk500v2 -P usb
PROGRAMMER=usbtiny
LOADCMD=avrdude
LOADARG=-p $(DUDECPUTYPE) -c $(PROGRAMMER) -e -U flash:w:
CC=avr-gcc
OBJCOPY=avr-objcopy

ifndef VERSION
include ../Version
endif

INC = \
	ao.h \
	ao_usb.h \
	ao_pins.h \
	altitude.h \
	ao_kalman.h

#
# Common AltOS sources
#
TELESCIENCE_STORAGE= \
	ao_m25.c \
	ao_spi_usart.c \
	ao_storage.c \

ALTOS_SRC = \
	ao_cmd.c \
	ao_mutex.c \
	ao_panic.c \
	ao_product.c \
	ao_stdio.c \
	ao_task.c \
	ao_timer.c \
	ao_led.c \
	ao_usb_avr.c \
	ao_adc_avr.c \
	ao_spi_slave.c \
	ao_log_telescience.c \
	$(TELESCIENCE_STORAGE)

PRODUCT=TeleScience-v0.1
MCU=atmega32u4
PRODUCT_DEF=-DTELESCIENCE
IDPRODUCT=0x000a
CFLAGS += -g -mmcu=$(MCU) -Wall -Wstrict-prototypes -O3 -mcall-prologues $(PRODUCT_DEF) -I. -DAVR

NICKLE=nickle

PROG=telescience-v0.1

SRC=$(ALTOS_SRC) ao_telescience.c
OBJ=$(SRC:.c=.o)

V=0
# The user has explicitly enabled quiet compilation.
ifeq ($(V),0)
quiet = @printf "  $1 $2 $@\n"; $($1)
endif
# Otherwise, print the full command line.
quiet ?= $($1)

all: $(PROG)

CHECK=sh ../check-avr-mem

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ)
	$(call quiet,CHECK) $(PROG) || ($(RM) -f $(PROG); exit 1)

$(PROG).hex: $(PROG)
	avr-size $(PROG)
	$(OBJCOPY) -R .eeprom -O ihex $(PROG) $@


load: $(PROG).hex
	$(LOADCMD) $(LOADARG)$(PROG).hex

../altitude.h: make-altitude
	nickle $< > $@

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

ao_product.rel: ao_product.c ao_product.h
	$(call quiet,CC) -c $(CFLAGS) -D PRODUCT_DEFS='\"ao_product.h\"' -o$@ $<

distclean:	clean

clean:
	rm -f $(OBJ)
	rm -f ao_product.h

install:

uninstall:

$(OBJ): ao.h ao_product.h ao_usb.h
