#
# Tiny AltOS build
#
#

include ../stm32l0/Makefile.defs

PUBLISH_DIR=$(HOME)/altusmetrumllc/Binaries
PUBLISH_HEX=$(PUBLISH_DIR)/$(HEX)

MICRO_SRC=\
	ao_report_micro.c \
	ao_log_micro.c \
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
	ao_lpuart.h \
	altitude-pa.h \
	ao_product.h \
	stm32l0.h

ALTOS_SRC = \
	ao_interrupt.c \
	ao_panic.c \
	ao_micro.c \
	ao_report_micro.c \
	ao_stdio.c \
	ao_notask.c \
	ao_serial_stm.c \
	ao_lpuart_stm.c \
	ao_timer.c \
	ao_spi_stm32l0.c \
	ao_adc_stm32l0.c \
	ao_ms5607.c \
	ao_exti_stm.c \
	ao_convert_pa.c \
	ao_led.c \
	ao_cmd.c \
	ao_product.c \
	ao_romconfig.c \
	ao_flash_stm32l0.c \
	ao_microflight.c \
	ao_log_micro.c \
	ao_microkalman.c

IDPRODUCT=0x14
PRODUCT=MicroPeak-v2.0
PRODUCT_DEF=-DMICROPEAK

CFLAGS = $(PRODUCT_DEF) $(STML0_CFLAGS) -Wl,--gc-sections -ffunction-sections -fdata-sections

PROGNAME=micropeak-v2.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC)
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

LDFLAGS=-nostartfiles $(CFLAGS) -L$(TOPDIR)/stm32l0 -Taltos.ld -n

$(PROG): Makefile $(OBJ) micropeak.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS) -Wl,-Map=$(PROGNAME)-$(VERSION).map

distclean:	clean

clean:
	rm -f *.o *.elf *.ihx *.map
	rm -f ao_product.h

publish: $(PUBLISH_HEX)

$(PUBLISH_HEX): $(HEX)
	cp -a $(HEX) $@

install:

uninstall:

$(OBJ): $(INC)
