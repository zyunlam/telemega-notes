#
# Tiny AltOS build
#
#

include ../stmf0/Makefile.defs

PUBLISH_DIR=$(HOME)/altusmetrumllc/Binaries
PUBLISH_HEX=$(PUBLISH_DIR)/$(HEX)

MICRO_SRC=\
	ao_report_micro.c \
	ao_log_micro.c \
	ao_microflight.c \
	ao_microkalman.c

ALTOS_SRC = \
	ao_micropeak.c \
	ao_spi_stm.c \
	ao_dma_stm.c \
	ao_led.c \
	ao_timer.c \
	ao_ms5607.c \
	ao_exti_stm.c \
	ao_convert_pa.c \
	ao_romconfig.c \
	ao_product.c \
	ao_panic.c \
	ao_stdio.c \
	ao_serial_stm.c \
	ao_usb_stm.c \
	ao_mutex.c \
	ao_interrupt.c \
	ao_cmd.c \
	ao_task.c \
	ao_data.c \
	ao_boot_chain.c \
	ao_microflight.c \
	ao_report_micro.c \
	ao_storage_stm.c \
	ao_storage.c \
	ao_log_micro.c \
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
	altitude-pa.h \
	stm32f0.h

IDPRODUCT=0x14
PRODUCT=MicroPeak-v2.0
PRODUCT_DEF=-DMICROPEAK

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS) -g -Os

PROGNAME=micropeak-v2.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC)
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

LDFLAGS=$(CFLAGS) -L$(TOPDIR)/stmf0 -Wl,-Tmicropeak.ld -n

$(PROG): Makefile $(OBJ) micropeak.ld
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS)

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

ao_product.o: ao_product.c ao_product.h

distclean:	clean

clean:
	rm -f *.o *.elf *.ihx
	rm -f ao_product.h

publish: $(PUBLISH_HEX)

$(PUBLISH_HEX): $(HEX)
	cp -a $(HEX) $@

../altitude-pa.h: make-altitude-pa
	nickle $< > $@

install:

uninstall:

$(OBJ): ao_product.h $(INC)
