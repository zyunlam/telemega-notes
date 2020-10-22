#
# AltOS build
#
#

include ../stmf0/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_pins.h \
	ao_product.h \
	ao_task.h \
	ao_adc_fast.h \
	ao_power.h \
	ao_flash_readout.h \
	ao_crc.h \
	stm32f0.h

#
# Common AltOS sources
#
ALTOS_SRC = \
	ao_interrupt.c \
	ao_timer.c \
	ao_panic.c \
	ao_mutex.c \
	ao_dma_stm.c \
	ao_adc_fast.c \
	ao_crc_stm.c \
	ao_stdio.c \
	ao_led_stmf0.c \
	ao_romconfig.c \
	ao_boot_chain.c \
	ao_usb_stm.c \
	ao_trng_send.c \
	ao_flash_readout.c \
	ao_task.c \
	ao_power.c \
	ao_gpio.c \
	ao_product.c

VENDOR=AltusMetrum
PROJECT_NAME=ChaosKey
PRODUCT=ChaosKey-hw-1.0-sw-$(VERSION)
PRODUCT_DEF=-DCHAOSKEY_V_1_0
IDVENDOR=0x1d50
IDPRODUCT=0x60c6

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS)

PROGNAME=chaoskey-v1.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx
BIN=$(PROGNAME)-all-$(VERSION).bin
METAINFO=org.altusmetrum.ChaosKey.metainfo.xml

SRC=$(ALTOS_SRC) ao_chaoskey.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX) $(BIN)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

$(BIN): $(PROG) $(LOADER)
	$(MAKEBIN) --output=$@ --base=$(FLASH_ADDR) $(LOADER) $(PROG)

$(LOADER):
	+cd flash-loader && make

%.cab: $(PROG) $(HEX) $(METAINFO)
	gcab --create --nopath $@ $(PROG) $(HEX) $(METAINFO)

cab: $(VENDOR)-$(PROJECT_NAME)-$(VERSION).cab

check: $(METAINFO)
	appstream-util validate-relax $(METAINFO)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx *.bin $(PROGNAME)-*.map
	rm -f ao_product.h
	rm -f *.cab

install:

uninstall:
