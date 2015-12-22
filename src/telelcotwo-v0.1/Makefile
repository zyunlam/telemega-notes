#
# AltOS build for TeleLCO
#
#

include ../stm/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_companion.h \
	ao_data.h \
	ao_sample.h \
	ao_pins.h \
	ao_product.h \
	ao_radio_spi.h \
	ao_radio_cmac.h \
	ao_cc1200_CC1200.h \
	ao_cc1200.h \
	ao_debounce.h \
	stm32l.h

#
# Common AltOS sources
#

#PROFILE=ao_profile.c
#PROFILE_DEF=-DAO_PROFILE=1

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_task.c \
	ao_led.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_freq.c \
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_beep_stm.c \
	ao_eeprom_stm.c \
	ao_fast_timer.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_cc1200.c \
	ao_radio_cmac.c \
	ao_aes.c \
	ao_aes_tables.c \
	ao_fec_tx.c \
	ao_fec_rx.c \
	ao_debounce.c \
	ao_button.c \
	ao_event.c \
	ao_lco_two.c \
	ao_lco_func.c \
	ao_lco_cmd.c \
	ao_radio_cmac_cmd.c

PRODUCT=TeleLCOTwo-v0.1
PRODUCT_DEF=-DTELELCOTWO
IDPRODUCT=0x0023

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) -Os -g

PROGNAME=telelcotwo-v0.1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_telelcotwo.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h

install:

uninstall:
