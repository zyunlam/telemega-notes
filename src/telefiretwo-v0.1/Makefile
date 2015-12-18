#
# TeleFire build file
#

include ../stm/Makefile.defs

INC = \
	ao.h \
	ao_pins.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_pad.h \
	ao_product.h \
	ao_radio_spi.h \
	ao_radio_cmac.h \
	ao_cc1200_CC1200.h \
	ao_cc1200.h \
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
	ao_adc_stm.c \
	ao_data.c \
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
	ao_eeprom_stm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_cc1200.c \
	ao_radio_cmac.c \
	ao_aes.c \
	ao_aes_tables.c \
	ao_pad.c \
	ao_radio_cmac_cmd.c

PRODUCT_SRC = \
	ao_telefiretwo.c

PRODUCT=TeleFire-v0.1
PRODUCT_DEF=-DTELEFIRETWO_V_0_1
IDPRODUCT=0x000f

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) -Os -g

PROGNAME = telefiretwo-v0.1
PROG = $(PROGNAME)-$(VERSION).elf
HEX = $(PROGNAME)-$(VERSION).ihx

SRC = $(ALTOS_SRC) $(PRODUCT_SRC)
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

