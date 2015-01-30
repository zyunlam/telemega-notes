#
# AltOS build
#
#

include ../stm/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_product.h \
	ao_cc1200_CC1200.h \
	ao_profile.h \
	ao_task.h \
	stm32l.h \
	Makefile

#PROFILE=ao_profile.c
#PROFILE_DEF=-DAO_PROFILE=1

#SAMPLE_PROFILE=ao_sample_profile.c \
#	ao_sample_profile_timer.c
#SAMPLE_PROFILE_DEF=-DHAS_SAMPLE_PROFILE=1

#STACK_GUARD=ao_mpu_stm.c
#STACK_GUARD_DEF=-DHAS_STACK_GUARD=1

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_data.c \
	ao_task.c \
	ao_led.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_serial_stm.c \
	ao_freq.c \
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_cc1200.c \
	ao_adc_stm.c \
	ao_btm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_eeprom_stm.c \
	ao_convert_volt.c \
	ao_packet_master.c \
	ao_packet.c \
	$(PROFILE) \
	$(SAMPLE_PROFILE) \
	$(STACK_GUARD)

PRODUCT=TeleBT-v3.0
PRODUCT_DEF=-DTELEBT_V_3_0
IDPRODUCT=0x000e

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) $(SAMPLE_PROFILE_DEF) $(STACK_GUARD_DEF) -Os -g

PROGNAME=telebt-v3.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_telebt.c
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
