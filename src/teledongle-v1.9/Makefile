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
	ao_task.h \
	ao_whiten.h \
	stm32l.h \
	Makefile

#PROFILE=ao_profile.c
#PROFILE_DEF=-DAO_PROFILE=1

#SAMPLE_PROFILE=ao_sample_profile.c \
#	ao_sample_profile_timer.c
#SAMPLE_PROFILE_DEF=-DHAS_SAMPLE_PROFILE=1

#STACK_GUARD=ao_mpu_stm.c
#STACK_GUARD_DEF=-DHAS_STACK_GUARD=1

#	ao_monitor.c \
#	ao_rssi.c \
#	ao_send_packet.c \
#	ao_packet_master.c \
#	ao_packet.c


ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cc1200.c \
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
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_eeprom_stm.c

PRODUCT=TeleDongle-v1.9
PRODUCT_DEF=-DTELEDONGLE
IDPRODUCT=0x000c

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) $(SAMPLE_PROFILE_DEF) $(STACK_GUARD_DEF) -Os -g

PROGNAME=teledongle-v1.9
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_teledongle.c
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
