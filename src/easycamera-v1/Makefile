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
	ao_data.h \
	ao_pins.h \
	ao_product.h \
	ao_profile.h \
	ao_task.h \
	stm32l.h \
	Makefile

#
# Common AltOS sources
#

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_task.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_serial_stm.c \
	ao_dma_stm.c \
	ao_data.c \
	ao_adc_stm.c \
	ao_beep_stm.c \
	ao_eeprom_stm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_report.c \
	ao_convert_volt.c

PRODUCT=EasyCamera-v1
PRODUCT_DEF=-DEASYCAMERA
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS)

PROGNAME=easycamera-v1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_easycamera.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
