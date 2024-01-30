#
# AltOS build
#
#

include ../samd21/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_companion.h \
	ao_data.h \
	ao_sample.h \
	ao_pins.h \
	altitude-pa.h \
	ao_kalman.h \
	ao_product.h \
	ao_task.h \
	ao_whiten.h \
	samd21.h \
	Makefile

SAMD21_ROM=128
SAMD21_RAM=16

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_task.c \
	ao_stdio.c \
	ao_storage.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_freq.c \
	ao_dma_samd21.c \
	ao_spi_samd21.c \
	ao_data.c \
	ao_bmi088.c \
	ao_mmc5983.c \
	ao_m25.c \
	ao_adc_samd21.c \
	ao_beep_samd21.c \
	ao_usb_samd21.c \
	ao_exti_samd21.c \
	ao_convert_volt.c \
	ao_report.c \
	ao_sample.c \
	ao_kalman.c \
	ao_pyro.c \
	ao_flight.c \
	ao_ignite.c \
	ao_log.c \
	ao_log_timer.c \
	$(PROFILE) \
	$(SAMPLE_PROFILE) \
	$(STACK_GUARD)

PRODUCT=EasyTimer-v2
PRODUCT_DEF=-DEASYTIMER_V_2
IDPRODUCT=0x000d

CFLAGS = $(PRODUCT_DEF) $(SAMD21_CFLAGS) $(PROFILE_DEF) $(SAMPLE_PROFILE_DEF) $(STACK_GUARD_DEF)

PROGNAME=easytimer-v2
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_easytimer.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
