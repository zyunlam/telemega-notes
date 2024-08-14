#
# AltOS build
#
#

TOPDIR=..

include $(TOPDIR)/stmf0/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_pins.h \
	ao_product.h \
	ao_tracker.h \
	ao_task.h \
	ao_cc1200.h \
	ao_fec.h \
	stm32f0.h \
	Makefile


ALTOS_SRC = \
	ao_adc_stm.c \
	ao_led_stmf0.c \
	ao_interrupt.c \
	ao_boot_chain.c \
	ao_product.c \
	ao_romconfig.c \
	ao_cmd.c \
	ao_config.c \
	ao_task.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_freq.c \
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_serial_stm.c \
	ao_gps_ublox.c \
	ao_gps_show.c \
	ao_cc1200.c \
	ao_aprs.c \
	ao_tracker.c \
	ao_telemetry.c \
	ao_storage.c \
	ao_m25.c \
	ao_log.c \
	ao_log_gps.c \
	ao_distance.c \
	ao_sqrt.c \
	ao_data.c \
	ao_convert_volt.c \
	$(SAMPLE_PROFILE)

PRODUCT=TeleGPS-v4.0
PRODUCT_DEF=-DTELEGPS
IDPRODUCT=0x0025

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS) $(PROFILE_DEF)

PROGNAME=telegps-v4.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_telegps.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS) -Wl,-Map=$(PROGNAME)-$(VERSION).map

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o ao_serial_stm.h $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
