#
# AltOS build
#
#

include ../lpc/Makefile.defs

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_pins.h \
	ao_product.h \
	ao_adxl375.h \
	lpc.h

#
# Common AltOS sources
#

ALTOS_SRC = \
	ao_interrupt.c \
	ao_boot_chain.c \
	ao_romconfig.c \
	ao_product.c \
	ao_mutex.c \
	ao_panic.c \
	ao_stdio.c \
	ao_storage.c \
	ao_report.c \
	ao_ignite.c \
	ao_flight.c \
	ao_kalman.c \
	ao_sample.c \
	ao_data.c \
	ao_convert_volt.c \
	ao_task.c \
	ao_log.c \
	ao_log_motor.c \
	ao_cmd.c \
	ao_config.c \
	ao_dma_lpc.c \
	ao_timer_lpc.c \
	ao_exti_lpc.c \
	ao_spi_lpc.c \
	ao_adc_lpc.c \
	ao_usb_lpc.c \
	ao_m25.c \
	ao_adxl375.c \
	ao_beep_lpc.c

PRODUCT=EasyMotor-v3
PRODUCT_DEF=-DEASYMOTOR_V_3
IDPRODUCT=0x002c

CFLAGS = $(PRODUCT_DEF) $(LPC_CFLAGS)

PROGNAME=easymotor-v2
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_easymotor.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h

install:

uninstall:
