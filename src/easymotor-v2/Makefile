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
	ao_adxl375.h \
	stm32f0.h

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
	ao_convert_pa.c \
	ao_convert_volt.c \
	ao_task.c \
	ao_log.c \
	ao_log_motor.c \
	ao_cmd.c \
	ao_config.c \
	ao_dma_stm.c \
	ao_timer.c \
	ao_exti_stm.c \
	ao_spi_stm.c \
	ao_adc_stm.c \
	ao_usb_stm.c \
	ao_m25.c \
	ao_adxl375.c \
	ao_beep_stm.c

PRODUCT=EasyMotor-v2
PRODUCT_DEF=-DEASYMOTOR_V_2
IDPRODUCT=0x002c

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS)

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
