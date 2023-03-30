#
# AltOS build
#
#

include ../stm32f1/Makefile.defs

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
	ao_ms5607.h \
	ao_bmx160.h \
	ao_adxl375.h \
	ao_task.h \
	ao_whiten.h \
	ao_sample_profile.h \
	ao_quaternion.h \
	stm32f1.h \
	Makefile

#
# Common AltOS sources
#

ALTOS_SRC = \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_clock.c \
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
	ao_ignite.c \
	ao_freq.c \
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_i2c_stm.c \
	ao_data.c \
	ao_ms5607.c \
	ao_bmi088.c \
	ao_mmc5983.c \
	ao_adxl375.c \
	ao_adc_stm.c \
	ao_beep_stm.c \
	ao_storage.c \
	ao_m25.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_report.c \
	ao_convert_pa.c \
	ao_convert_volt.c \
	ao_log.c \
	ao_log_mega.c \
	ao_sample.c \
	ao_kalman.c \
	ao_flight.c \
	ao_companion.c \
	ao_pyro.c \
	$(PROFILE) \
	$(SAMPLE_PROFILE) \
	$(STACK_GUARD)

PRODUCT=EasyMega-v3.0
PRODUCT_DEF=-DEASYMEGA
IDPRODUCT=0x0028

CFLAGS = $(PRODUCT_DEF) $(STM32F1_CFLAGS)

PROGNAME=easymega-v3.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_easymega.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIBS)

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h

install:

uninstall:
