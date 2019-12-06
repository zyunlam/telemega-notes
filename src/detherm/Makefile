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
	ao_task.h \
	stm32f0.h

#
# Common AltOS sources
#
ALTOS_SRC = \
	ao_interrupt.c \
	ao_timer.c \
	ao_panic.c \
	ao_mutex.c \
	ao_dma_stm.c \
	ao_usb_stm.c \
	ao_spi_stm.c \
	ao_exti_stm.c \
	ao_stdio.c \
	ao_led_stmf0.c \
	ao_log.c \
	ao_log_mini.c \
	ao_sample.c \
	ao_data.c \
	ao_kalman.c \
	ao_flight.c \
	ao_report.c \
	ao_storage.c \
	ao_m25.c \
	ao_romconfig.c \
	ao_boot_chain.c \
	ao_ms5607.c \
	ao_convert_pa.c \
	ao_pwm.c \
	ao_servo.c \
	ao_task.c \
	ao_config.c \
	ao_cmd.c \
	ao_product.c

PRODUCT=Detherm-v1.0
PRODUCT_DEF=-DDETHERM_V_1_0
IDPRODUCT=0x0013

CFLAGS = $(PRODUCT_DEF) $(STMF0_CFLAGS)

PROGNAME=detherm-v1.0
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_detherm.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) altos.ld
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS)

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) -o $@

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h

install:

uninstall:
