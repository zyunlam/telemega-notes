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
	ao_companion.h \
	ao_data.h \
	ao_sample.h \
	ao_pins.h \
	altitude-pa.h \
	ao_kalman.h \
	ao_product.h \
	ao_profile.h \
	ao_task.h \
	ao_whiten.h \
	ao_sample_profile.h \
	ao_mpu.h \
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
	ao_task.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_freq.c \
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_data.c \
	ao_bmx160.c \
	ao_adc_stm.c \
	ao_beep_stm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_eeprom_stm.c \
	ao_convert_volt.c \
	ao_report.c \
	ao_sample.c \
	ao_kalman.c \
	ao_pyro.c \
	ao_timer_flight.c \
	$(PROFILE) \
	$(SAMPLE_PROFILE) \
	$(STACK_GUARD)

PRODUCT=EasyTimer-v1
PRODUCT_DEF=-DEASYTIMER_V_1
IDPRODUCT=0x000d

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) $(SAMPLE_PROFILE_DEF) $(STACK_GUARD_DEF)

PROGNAME=easytimer-v1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) ao_easytimer.c
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
