include ../stm32f1/Makefile.defs

INC = \
	ao.h \
	ao_pins.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_exti.h \
	ao_product.h \
	stm32f1.h \
	Makefile

ALTOS_SRC = \
	ao_clock.c \
	ao_timer.c \
	ao_boot_chain.c \
	ao_interrupt.c \
	ao_product.c \
	ao_romconfig.c \
	ao_led.c \
	ao_task.c \
	ao_panic.c \
	ao_stdio.c \
	ao_serial_stm.c \
	ao_usb_stm.c \
	ao_cmd.c

PRODUCT=Nucleo-f103
PRODUCT_DEF=-DNUCLEO_F103
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(STM32F1_CFLAGS)

PROGNAME=nucleo
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) hello.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

$(OBJ): $(INC)

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
