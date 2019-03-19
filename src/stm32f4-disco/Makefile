include ../stm32f4/Makefile-raw.defs

IDVENDOR=0xfffe
IDPRODUCT=0xfffa
PRODUCT=stm32f4-disco
SERIAL=1

INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_task.h \
	ao_product.h \
	stm32f4.h \
	Makefile

ALTOS_SRC = \
	ao_interrupt.c \
	ao_romconfig.c \
	ao_panic.c \
	ao_timer.c \
	ao_task.c \
	ao_stdio.c \
	ao_product.c \
	ao_cmd.c \
	ao_exti_stm32f4.c \
	ao_usart_stm32f4.c \
	ao_usb_gen.c \
	ao_usb_stm32f4.c \
	ao_led.c \
	ao_impure.c

CFLAGS = $(STM32F4_CFLAGS)

PROG=stm32f4-disco-$(VERSION)
ELF=$(PROG).elf
IHX=$(PROG).ihx

SRC=$(ALTOS_SRC) ao_disco.c
OBJ=$(SRC:.c=.o)
MAP=$(PROG).map

all: $(ELF) $(IHX)

$(ELF): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $@ $(OBJ) -Wl,-M=$(MAP) $(LIBS)

$(OBJ): $(INC)

distclean:	clean

clean:
	rm -f *.o *.elf *.ihx *.map

install:

uninstall:
