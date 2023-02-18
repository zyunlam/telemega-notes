include ../stm32f1/Makefile-raw.defs

INC = \
	ao.h \
	ao_pins.h \
	ao_arch.h \
	ao_arch_funcs.h \
	stm32f1.h \
	Makefile

ALTOS_SRC = \
	ao_clock.c \
	ao_timer.c \
	ao_interrupt.c \
	ao_led.c

CFLAGS = $(PRODUCT_DEF) $(STM32F1_CFLAGS)

PROGNAME=nucleo
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

SRC=$(ALTOS_SRC) hello.c
OBJ=$(SRC:.c=.o)

all: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ)
	$(call quiet,CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS) --oslib=semihost

$(OBJ): $(INC)

clean:
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx $(PROGNAME)-*.map
	rm -f ao_product.h

install:

uninstall:
