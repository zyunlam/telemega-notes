include ../stm32f4/Makefile-raw.defs

ALTOS_SRC = \
	ao_interrupt.c \
	ao_panic.c \
	ao_timer.c

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

distclean:	clean

clean:
	rm -f *.o *.elf *.ihx

install:

uninstall:
