include ../stm32f1/Makefile.defs

INC = \
	ao.h \
	ao_pins.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_exti.h \
	ao_product.h \
	ao_st7565.h \
	stm32f1.h \
	Makefile

ALTOS_SRC = \
	ao_clock.c \
	ao_timer.c \
	ao_mutex.c \
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
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_st7565.c \
	ao_rect.c \
	ao_text.c \
	ao_box.c \
	ao_copy.c \
	ao_blt.c \
	ao_line.c \
	ao_logo.c \
	ao_poly.c \
	BitstreamVeraSans-Roman-58.c \
	BitstreamVeraSans-Roman-24.c \
	BitstreamVeraSans-Roman-10.c \
	BitstreamVeraSans-Roman-12.c \
	BenguiatGothicStd-Bold-26.c \
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
