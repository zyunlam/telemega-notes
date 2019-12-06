#
# AltOS build
#
#

include ../stm/Makefile.defs
include ../scheme/Makefile-inc


INC = \
	ao.h \
	ao_arch.h \
	ao_arch_funcs.h \
	ao_boot.h \
	ao_pins.h \
	ao_kalman.h \
	ao_product.h \
	ao_profile.h \
	ao_task.h \
	math.h \
	ao_mpu.h \
	stm32l.h \
	ao_vga.h \
	ao_draw.h \
	ao_draw_int.h \
	ao_font.h \
	ao_ps2.h \
	$(SCHEME_HDRS) \
	ao_flip_bits.h \
	Makefile

#PROFILE=ao_profile.c
#PROFILE_DEF=-DAO_PROFILE=1

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
	ao_errno.c \
	ao_stdio.c \
	ao_panic.c \
	ao_timer.c \
	ao_mutex.c \
	ao_serial_stm.c \
	ao_dma_stm.c \
	ao_spi_stm.c \
	ao_usb_stm.c \
	ao_exti_stm.c \
	ao_i2c_stm.c \
	ao_as1107.c \
	ao_matrix.c \
	ao_vga.c \
	ao_blt.c \
	ao_copy.c \
	ao_rect.c \
	ao_text.c \
	ao_line.c \
	ao_ps2.c \
	ao_console.c \
	ao_sdcard.c \
	ao_bufio.c \
	ao_fat.c \
	ao_flash_stm.c \
	ao_button.c \
	ao_event.c \
	ao_1802.c \
	ao_hex.c \
	$(SCHEME_SRCS) \
	ao_scheme_os_save.c \
	$(PROFILE) \
	$(SAMPLE_PROFILE) \
	$(STACK_GUARD)

PRODUCT=CortexELF-v1
PRODUCT_DEF=-DCORTEXELF
IDPRODUCT=0x000a

PROGNAME=cortexelf-v1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

MAP=$(PROG).map

MAPFILE=-Wl,-M=$(MAP)

LDFLAGS=-L../stm -L/local/newlib-mini/arm-none-eabi/lib/thumb/v7-m/ -Wl,-Tcortexelf.ld $(MAPFILE) -nostartfiles
AO_CFLAGS=-I. -I../stm -I../kernel -I../drivers -I../draw -I../scheme -I.. -I/local/newlib-mini/arm-none-eabi/include
LIBS=-lc -lm -lgcc

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) $(SAMPLE_PROFILE_DEF) $(STACK_GUARD_DEF)


SRC=$(ALTOS_SRC) ao_cortexelf.c
OBJ=$(SRC:.c=.o)

all:: $(PROG) $(HEX)

$(PROG): Makefile $(OBJ) cortexelf.ld
	$(call quiet,CC) $(LDFLAGS) $(CFLAGS) -o $(PROG) $(OBJ) $(LIBS)

../altitude-pa.h: make-altitude-pa
	nickle $< > $@

$(OBJ): $(INC)

ao_product.h: ao-make-product.5c ../Version
	$(call quiet,NICKLE,$<) $< -m altusmetrum.org -i $(IDPRODUCT) -p $(PRODUCT) -v $(VERSION) > $@

distclean:	clean

clean::
	rm -f *.o $(PROGNAME)-*.elf $(PROGNAME)-*.ihx
	rm -f ao_product.h ao_flip_bits.h

ao_flip_bits.h: ao_flip_bits.5c
	nickle ao_flip_bits.5c > $@

include ../scheme/Makefile-scheme

install:

uninstall:
