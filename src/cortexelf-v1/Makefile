#
# AltOS build
#
#

include ../stm/Makefile.defs
LDFLAGS=-L../stm -Wl,-Tcortexelf.ld

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
	math.h \
	ao_vga.h \
	ao_draw.h \
	ao_draw_int.h \
	ao_font.h \
	ao_ps2.h \
	ao_lisp.h \
	ao_lisp_const.h \
	ao_lisp_os.h \
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
	ao_lisp_lex.c \
	ao_lisp_mem.c \
	ao_lisp_cons.c \
	ao_lisp_eval.c \
	ao_lisp_string.c \
	ao_lisp_atom.c \
	ao_lisp_int.c \
	ao_lisp_poly.c \
	ao_lisp_builtin.c \
	ao_lisp_read.c \
	ao_lisp_rep.c \
	ao_lisp_frame.c \
	ao_lisp_error.c \
	ao_lisp_lambda.c \
	ao_lisp_save.c \
	ao_lisp_stack.c \
	ao_lisp_os_save.c \
	$(PROFILE) \
	$(SAMPLE_PROFILE) \
	$(STACK_GUARD)

PRODUCT=CortexELF-v1
PRODUCT_DEF=-DCORTEXELF
IDPRODUCT=0x000a

CFLAGS = $(PRODUCT_DEF) $(STM_CFLAGS) $(PROFILE_DEF) $(SAMPLE_PROFILE_DEF) $(STACK_GUARD_DEF) -Os -g

PROGNAME=cortexelf-v1
PROG=$(PROGNAME)-$(VERSION).elf
HEX=$(PROGNAME)-$(VERSION).ihx

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
	rm -f ao_product.h

include ../lisp/Makefile-lisp

install:

uninstall:
