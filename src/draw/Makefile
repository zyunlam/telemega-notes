TOPDIR=..
include $(TOPDIR)/Makefile.defs

vpath %.bdf fonts

BITMAP_SIZES = 10, 12, 14, 16, 18, 24, 26, 28, 32, 36, 48, 64, 72

BDFS=\
	BenguiatGothicStd-Bold-10.bdf \
	BenguiatGothicStd-Bold-12.bdf \
	BenguiatGothicStd-Bold-14.bdf \
	BenguiatGothicStd-Bold-16.bdf \
	BenguiatGothicStd-Bold-18.bdf \
	BenguiatGothicStd-Bold-24.bdf \
	BenguiatGothicStd-Bold-26.bdf \
	BenguiatGothicStd-Bold-28.bdf \
	BenguiatGothicStd-Bold-32.bdf \
	BenguiatGothicStd-Bold-36.bdf \
	BenguiatGothicStd-Bold-48.bdf \
	BenguiatGothicStd-Bold-64.bdf \
	BenguiatGothicStd-Bold-72.bdf \
	BenguiatGothicStd-Medium-10.bdf \
	BenguiatGothicStd-Medium-12.bdf \
	BenguiatGothicStd-Medium-14.bdf \
	BenguiatGothicStd-Medium-16.bdf \
	BenguiatGothicStd-Medium-18.bdf \
	BenguiatGothicStd-Medium-24.bdf \
	BenguiatGothicStd-Medium-26.bdf \
	BenguiatGothicStd-Medium-28.bdf \
	BenguiatGothicStd-Medium-32.bdf \
	BenguiatGothicStd-Medium-36.bdf \
	BenguiatGothicStd-Medium-48.bdf \
	BenguiatGothicStd-Medium-64.bdf \
	BenguiatGothicStd-Medium-72.bdf \
	BitstreamVeraSansMono-Roman-10.bdf \
	BitstreamVeraSansMono-Roman-12.bdf \
	BitstreamVeraSansMono-Roman-14.bdf \
	BitstreamVeraSansMono-Roman-16.bdf \
	BitstreamVeraSansMono-Roman-18.bdf \
	BitstreamVeraSansMono-Roman-24.bdf \
	BitstreamVeraSansMono-Roman-26.bdf \
	BitstreamVeraSansMono-Roman-28.bdf \
	BitstreamVeraSansMono-Roman-32.bdf \
	BitstreamVeraSansMono-Roman-36.bdf \
	BitstreamVeraSansMono-Roman-48.bdf \
	BitstreamVeraSansMono-Roman-64.bdf \
	BitstreamVeraSansMono-Roman-72.bdf \
	FrutigerLT-Roman-10.bdf \
	FrutigerLT-Roman-12.bdf \
	FrutigerLT-Roman-14.bdf \
	FrutigerLT-Roman-16.bdf \
	FrutigerLT-Roman-18.bdf \
	FrutigerLT-Roman-24.bdf \
	FrutigerLT-Roman-26.bdf \
	FrutigerLT-Roman-28.bdf \
	FrutigerLT-Roman-32.bdf \
	FrutigerLT-Roman-36.bdf \
	FrutigerLT-Roman-48.bdf \
	FrutigerLT-Roman-64.bdf \
	FrutigerLT-Roman-72.bdf

FONT_SRCS=$(BDFS:.bdf=.c)

.SUFFIXES: .bdf .c

.bdf.c:
	nickle font-convert -o $@ $<

all: ao_font.h ao_logo.h draw-test

$(FONT_SRCS): font-convert

ao_font.h: $(FONT_SRCS)
	grep -h '^const struct ao_font' $(FONT_SRCS) | sed -e 's/^/extern /' -e 's/ =.*$$/;/' > $@

ao_logo.h: make-logo
	nickle make-logo ao_logo > $@

SRCS=\
	draw-test.c \
	ao_blt.c \
	ao_copy.c \
	ao_line.c \
	ao_pattern.c \
	ao_rect.c \
	ao_poly.c \
	ao_text.c \
	ao_logo.c \
	$(FONT_SRCS)

OBJS=$(SRCS:.c=.o)

LIBS=-lXrender -lX11 -lm

CFLAGS=-O2 -g $(WARN_FLAGS)

HEADERS=\
	ao_draw.h \
	ao_draw_int.h \
	ao_font.h \
	ao_logo.h

draw-test: $(OBJS)
	cc $(CFLAGS) -o $@ $(OBJS) $(LIBS)

$(OBJS): $(HEADERS)

clean:
	rm -f $(OBJS) ao_font.h ao_logo.h $(FONT_SRCS)
