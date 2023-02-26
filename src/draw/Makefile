BDFS=\
	FrutigerLT-Roman-10.bdf \
	FrutigerLT-Roman-12.bdf \
	FrutigerLT-Roman-14.bdf \
	FrutigerLT-Roman-18.bdf \
	FrutigerLT-Roman-24.bdf \
	FrutigerLT-Roman-36.bdf \
	FrutigerLT-Roman-48.bdf \
	FrutigerLT-Roman-64.bdf \

FONT_SRCS=$(BDFS:.bdf=.c)

.SUFFIXES: .bdf .c

.bdf.c:
	nickle font-convert $*.bdf > $@

all: ao_font.h ao_logo.h draw-test

$(FONT_SRCS): font-convert

ao_font.h: $(FONT_SRCS)
	grep -h '^const struct ao_font' $(FONT_SRCS) | sed -e 's/^/extern /' -e 's/ =.*$$/;/' > $@

ao_logo.h: make-logo Makefile
	nickle make-logo ao_logo 48 0 10 > $@

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

CFLAGS=-O0 -g

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
