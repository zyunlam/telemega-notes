bin_PROGRAMS=ao-fakeflight

AM_CFLAGS=-I$(top_srcdir)/ao-tools/lib $(LIBUSB_CFLAGS) -I$(top_srcdir)/src/drivers -O0 -g
AO_POSTFLIGHT_LIBS=$(top_builddir)/ao-tools/lib/libao-tools.a

ao_fakeflight_DEPENDENCIES = $(AO_POSTFLIGHT_LIBS)

ao_fakeflight_LDADD=$(AO_POSTFLIGHT_LIBS)

LIBS=-lm

ao_fakeflight_SOURCES = \
	ao-fakeflight.c \
	ao-fakeflight.h \
	ao-fake-convert.c \
	ao-fake-log.c \
	ao-physics.c \
	ao-rocket.c

clean-local:
	$(RM) $(BUILT_SOURCES)

ALTOS=$(top_srcdir)/src
DRIVERS=$(ALTOS)/drivers
KERNEL=$(ALTOS)/kernel
