bin_PROGRAMS=ao-sendfake

AM_CFLAGS=-I$(top_srcdir)/ao-tools/lib $(LIBUSB_CFLAGS)
AO_STMLOAD_LIBS=$(top_builddir)/ao-tools/lib/libao-tools.a

ao_sendfake_DEPENDENCIES = $(AO_STMLOAD_LIBS)

ao_sendfake_LDADD=$(AO_STMLOAD_LIBS) $(LIBUSB_LIBS)

ao_sendfake_SOURCES=ao-sendfake.c ao-sendfake.h

man_MANS = ao-sendfake.1
