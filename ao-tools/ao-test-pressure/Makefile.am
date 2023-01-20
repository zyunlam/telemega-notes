bin_PROGRAMS=ao-test-pressure

AM_CFLAGS=$(WARN_CFLAGS) -I$(top_srcdir)/ao-tools/lib $(LIBUSB_CFLAGS)

ao_test_pressure_DEPENDENCIES = $(top_builddir)/ao-tools/lib/libao-tools.a

ao_test_pressure_LDADD=$(top_builddir)/ao-tools/lib/libao-tools.a $(LIBUSB_LIBS)

ao_test_pressure_SOURCES=ao-test-pressure.c

man_MANS = ao-test-pressure.1
