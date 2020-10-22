AM_CFLAGS=-DLINUX -DPOSIX_TTY -I$(JVM_INCLUDE) -I$(JVM_INCLUDE)/linux
AM_JAVACFLAGS=$(JAVAC_VERSION_FLAGS) -encoding UTF-8 -Xlint:deprecation -Xlint:unchecked

altoslibdir=$(libdir)/altos

altoslib_LTLIBRARIES=libaltos.la
libaltos_la_LDFLAGS=-version-info 1:0:1 -Wl,-znoexecstack

libaltos_la_SOURCES=\
	libaltos_common.c \
	libaltos_posix.c \
	libaltos_linux.c \
	libaltos_wrap.c \
	libaltos.h \
	libaltos_posix.h \
	libaltos_private.h

WINDOWS_SRC=\
	libaltos_common.c\
	libaltos_windows.c\
	libaltos_wrap.c

WINDOWS_H=\
	libaltos.h

noinst_PROGRAMS=cjnitest btletest

cjnitest_SOURCES=cjnitest.c
cjnitest_LDADD=libaltos.la

btletest_SOURCES=btletest.c

btletest_LDADD=-lbluetooth

if MULTI_ARCH
altoslib_LTLIBRARIES+=libaltos32.la libaltos64.la

libaltos32_la_LDFLAGS=$(libaltos_la_LDFLAGS)
libaltos64_la_LDFLAGS=$(libaltos_la_LDFLAGS)

libaltos32_la_CFLAGS=-m32 $(AM_CFLAGS)
libaltos64_la_CFLAGS=-m64 $(AM_CFLAGS)

libaltos32_la_SOURCES=$(libaltos_la_SOURCES)
libaltos64_la_SOURCES=$(libaltos_la_SOURCES)

noinst_PROGRAMS+=cjnitest32 cjnitest64

cjnitest32_CFLAGS=-m32
cjnitest64_CFLAGS=-m64

cjnitest32_SOURCES=$(cjnitest_SOURCES)
cjnitest64_SOURCES=$(cjnitest_SOURCES)

cjnitest32_LDADD=libaltos32.la
cjnitest64_LDADD=libaltos64.la

endif

LIBS=-ldl

HFILES=libaltos.h

SWIG_FILE=libaltos.swig

CLASSDIR=libaltosJNI

$(SWIG_FILE): libaltos.i0 $(HFILES)
	sed 's;//%;%;' libaltos.i0 $(HFILES) > $(SWIG_FILE)

all-local: classlibaltos.stamp

libaltos_wrap.c: classlibaltos.stamp

classlibaltos.stamp: $(SWIG_FILE)
	swig -java -package libaltosJNI $(SWIG_FILE)
	mkdir -p libaltosJNI
	$(JAVAC) -d . $(AM_JAVACFLAGS) $(JAVACFLAGS) *.java && \
	touch classlibaltos.stamp

MINGCC32=i686-w64-mingw32-gcc
MINGCC64=x86_64-w64-mingw32-gcc
MINGFLAGS=-Wall -Wextra -DWINDOWS -DBUILD_DLL -mconsole -I$(JVM_INCLUDE) -I$(JVM_INCLUDE)/linux
MINGLIBS=-lsetupapi -lws2_32

fat: all altos.dll altos64.dll

#altos.dll: $(WINDOWS_SRC) $(WINDOWS_H)
#	$(MINGCC32) -o $@ $(MINGFLAGS) -shared $(WINDOWS_SRC) $(MINGLIBS)

#altos64.dll: $(WINDOWS_SRC) $(WINDOWS_H)
#	$(MINGCC64) -o $@ $(MINGFLAGS) -shared $(WINDOWS_SRC) $(MINGLIBS)

clean-local:
	-rm -rf libaltosJNI *.class *.java classlibaltos.stamp $(SWIG_FILE) libaltos_wrap.c
#	-rm -rf  altos.dll altos64.dll	
