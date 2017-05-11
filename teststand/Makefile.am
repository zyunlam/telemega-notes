JAVAROOT=classes
AM_JAVACFLAGS=-target 1.6 -encoding UTF-8 -Xlint:deprecation -Xlint:unchecked -source 6

man_MANS=teststand.1

altoslibdir=$(libdir)/altos

CLASSPATH_ENV=mkdir -p $(JAVAROOT); CLASSPATH=".:classes:../altoslib/*:../altosuilib/*:../libaltos:$(JCOMMON)/jcommon.jar:$(JFREECHART)/jfreechart.jar:$(FREETTS)/freetts.jar"

bin_SCRIPTS=teststand

teststanddir=$(datadir)/java

teststand_JAVA= \
	TestStand.java \
	TestStandConfig.java \
	TestStandConfigUI.java \
	TestStandDataPoint.java \
	TestStandDataSet.java \
	TestStandGraph.java \
	TestStandGraphUI.java \
	TestStandPreferences.java \
	TestStandState.java \
	TestStandStatus.java \
	TestStandStatusUpdate.java

JFREECHART_CLASS= \
    jfreechart.jar

JCOMMON_CLASS=\
    jcommon.jar

FREETTS_CLASS= \
	cmudict04.jar \
	cmulex.jar \
	cmu_time_awb.jar \
	cmutimelex.jar \
	cmu_us_kal.jar \
	en_us.jar \
	freetts.jar

JAR=teststand.jar

FATJAR=teststand-fat.jar

if MULTI_ARCH
LIBALTOS_LINUX=libaltos32.so libaltos64.so
else
LIBALTOS_LINUX=libaltos.so
endif

LIBALTOS= \
	$(LIBALTOS_LINUX) \
	libaltos.dylib \
	altos64.dll \
	altos.dll

ALTOSLIB_CLASS=\
	altoslib_$(ALTOSLIB_VERSION).jar

ALTOSUILIB_CLASS=\
	altosuilib_$(ALTOSUILIB_VERSION).jar

# Icons
ICONDIR=$(top_srcdir)/icon

JAVA_ICONS=\
	$(ICONDIR)/altusmetrum-teststand-16.png \
	$(ICONDIR)/altusmetrum-teststand-32.png \
	$(ICONDIR)/altusmetrum-teststand-48.png \
	$(ICONDIR)/altusmetrum-teststand-64.png \
	$(ICONDIR)/altusmetrum-teststand-128.png\
	$(ICONDIR)/altusmetrum-teststand-256.png

# icon base names for jar
ICONJAR= \
	-C $(ICONDIR) altusmetrum-teststand-16.png \
	-C $(ICONDIR) altusmetrum-teststand-32.png \
	-C $(ICONDIR) altusmetrum-teststand-48.png \
	-C $(ICONDIR) altusmetrum-teststand-64.png \
	-C $(ICONDIR) altusmetrum-teststand-128.png\
	-C $(ICONDIR) altusmetrum-teststand-256.png

WINDOWS_ICONS	=\
	../icon/altusmetrum-teststand.ico \
	../icon/altusmetrum-teststand.exe
	../icon/application-vnd.altusmetrum.eeprom.ico \
	../icon/application-vnd.altusmetrum.eeprom.exe 

MACOSX_ICONS	=\
	../icon/altusmetrum-teststand.icns \
	../icon/application-vnd.altusmetrum.eeprom.icns

LINUX_ICONS	=\
	$(ICONDIR)/altusmetrum-altosui.svg \
	$(ICONDIR)/application-vnd.altusmetrum.eeprom.svg 

LINUX_MIMETYPE	=\
	$(ICONDIR)/org-altusmetrum-mimetypes.xml

# Firmware
FIRMWARE_TD_3_0=$(top_srcdir)/src/teledongle-v3.0/teledongle-v3.0-$(VERSION).ihx
FIRMWARE_TD=$(FIRMWARE_TD_3_0)

FIRMWARE_TBT_3_0=$(top_srcdir)/src/telebt-v3.0/telebt-v3.0-$(VERSION).ihx
FIRMWARE_TBT=$(FIRMWARE_TBT_3_0)

FIRMWARE_TS_1_0=$(top_srcdir)/src/telefiretwo-v1.0/telefiretwo-v1.0-$(VERSION).ihx
FIRMWARE_TS=$(FIRMWARE_TS_1_0)

FIRMWARE=$(FIRMWARE_TS) $(FIRMWARE_TD) $(FIRMWARE_TBT)

desktopdir = $(datadir)/applications
desktop_file = altusmetrum-teststand.desktop
desktop_SCRIPTS = $(desktop_file)

all-local: teststand-test teststand-jdb $(JAR)

clean-local:
	-rm -rf classes $(JAR) $(FATJAR) \
		TestStand-Linux-*.tar.bz2 TestStand-Mac-*.dmg TestStand-Windows-*.exe \
		altoslib_*.jar altosuilib_*.jar \
		$(JFREECHART_CLASS) $(JCOMMON_CLASS) $(FREETTS_CLASS) $(LIBALTOS) Manifest.txt Manifest-fat.txt \
		teststand teststand-test teststand-jdb macosx linux windows teststand-windows.log \
		teststand-windows.nsi *.desktop

EXTRA_DIST = $(desktop_file).in

$(desktop_file): $(desktop_file).in
	sed -e 's#%bindir%#@bindir@#' -e 's#%icondir%#$(datadir)/icons/hicolor/scalable/apps#' ${srcdir}/$(desktop_file).in > $@
	chmod +x $@

LINUX_DIST=TestStand-Linux-$(VERSION).tar.bz2
LINUX_SH=TestStand-Linux-$(VERSION).sh
MACOSX_DIST=TestStand-Mac-$(VERSION).dmg
WINDOWS_DIST=TestStand-Windows-$(VERSION_DASH).exe

TELEGPS_DOC=$(top_srcdir)/doc/teststand.pdf

DOC=$(TELEGPS_DOC)

FAT_FILES=$(FATJAR) $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) $(FREETTS_CLASS) $(JFREECHART_CLASS) $(JCOMMON_CLASS)

LINUX_FILES=$(FAT_FILES) $(LIBALTOS_LINUX) $(FIRMWARE) $(DOC) $(desktop_file).in $(LINUX_ICONS) $(LINUX_MIMETYPE)
LINUX_EXTRA=teststand-fat $(desktop_file).in

MACOSX_INFO_PLIST=Info.plist
MACOSX_README=ReadMe-Mac.rtf
MACOSX_FILES=$(FAT_FILES) libaltos.dylib $(MACOSX_INFO_PLIST) $(MACOSX_README) $(DOC) $(MACOSX_ICONS)
MACOSX_EXTRA=$(FIRMWARE)

WINDOWS_FILES=$(FAT_FILES) altos.dll altos64.dll $(top_srcdir)/altusmetrum.inf $(top_srcdir)/altusmetrum.cat $(DOC) $(WINDOWS_ICONS)

if FATINSTALL

FATTARGET=$(FATDIR)/$(VERSION)

LINUX_DIST_TARGET=$(FATTARGET)/$(LINUX_DIST)
LINUX_SH_TARGET=$(FATTARGET)/$(LINUX_SH)
MACOSX_DIST_TARGET=$(FATTARGET)/$(MACOSX_DIST)
WINDOWS_DIST_TARGET=$(FATTARGET)/$(WINDOWS_DIST)

fat: $(LINUX_DIST_TARGET) $(LINUX_SH_TARGET) $(MACOSX_DIST_TARGET) $(WINDOWS_DIST_TARGET)

$(LINUX_DIST_TARGET): $(LINUX_DIST)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(LINUX_SH_TARGET): $(LINUX_SH)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(MACOSX_DIST_TARGET): $(MACOSX_DIST)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(WINDOWS_DIST_TARGET): $(WINDOWS_DIST)
	mkdir -p $(FATTARGET)
	cp -p $< $@

else
fat: $(LINUX_DIST) $(LINUX_SH) $(MACOSX_DIST) $(WINDOWS_DIST)
endif

teststand: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec java  -Djava.library.path="$(altoslibdir)" -jar "$(teststanddir)/teststand.jar" "$$@"' >> $@
	chmod +x $@

teststand-jdb: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec jdb -classpath "classes:./*:../libaltos:$(JCOMMON)/jcommon.jar:$(JFREECHART)/jfreechart.jar" -Djava.library.path="../libaltos/.libs" org.altusmetrum.teststand.TestStand "$$@"' >> $@
	chmod +x $@

teststand-test: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec java -Djava.library.path="../libaltos/.libs" -jar teststand.jar "$$@"' >> $@
	chmod +x $@

install-teststandJAVA: teststand.jar
	@$(NORMAL_INSTALL)
	test -z "$(teststanddir)" || $(MKDIR_P) "$(DESTDIR)$(teststanddir)"
	echo " $(INSTALL_DATA)" "$<" "'$(DESTDIR)$(teststanddir)/teststand.jar'"; \
	$(INSTALL_DATA) "$<" "$(DESTDIR)$(teststanddir)"

$(JAR): classteststand.stamp Manifest.txt $(JAVA_ICONS) $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS)
	jar cfm $@ Manifest.txt \
		$(ICONJAR) \
		-C classes org \
		-C ../libaltos libaltosJNI

$(FATJAR): classteststand.stamp Manifest-fat.txt $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) $(JFREECHART_CLASS) $(JCOMMON_CLASS) $(JAVA_ICONS)
	jar cfm $@ Manifest-fat.txt \
		$(ICONJAR) \
		-C classes org \
		-C ../libaltos libaltosJNI

libaltos.so: build-libaltos
	-rm -f "$@"
	$(LN_S) ../libaltos/.libs/"$@" .

libaltos32.so: build-libaltos
	-rm -f "$@"
	$(LN_S) ../libaltos/.libs/"$@" .

libaltos64.so: build-libaltos
	-rm -f "$@"
	$(LN_S) ../libaltos/.libs/"$@" .

libaltos.dylib:
	-rm -f "$@"
	$(LN_S) ../libaltos/"$@" .

altos.dll: ../libaltos/altos.dll
	-rm -f "$@"
	$(LN_S) ../libaltos/"$@" .

altos64.dll: ../libaltos/altos64.dll
	-rm -f "$@"
	$(LN_S) ../libaltos/"$@" .

../libaltos/.libs/libaltos64.so: ../libaltos/.libs/libaltos32.so

../libaltos/.libs/libaltos32.so: build-libaltos

../libaltos/.libs/libaltos.so: build-libaltos

../libaltos/altos.dll: build-altos-dll

../libaltos/altos64.dll: build-altos64-dll

build-libaltos:
	+cd ../libaltos && make libaltos.la
build-altos-dll:
	+cd ../libaltos && make altos.dll

build-altos64-dll:
	+cd ../libaltos && make altos64.dll

$(ALTOSLIB_CLASS):
	-rm -f "$@"
	$(LN_S) ../altoslib/"$@" .

$(ALTOSUILIB_CLASS):
	-rm -f "$@"
	$(LN_S) ../altosuilib/"$@" .

$(FREETTS_CLASS):
	-rm -f "$@"
	$(LN_S) "$(FREETTS)"/"$@" .

$(JFREECHART_CLASS):
	-rm -f "$@"
	$(LN_S) "$(JFREECHART)"/"$@" .

$(JCOMMON_CLASS):
	-rm -f "$@"
	$(LN_S) "$(JCOMMON)"/"$@" .

$(LINUX_DIST): $(LINUX_FILES) $(LINUX_EXTRA)
	-rm -f $@
	-rm -rf linux
	mkdir -p linux/TestStand
	cp -p $(LINUX_FILES) linux/TestStand
	cp -p teststand-fat linux/TestStand/teststand
	chmod +x linux/TestStand/teststand
	tar cjf $@ -C linux TestStand

$(LINUX_SH): $(LINUX_DIST) $(srcdir)/../altosui/linux-install.sh
	sed 's/AltOS/TestStand/g' $(srcdir)/../altosui/linux-install.sh | cat - $(LINUX_DIST) > $@
	chmod +x $@

$(MACOSX_DIST): $(MACOSX_FILES) $(MACOSX_EXTRA) Makefile
	-rm -f $@
	-rm -rf macosx
	mkdir macosx
	cp -a TestStand.app macosx/
	cp -a $(MACOSX_README) macosx/ReadMe.rtf
	mkdir -p macosx/Doc
	cp -a $(DOC) macosx/Doc
	cp -p Info.plist macosx/TestStand.app/Contents
	mkdir -p macosx/AltOS-$(VERSION) macosx/TestStand.app/Contents/Resources/Java
	cp -p $(MACOSX_ICONS) macosx/TestStand.app/Contents/Resources
	cp -p $(FATJAR) macosx/TestStand.app/Contents/Resources/Java/teststand.jar
	cp -p libaltos.dylib macosx/TestStand.app/Contents/Resources/Java
	cp -p $(ALTOSLIB_CLASS) macosx/TestStand.app/Contents/Resources/Java
	cp -p $(ALTOSUILIB_CLASS) macosx/TestStand.app/Contents/Resources/Java
	cp -p $(FREETTS_CLASS) macosx/TestStand.app/Contents/Resources/Java
	cp -p $(JFREECHART_CLASS) macosx/TestStand.app/Contents/Resources/Java
	cp -p $(JCOMMON_CLASS) macosx/TestStand.app/Contents/Resources/Java
	cp -p $(MACOSX_EXTRA) macosx/AltOS-$(VERSION)
	genisoimage -D -V TestStand-$(VERSION) -no-pad -r -apple -o $@ macosx

$(WINDOWS_DIST): $(WINDOWS_FILES) teststand-windows.nsi
	-rm -f $@
	makensis -Oteststand-windows.log "-XOutFile $@" "-DVERSION=$(VERSION)" teststand-windows.nsi || (cat teststand-windows.log && exit 1)

Manifest.txt: Makefile
	echo 'Main-Class: org.altusmetrum.teststand.TestStand' > $@
	echo "Class-Path: $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) $(FREETTS)/freetts.jar $(JCOMMON)/jcommon.jar $(JFREECHART)/jfreechart.jar" >> $@

Manifest-fat.txt:
	echo 'Main-Class: org.altusmetrum.teststand.TestStand' > $@
	echo "Class-Path: $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) freetts.jar jcommon.jar jfreechart.jar" >> $@

