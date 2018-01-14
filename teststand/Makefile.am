
JAVAROOT=classes
AM_JAVACFLAGS=-target 1.6 -encoding UTF-8 -Xlint:deprecation -Xlint:unchecked -source 6

man_MANS=teststand.1

altoslibdir=$(libdir)/altos

CLASSPATH_ENV=mkdir -p $(JAVAROOT); CLASSPATH="$(JAVAROOT):../altoslib/*:../altosuilib/*:../libaltos:$(JCOMMON)/jcommon.jar:$(JFREECHART)/jfreechart.jar:$(FREETTS)/freetts.jar"

bin_SCRIPTS=teststand

teststand_JAVA = \
	AltosAscent.java \
	AltosChannelMenu.java \
	AltosCompanionInfo.java \
	AltosConfigFC.java \
	AltosConfigFCUI.java \
	AltosConfigPyroUI.java \
	AltosConfigureUI.java \
	AltosConfigTD.java \
	AltosConfigTDUI.java \
	AltosDescent.java \
	AltosFlightStatus.java \
	AltosFlightStatusUpdate.java \
	AltosFlightUI.java \
	Altos.java \
	AltosIdleMonitorUI.java \
	AltosIgniteUI.java \
	AltosIgnitor.java \
	AltosLaunch.java \
	AltosLaunchUI.java \
	AltosLanded.java \
	AltosPad.java \
	TestStand.java \
	AltosGraphUI.java

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

ALTOSLIB_CLASS=\
	altoslib_$(ALTOSLIB_VERSION).jar

ALTOSUILIB_CLASS=\
	altosuilib_$(ALTOSUILIB_VERSION).jar

if MULTI_ARCH
LIBALTOS_LINUX=libaltos32.so libaltos64.so
else
LIBALTOS_LINUX=libaltos.so
endif

LIBALTOS= \
	$(LIBALTOS_LINUX) \
	libaltos.dylib \
	altos.dll \
	altos64.dll

desktopdir = $(datadir)/applications
desktop_file = altusmetrum-teststand.desktop
desktop_SCRIPTS = $(desktop_file)

JAR=teststand.jar

FATJAR=teststand-fat.jar

# Icons
ICONDIR=../icon

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
	$(ICONDIR)/altusmetrum-teststand.ico \
	$(ICONDIR)/altusmetrum-teststand.exe
	$(ICONDIR)/application-vnd.altusmetrum.eeprom.ico \
	$(ICONDIR)/application-vnd.altusmetrum.eeprom.exe \
	$(ICONDIR)/application-vnd.altusmetrum.telemetry.ico \
	$(ICONDIR)/application-vnd.altusmetrum.telemetry.exe

MACOSX_ICONS	=\
	$(ICONDIR)/altusmetrum-teststand.icns \
	$(ICONDIR)/application-vnd.altusmetrum.eeprom.icns \
	$(ICONDIR)/application-vnd.altusmetrum.telemetry.icns

LINUX_ICONS	=\
	$(ICONDIR)/altusmetrum-teststand.svg \
	$(ICONDIR)/application-vnd.altusmetrum.eeprom.svg \
	$(ICONDIR)/application-vnd.altusmetrum.telemetry.svg

LINUX_MIMETYPE	=\
	$(ICONDIR)/org-altusmetrum-mimetypes.xml

# Firmware
FIRMWARE_TD_0_2=$(top_srcdir)/src/teledongle-v0.2/teledongle-v0.2-$(VERSION).ihx
FIRMWARE_TD_3_0=$(top_srcdir)/src/teledongle-v3.0/teledongle-v3.0-$(VERSION).ihx
FIRMWARE_TD=$(FIRMWARE_TD_0_2) $(FIRMWARE_TD_3_0)

FIRMWARE_TM_1_0=$(top_srcdir)/src/telemetrum-v1.0/telemetrum-v1.0-$(VERSION).ihx
FIRMWARE_TM_1_1=$(top_srcdir)/src/telemetrum-v1.1/telemetrum-v1.1-$(VERSION).ihx
FIRMWARE_TM_1_2=$(top_srcdir)/src/telemetrum-v1.2/telemetrum-v1.2-$(VERSION).ihx
FIRMWARE_TM_2_0=$(top_srcdir)/src/telemetrum-v2.0/telemetrum-v2.0-$(VERSION).ihx
#FIRMWARE_TM_3_0=$(top_srcdir)/src/telemetrum-v3.0/telemetrum-v3.0-$(VERSION).ihx
#FIRMWARE_TM=$(FIRMWARE_TM_1_0) $(FIRMWARE_TM_1_1) $(FIRMWARE_TM_1_2) $(FIRMWARE_TM_2_0) $(FIRMWARE_TM_3_0)
FIRMWARE_TM=$(FIRMWARE_TM_1_0) $(FIRMWARE_TM_1_1) $(FIRMWARE_TM_1_2) $(FIRMWARE_TM_2_0)

FIRMWARE_TELEMINI_1_0=$(top_srcdir)/src/telemini-v1.0/telemini-v1.0-$(VERSION).ihx
FIRMWARE_TELEMINI_3_0=$(top_srcdir)/src/telemini-v3.0/telemini-v3.0-$(VERSION).ihx
FIRMWARE_TELEMINI=$(FIRMWARE_TELEMINI_1_0) $(FIRMWARE_TELEMINI_3_0)

FIRMWARE_TBT_1_0=$(top_srcdir)/src/telebt-v1.0/telebt-v1.0-$(VERSION).ihx
FIRMWARE_TBT_3_0=$(top_srcdir)/src/telebt-v3.0/telebt-v3.0-$(VERSION).ihx
FIRMWARE_TBT_4_0=$(top_srcdir)/src/telebt-v4.0/telebt-v4.0-$(VERSION).ihx
FIRMWARE_TBT=$(FIRMWARE_TBT_1_0) $(FIRMWARE_TBT_3_0) $(FIRMWARE_TBT_4_0)

FIRMWARE_TMEGA_1_0=$(top_srcdir)/src/telemega-v1.0/telemega-v1.0-$(VERSION).ihx
FIRMWARE_TMEGA_2_0=$(top_srcdir)/src/telemega-v2.0/telemega-v2.0-$(VERSION).ihx
FIRMWARE_TMEGA=$(FIRMWARE_TMEGA_1_0) $(FIRMWARE_TMEGA_2_0)

FIRMWARE_EMINI_1_0=$(top_srcdir)/src/easymini-v1.0/easymini-v1.0-$(VERSION).ihx
FIRMWARE_EMINI=$(FIRMWARE_EMINI_1_0)

FIRMWARE_EMEGA_1_0=$(top_srcdir)/src/easymega-v1.0/easymega-v1.0-$(VERSION).ihx
FIRMWARE_EMEGA=$(FIRMWARE_EMEGA_1_0)

FIRMWARE_TGPS_1_0=$(top_srcdir)/src/telegps-v1.0/telegps-v1.0-$(VERSION).ihx
FIRMWARE_TGPS=$(FIRMWARE_TGPS_1_0)

FIRMWARE=$(FIRMWARE_TM) $(FIRMWARE_TELEMINI) $(FIRMWARE_TD) $(FIRMWARE_TBT) $(FIRMWARE_TMEGA) $(FIRMWARE_EMINI) $(FIRMWARE_TGPS) $(FIRMWARE_EMEGA)

ALTUSMETRUM_DOC=$(top_srcdir)/doc/altusmetrum.pdf
ALTOS_DOC=$(top_srcdir)/doc/altos.pdf
TELEMETRY_DOC=$(top_srcdir)/doc/telemetry.pdf
TEMPLATE_DOC=\
	$(top_srcdir)/doc/telemetrum-outline.pdf \
	$(top_srcdir)/doc/easymini-outline.pdf \
	$(top_srcdir)/doc/telemega-outline.pdf \
	$(top_srcdir)/doc/telemini-v1-outline.pdf \
	$(top_srcdir)/doc/telemini-v3-outline.pdf

DOC=$(ALTUSMETRUM_DOC) $(ALTOS_DOC) $(TELEMETRY_DOC) $(TEMPLATE_DOC)

# Distribution targets
LINUX_DIST=Altos-Linux-$(VERSION).tar.bz2
LINUX_SH=Altos-Linux-$(VERSION).sh
MACOSX_DIST=Altos-Mac-$(VERSION).dmg
WINDOWS_DIST=Altos-Windows-$(VERSION_DASH).exe
MDWN=$(VERSION).mdwn
MDWNTMPL=mdwn.tmpl

FAT_FILES=$(FATJAR) $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) $(FREETTS_CLASS) $(JFREECHART_CLASS) $(JCOMMON_CLASS)

LINUX_LIBS=$(LIBALTOS_LINUX)

LINUX_FILES=$(FAT_FILES) $(LINUX_LIBS) $(FIRMWARE) $(DOC) $(desktop_file).in $(LINUX_ICONS) $(LINUX_MIMETYPE)
LINUX_EXTRA=teststand-fat

MACOSX_INFO_PLIST=Info.plist
MACOSX_FILES=$(FAT_FILES) libaltos.dylib $(MACOSX_INFO_PLIST) $(DOC) ReadMe-Mac.rtf $(MACOSX_ICONS)
MACOSX_EXTRA=$(FIRMWARE)

WINDOWS_FILES=$(FAT_FILES) $(FIRMWARE) altos.dll altos64.dll $(top_srcdir)/altusmetrum.inf $(top_srcdir)/altusmetrum.cat $(WINDOWS_ICONS)

all-local: classes/teststand $(JAR) teststand teststand-test teststand-jdb $(MDWN)

clean-local:
	-rm -rf classes $(JAR) $(FATJAR) \
		Altos-Linux-*.tar.bz2 Altos-Linux-*.sh Altos-Mac-*.dmg Altos-Windows-*.exe \
		windows altoslib_*.jar altosuilib_*.jar $(FREETTS_CLASS) \
		$(JFREECHART_CLASS) $(JCOMMON_CLASS) $(LIBALTOS) Manifest.txt Manifest-fat.txt altos-windows.log altos-windows.nsi \
		teststand teststand-test teststand-jdb macosx linux *.desktop *.mdwn

EXTRA_DIST = $(desktop_file).in

$(desktop_file): $(desktop_file).in
	sed -e 's#%bindir%#@bindir@#' -e 's#%icondir%#$(datadir)/icons/hicolor/scalable/apps#' ${srcdir}/$(desktop_file).in > $@
	chmod +x $@

if FATINSTALL

FATTARGET=$(FATDIR)/AltOS/releases/$(VERSION)

LINUX_SH_TARGET=$(FATTARGET)/$(LINUX_SH)
MACOSX_DIST_TARGET=$(FATTARGET)/$(MACOSX_DIST)
WINDOWS_DIST_TARGET=$(FATTARGET)/$(WINDOWS_DIST)
MDWN_TARGET=$(FATDIR)/AltOS/releases/$(MDWN)
RELNOTES=release-notes-$(VERSION).html
RELNOTES_SRC=$(top_builddir)/doc/$(RELNOTES)
RELNOTES_TARGET=$(FATTARGET)/$(RELNOTES)

fat-install: fat $(LINUX_SH_TARGET) $(MACOSX_DIST_TARGET) $(WINDOWS_DIST_TARGET) $(MDWN_TARGET) $(RELNOTES_TARGET)

$(LINUX_SH_TARGET): $(LINUX_SH)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(MACOSX_DIST_TARGET): $(MACOSX_DIST)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(WINDOWS_DIST_TARGET): $(WINDOWS_DIST)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(MDWN_TARGET): $(MDWN)
	mkdir -p $(FATTARGET)
	cp -p $< $@

$(RELNOTES_TARGET): $(RELNOTES_SRC)
	mkdir -p $(FATTARGET)
	sh $(top_srcdir)/doc/install-html -d $(FATTARGET) $(RELNOTES_SRC)

endif

$(MDWN): $(MDWNTMPL)
	sed -e 's/%version%/$(VERSION)/g' -e 's/%version_dash%/$(VERSION_DASH)/g' $(MDWNTMPL) > $@

fat: $(LINUX_DIST) $(LINUX_SH) $(MACOSX_DIST) $(WINDOWS_DIST)

teststanddir=$(datadir)/java

install-teststandJAVA: teststand.jar
	@$(NORMAL_INSTALL)
	test -z "$(teststanddir)" || $(MKDIR_P) "$(DESTDIR)$(teststanddir)"
	echo " $(INSTALL_DATA)" "$<" "'$(DESTDIR)$(teststanddir)/teststand.jar'"; \
	$(INSTALL_DATA) "$<" "$(DESTDIR)$(teststanddir)"

classes/teststand:
	mkdir -p classes/teststand

$(JAR): classteststand.stamp Manifest.txt $(JAVA_ICONS) $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS)
	jar cfm $@ Manifest.txt \
		$(ICONJAR) \
		-C classes teststand \
		-C ../libaltos libaltosJNI

$(FATJAR): classteststand.stamp Manifest-fat.txt $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) $(FREETTS_CLASS) $(JFREECHART_CLASS) $(JCOMMON_CLASS) $(LIBALTOS) $(JAVA_ICONS)
	jar cfm $@ Manifest-fat.txt \
		$(ICONJAR) \
		-C classes teststand \
		-C ../libaltos libaltosJNI

Manifest.txt: Makefile
	echo 'Main-Class: teststand.TestStand' > $@
	echo "Class-Path: $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) $(FREETTS)/freetts.jar $(JCOMMON)/jcommon.jar $(JFREECHART)/jfreechart.jar" >> $@

Manifest-fat.txt:
	echo 'Main-Class: teststand.TestStand' > $@
	echo "Class-Path: $(ALTOSLIB_CLASS) $(ALTOSUILIB_CLASS) freetts.jar jcommon.jar jfreechart.jar" >> $@

teststand: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec java -Djava.library.path="$(altoslibdir)" -jar "$(altosuidir)/teststand.jar" "$$@"' >> $@
	chmod +x $@

teststand-test: Makefile
	echo '#!/bin/sh' > $@
	echo 'dir="$$(dirname $$0)"' >> $@
	echo 'cd "$$dir"' >> $@
	echo 'teststand="$$(pwd -P)"' >> $@
	echo 'altos="$$(dirname $$teststand)"' >> $@
	echo 'exec java -Djava.library.path="$$altos/libaltos/.libs" -jar "$$teststand/teststand.jar" "$$@"' >> $@
	chmod +x $@

teststand-jdb: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec jdb -classpath "classes:./*:../libaltos:$(FREETTS)/freetts.jar:$(JCOMMON)/jcommon.jar:$(JFREECHART)/jfreechart.jar" -Djava.library.path="../libaltos/.libs" teststand/TestStand "$$@"' >> $@
	chmod +x $@

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
	mkdir -p linux/AltOS
	cp -p $(LINUX_FILES) linux/AltOS
	cp -p teststand-fat linux/AltOS/teststand
	chmod +x linux/AltOS/teststand
	tar cjf $@ -C linux AltOS

$(LINUX_SH): $(LINUX_DIST) linux-install.sh
	cat linux-install.sh $(LINUX_DIST) > $@
	chmod +x $@

$(MACOSX_DIST): $(MACOSX_FILES) $(MACOSX_EXTRA) Makefile
	-rm -f $@
	-rm -rf macosx
	mkdir macosx
	cp -a TestStand.app macosx/
	cp -a ReadMe-Mac.rtf macosx/ReadMe.rtf
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
	genisoimage -D -V AltOS-$(VERSION) -no-pad -r -apple -o $@ macosx

$(WINDOWS_DIST): $(WINDOWS_FILES) altos-windows.nsi Instdrv/NSIS/Includes/java.nsh
	-rm -f $@
	makensis -Oaltos-windows.log "-XOutFile $@" "-DVERSION=$(VERSION)" altos-windows.nsi || (cat altos-windows.log && exit 1)
