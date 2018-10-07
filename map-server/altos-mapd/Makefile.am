JAVAROOT=classes
AM_JAVACFLAGS=-target 1.6 -encoding UTF-8 -Xlint:deprecation -Xlint:unchecked -source 6

altoslibdir=$(libdir)/altos

CLASSPATH_ENV=mkdir -p $(JAVAROOT); CLASSPATH="$(JAVAROOT):../../altoslib/*"

bin_SCRIPTS=altos-mapd

altosmapd_JAVA = \
	AltosMapd.java \
	AltosMapdServer.java \
	AltosMapdClient.java \
	AltosMapdPreferences.java

ALTOSLIB_CLASS=\
	altoslib_$(ALTOSLIB_VERSION).jar

JAR=altosmapd.jar

FATJAR=altosmapd-fat.jar

all-local: classes/altosmapd $(JAR) altos-mapd altos-mapd-test altos-mapd-jdb

install-altosmapdJAVA: altosmapd.jar
	@$(NORMAL_INSTALL)
	test -z "$(altosmapddir)" || $(MKDIR_P) "$(DESTDIR)$(altosmapddir)"
	echo " $(INSTALL_DATA)" "$<" "'$(DESTDIR)$(altosmapddir)/altosdmap.jar'"; \
	$(INSTALL_DATA) "$<" "$(DESTDIR)$(altosmapddir)"

classes/altosmapd:
	mkdir -p classes/altosmapd

$(JAR): classaltosmapd.stamp Manifest.txt $(ALTOSLIB_CLASS)
	jar cfm $@ Manifest.txt \
		-C classes altosmapd

altosmapddir=$(datadir)/java

$(FATJAR): classaltosmapd.stamp Manifest-fat.txt $(ALTOSLIB_CLASS)
	jar cfm $@ Manifest-fat.txt \
		-C classes altosmapd

altos-mapd: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec java -Djava.library.path="$(altoslibdir)" -jar "$(altosmapddir)/altosmapd.jar" "$$@"' >> $@
	chmod +x $@

altos-mapd-test: Makefile
	echo '#!/bin/sh' > $@
	echo 'dir="$$(dirname $$0)"' >> $@
	echo 'cd "$$dir"' >> $@
	echo 'altosmapd="$$(pwd -P)"' >> $@
	echo 'exec java -jar "$$altosmapd/altosmapd.jar" "$$@"' >> $@
	chmod +x $@

altos-mapd-jdb: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec jdb altosmapd/AltosMapd "$$@"' >> $@
	chmod +x $@

$(ALTOSLIB_CLASS):
	-rm -f "$@"
	$(LN_S) ../../altoslib/"$@" .

