JAVAROOT=classes
AM_JAVACFLAGS=-target 1.6 -encoding UTF-8 -Xlint:deprecation -Xlint:unchecked -source 6

altoslibdir=$(libdir)/altos

CLASSPATH_ENV=mkdir -p $(JAVAROOT); CLASSPATH="$(JAVAROOT):../../altoslib/*"

bin_SCRIPTS=altos-map

altosmap_JAVA = \
	AltosMap.java

ALTOSLIB_CLASS=\
	altoslib_$(ALTOSLIB_VERSION).jar

JAR=altosmap.jar

FATJAR=altosmap-fat.jar

all-local: classes/altosmap $(JAR) altos-map altos-map-test altos-map-jdb

classes/altosmap:
	mkdir -p classes/altosmap

$(JAR): classaltosmap.stamp Manifest.txt $(ALTOSLIB_CLASS)
	jar cfm $@ Manifest.txt \
		-C classes altosmap

altosmapdir=$(datadir)/java

$(FATJAR): classaltosmap.stamp Manifest-fat.txt $(ALTOSLIB_CLASS)
	jar cfm $@ Manifest-fat.txt \
		-C classes altosmap

altos-map: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec java -Djava.library.path="$(altoslibdir)" -jar "$(altosmapdir)/altosmap.jar" "$$@"' >> $@
	chmod +x $@

altos-map-test: Makefile
	echo '#!/bin/sh' > $@
	echo 'dir="$$(dirname $$0)"' >> $@
	echo 'cd "$$dir"' >> $@
	echo 'altosmap="$$(pwd -P)"' >> $@
	echo 'exec java -jar "$$altosmap/altosmap.jar" "$$@"' >> $@
	chmod +x $@

altos-map-jdb: Makefile
	echo "#!/bin/sh" > $@
	echo 'exec jdb altosmap/AltosMap "$$@"' >> $@
	chmod +x $@

$(ALTOSLIB_CLASS):
	-rm -f "$@"
	$(LN_S) ../../altoslib/"$@" .

