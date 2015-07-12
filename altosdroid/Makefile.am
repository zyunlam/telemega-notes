if ANDROID
all_target=bin/AltosDroid-debug.apk
if ANDROID_RELEASE
all_target+=bin/AltosDroid-release.apk
endif
else
all_target=
endif

SDK=$(ANDROID_SDK)
SDK_TARGET=$(shell sed -ne 's/^target=//p' project.properties)

DX=$(SDK)/platform-tools/dx
ADB=$(SDK)/platform-tools/adb
AAPT=$(SDK)/platform-tools/aapt
APKBUILDER=$(SDK)/tools/apkbuilder
ZIPALIGN_A=$(SDK)/tools/zipalign
ZIPALIGN_B=$(SDK)/build-tools/*/zipalign

JAVA_SRC_DIR=src/org/altusmetrum/AltosDroid
EXT_LIBDIR=libs
DRAWABLE_DIR=res/drawable
LAYOUT_DIR=res/layout
MENU_DIR=res/menu
VALUES_DIR=res/values
XML_DIR=res/xml
ALTOSLIB_SRCDIR=../altoslib
ALTOSLIB_JAR=altoslib_$(ALTOSLIB_VERSION).jar

ALTOSLIB=$(EXT_LIBDIR)/$(ALTOSLIB_JAR)

SUPPORT_V4_SRCDIR=$(SDK)/extras/android/support/v4
SUPPORT_V4_JAR=android-support-v4.jar

SUPPORT_V4=$(EXT_LIBDIR)/$(SUPPORT_V4_JAR)

GOOGLE_PLAY_SERVICES_LIB_SRCDIR=$(SDK)/extras/google/google_play_services/libproject
GOOGLE_PLAY_SERVICES_LIB=google-play-services_lib

JAVA_SRC=$(JAVA_SRC_DIR)/*.java $(JAVA_SRC_DIR)/BuildInfo.java

DRAWABLES=\
    $(DRAWABLE_DIR)/redled.png \
    $(DRAWABLE_DIR)/greenled.png \
    $(DRAWABLE_DIR)/grayled.png

LAYOUTS=$(LAYOUT_DIR)/*.xml
MENUS=$(MENU_DIR)/*.xml
VALUES=$(VALUES_DIR)/*.xml
XMLS=$(XML_DIR)/*.xml

RES=$(LAYOUTS) $(MENUS) $(VALUES) $(XMLS)

SRC=$(JAVA_SRC) $(DRAWABLES) $(RES)

all: $(all_target)

.NOTPARALLEL:

$(ALTOSLIB): $(ALTOSLIB_SRCDIR)/$(ALTOSLIB_JAR)
	mkdir -p $(EXT_LIBDIR)
	cd $(EXT_LIBDIR) && ln -sf $(shell echo $(EXT_LIBDIR) | sed 's|[^/]\+|..|g')/$(ALTOSLIB_SRCDIR)/$(ALTOSLIB_JAR) .

$(SUPPORT_V4): $(SUPPORT_V4_SRCDIR)/$(SUPPORT_V4_JAR)
	mkdir -p $(EXT_LIBDIR)
	cd $(EXT_LIBDIR) && ln -sf $(SUPPORT_V4_SRCDIR)/$(SUPPORT_V4_JAR) .

$(GOOGLE_PLAY_SERVICES_LIB): $(GOOGLE_PLAY_SERVICES_LIB_SRCDIR)/$(GOOGLE_PLAY_SERVICES_LIB)
	cp -a $(GOOGLE_PLAY_SERVICES_LIB_SRCDIR)/$(GOOGLE_PLAY_SERVICES_LIB) .
	cd $(GOOGLE_PLAY_SERVICES_LIB) && $(SDK)/tools/android update project --target $(SDK_TARGET) --path .

$(JAVA_SRC_DIR)/BuildInfo.java: $(filter-out $(JAVA_SRC_DIR)/BuildInfo.java,$(shell echo $(JAVA_SRC)))
	./buildinfo.sh

$(DRAWABLE_DIR)/%.png: ../icon/%.png
	cd $(DRAWABLE_DIR) && ln -sf $(shell echo $(DRAWABLE_DIR) | sed 's|[^/]\+|..|g')/$< .

if ANDROID
install-release: bin/AltosDroid-release.apk
	$(ADB) install -r bin/AltosDroid-release.apk

install-debug: bin/AltosDroid-debug.apk
	$(ADB) install -r bin/AltosDroid-debug.apk

bin/AltosDroid-debug.apk: $(SRC) $(ALTOSLIB) $(SUPPORT_V4) $(GOOGLE_PLAY_SERVICES_LIB)
	ant debug

bin/AltosDroid-release-unsigned.apk: $(SRC) $(ALTOSLIB) $(SUPPORT_V4) $(GOOGLE_PLAY_SERVICES_LIB)
	ant release

bin/AltosDroid-release.apk: bin/AltosDroid-release-unsigned.apk
	jarsigner -sigalg SHA1withDSA -digestalg SHA1 \
	   -keystore ~/altusmetrumllc/google-play-release.keystore \
	   -storepass:file ~/altusmetrumllc/google-play-passphrase \
	   -signedjar bin/AltosDroid-release-signed.apk \
	   bin/AltosDroid-release-unsigned.apk AltosDroid
	if [ -f $(ZIPALIGN_A) ]; then \
		$(ZIPALIGN_A) -f 4 \
		   bin/AltosDroid-release-signed.apk \
		   bin/AltosDroid-release.apk; \
	else \
		$(ZIPALIGN_B) -f 4 \
		   bin/AltosDroid-release-signed.apk \
		   bin/AltosDroid-release.apk; \
	fi

release: bin/AltosDroid-release.apk

clean-local: $(GOOGLE_PLAY_SERVICES_LIB)
	ant clean
	rm -f $(JAVA_SRC_DIR)/BuildInfo.java
	rm -f $(DRAWABLES)
	rm -rf $(EXT_LIBDIR)
	rm -rf $(GOOGLE_PLAY_SERVICES_LIB)

else

clean-local:

endif

clean: clean-local
