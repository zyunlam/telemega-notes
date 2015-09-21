SUBDIRS=ao-tools src doc icon altoslib libaltos altosuilib altosui micropeak ao-utils altosdroid telegps

EXTRA_DIST = ChangeLog

MAINTAINERCLEANFILES = ChangeLog

.PHONY: ChangeLog

ChangeLog:
	(GIT_DIR=$(top_srcdir)/.git git log > .changelog.tmp && mv .changelog.tmp ChangeLog; rm -f .changelog.tmp) || \
	(touch ChangeLog; echo 'git directory not found: installing possibly empty changelog.' >&2)

dist-hook: ChangeLog

fat:
	cd src && $(MAKE) all
	cd doc && $(MAKE) all
	cd libaltos && $(MAKE) fat
	cd altoslib && $(MAKE) all
	cd altosuilib && $(MAKE) all
	cd icon && $(MAKE) fat
	cd altosui && $(MAKE) fat
	cd micropeak && $(MAKE) fat
	cd telegps && $(MAKE) fat

fat_linux = \
	altosui/Altos-Linux-$(VERSION).sh altosui/Altos-Linux-$(VERSION).tar.bz2 \
	telegps/TeleGPS-Linux-$(VERSION).sh telegps/TeleGPS-Linux-$(VERSION).tar.bz2 \
	micropeak/MicroPeak-Linux-$(VERSION).sh micropeak/MicroPeak-Linux-$(VERSION).tar.bz2

fat_mac = \
	altosui/Altos-Mac-$(VERSION).dmg \
	telegps/TeleGPS-Mac-$(VERSION).dmg \
	micropeak/MicroPeak-Mac-$(VERSION).dmg

fat_windows = \
	altosui/Altos-Windows-$(VERSION_DASH).exe \
	telegps/TeleGPS-Windows-$(VERSION_DASH).exe \
	micropeak/MicroPeak-Windows-$(VERSION_DASH).exe

keithp-fat: fat
	ssh keithp.com mkdir -p public_html/altos-$(VERSION)
	scp -p $(fat_linux) $(fat_mac) $(fat_windows) keithp.com:public_html/altos-$(VERSION)

set-java-versions:
	$(top_srcdir)/fix-java-versions org.altusmetrum.altoslib=$(ALTOSLIB_VERSION) org.altusmetrum.altosuilib=$(ALTOSUILIB_VERSION)
