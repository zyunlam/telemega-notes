MAC_RES=16 32 128 256 512
WIN_RES=16 24 32 48 64 72 96 128 180 256
RES=$(shell echo $(MAC_RES) $(WIN_RES) | awk '{ for (i = 1; i <= NF; i++) printf("%s\n", $$i); }' | sort -n -u)

AM_FILES=$(shell for i in $(RES); do echo altus-metrum-$$i.png; done)
MP_FILES=$(shell for i in $(RES); do echo micropeak-$$i.png; done)
TG_FILES=$(shell for i in $(RES); do echo telegps-$$i.png; done)

MAC_TG_FILES=$(shell for i in $(MAC_RES); do echo telegps-$$i.png; done)
MAC_MP_FILES=$(shell for i in $(MAC_RES); do echo micropeak-$$i.png; done)
MAC_AM_FILES=$(shell for i in $(MAC_RES); do echo altus-metrum-$$i.png; done)

WIN_TG_FILES=$(shell for i in $(WIN_RES); do echo telegps-$$i.png; done)
WIN_MP_FILES=$(shell for i in $(WIN_RES); do echo micropeak-$$i.png; done)
WIN_AM_FILES=$(shell for i in $(WIN_RES); do echo altus-metrum-$$i.png; done)

ICO_FILES=altus-metrum.ico micro-peak.ico telegps.ico
ICNS_FILES=AltosUIIcon.icns TeleGPS.icns MicroPeak.icns

icondir = $(datadir)/icons/hicolor/scalable/apps

AM_ICON = altusmetrum.svg
MP_ICON = micropeak.svg
TG_ICON = telegps.svg

icon_DATA = $(AM_ICON) $(MP_ICON) $(TG_ICON)

EXTRA_DIST = $(icon_DATA) $(AM_FILES) $(MP_FILES) $(TG_FILES)

res:
	echo $(RES)

all-local: $(ICO_FILES) $(ICNS_FILES)

clean-local:
	$(RM) altus-metrum-*.png telegps-*.png micropeak-*.png *.build *.ico *.icns

$(AM_FILES): altusmetrum.build

altusmetrum.build: altusmetrum.svg
	for i in $(RES); do ./make-png altusmetrum.svg $$i altus-metrum-$$i.png; done && touch $@

$(TG_FILES): telegps.build

telegps.build: telegps.svg
	for i in $(RES); do ./make-png telegps.svg $$i telegps-$$i.png; done && touch $@

$(MP_FILES): micropeak.build

micropeak.build: micropeak.svg
	for i in $(RES); do ./make-png micropeak.svg $$i micropeak-$$i.png; done && touch $@

#clean-local:
#	$(RM) -f $(ICO_FILES)

altus-metrum.ico: $(WIN_AM_FILES)
	icotool -c -o $@ $(WIN_AM_FILES)

micro-peak.ico: $(WIN_MP_FILES)
	icotool -c -o $@ $(WIN_MP_FILES)

telegps.ico: $(WIN_TG_FILES)
	icotool -c -o $@ $(WIN_TG_FILES)

AltosUIIcon.icns: $(MAC_AM_FILES)
	png2icns $@ $(MAC_AM_FILES)

TeleGPS.icns: $(MAC_TG_FILES)
	png2icns $@ $(MAC_TG_FILES)

MicroPeak.icns: $(MAC_MP_FILES)
	png2icns $@ $(MAC_MP_FILES)
