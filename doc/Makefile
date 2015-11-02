#
#	http://docbook.sourceforge.net/release/xsl/current/README
#

RELNOTES_INC=\
	release-notes-0.7.1.inc \
	release-notes-0.8.inc \
	release-notes-0.9.inc \
	release-notes-0.9.2.inc \
	release-notes-1.0.1.inc \
	release-notes-1.1.inc \
	release-notes-1.1.1.inc \
	release-notes-1.2.inc \
	release-notes-1.2.1.inc \
	release-notes-1.3.inc \
	release-notes-1.3.1.inc \
	release-notes-1.3.2.inc \
	release-notes-1.4.inc \
	release-notes-1.4.1.inc \
	release-notes-1.4.2.inc \
	release-notes-1.5.inc \
	release-notes-1.6.inc \
	release-notes-1.6.1.inc

IMAGES=\
	altosui.png \
	ascent.png \
	configure-altimeter.png \
	configure-altosui.png \
	configure-groundstation.png \
	configure-pyro.png \
	descent.png \
	device-selection.png \
	easymega.svg \
	easymega-v1.0-bottom.jpg \
	easymega-v1.0-top.jpg \
	easymini.svg \
	easymini-top.jpg \
	fire-igniter.png \
	graph-configure.png \
	graph-map.png \
	graph.png \
	graph-stats.png \
	ignitor.png \
	landed.png \
	launch-pad.png \
	load-maps.png \
	micropeak-app.png \
	micropeak-back.jpg \
	micropeak-device-dialog.png \
	micropeak-dime.jpg \
	micropeak-download.png \
	micropeak-graph-configure.png \
	micropeak-graph.png \
	micropeak-nofont.svg \
	micropeak-preferences.png \
	micropeak-raw-data.png \
	micropeak-save-dialog.png \
	micropeak-statistics.png \
	MicroPeakUSB-2.0-inuse.jpg \
	MicroPeakUSB-2.0.jpg \
	monitor-idle.png \
	scan-channels.png \
	site-map.png \
	table.png \
	telegps-configure.png \
	telegps-graph-configure.png \
	telegps-graph-graph.png \
	telegps-graph-map.png \
	telegps-graph-stats.png \
	telegps-info.png \
	telegps-location.png \
	telegps-map.png \
	telegps-preferences.png \
	telegps-scan.png \
	telegps-status.png \
	telegps-table.png \
	telegps-v1.0-top.jpg \
	telemega.svg \
	telemega-v1.0-top.jpg \
	telemetrum.svg \
	telemetrum-v1.1-thside.jpg \
	telemetrum-v2.0-th.jpg \
	telemini.svg \
	telemini-v1-top.jpg \
	telemini-v2-top.jpg \
	altusmetrum-oneline.svg \
	telegps-oneline.svg \
	micropeak-oneline.svg

TXT_FILES=altusmetrum.txt

INC_FILES=\
	dedication.inc \
	intro.inc \
	getting-started.inc \
	usage.inc \
	telemetrum.inc \
	telemini-v1.0.inc \
	telemini-v2.0.inc \
	easymini.inc \
	telemega.inc \
	easymega.inc \
	installation.inc \
	using-am-products.inc \
	updating-firmware.inc \
	altosui.inc \
	altosdroid.inc \
	system-operation.inc \
	pyro-channels.inc \
	flight-data-recording.inc \
	handling.inc \
	specs.inc \
	release-notes.inc \
	$(RELNOTES_INC)

RAW_FILES=$(TXT_FILES:.txt=.raw) $(INC_FILES:.inc=.raw)

TELEGPS_INC_FILES=\
	dedication.inc \
	telegps-quick-start.inc \
	telegps-using.inc \
	telegps-system-operation.inc \
	telegps-application.inc \
	handling.inc \
	telegps-specs.inc \
	telegps-updating-firmware.inc \
	telegps-release-notes.inc

TELEGPS_TXT_FILES=\
	telegps.txt

TELEGPS_RAW_FILES=$(TELEGPS_TXT_FILES:.txt=.raw) $(TELEGPS_INC_FILES:.inc=.raw)

MICROPEAK_TXT_FILES=\
	micropeak.txt

MICROPEAK_INC_FILES=

MICROPEAK_RAW_FILES=$(MICROPEAK_TXT_FILES:.txt=.raw) $(MICROPEAK_INC_FILES:.inc=.raw)

OUTLINE_TXT_FILES=\
	easymega-outline.txt \
	easymini-outline.txt \
	telemega-outline.txt \
	telemetrum-outline.txt \
	telemini-outline.txt

OUTLINE_RAW_FILES=$(OUTLINE_TXT_FILES:.txt=.raw)

OUTLINE_PDF_FILES=$(OUTLINE_TXT_FILES:.txt=.pdf)

SVG=\
	easymini.svg \
	telemega.svg \
	telemetrum.svg \
	telemini.svg \
	easymega.svg

RELNOTES_PDF=$(RELNOTES_INC:.inc=.pdf)
RELNOTES_HTML=$(RELNOTES_INC:.inc=.html)

ONEFILE_TXT_FILES=\
	altos.txt \
	companion.txt \
	telemetry.txt

ONEFILE_RAW_FILES=$(ONEFILE_TXT_FILES:.txt=.raw)
ONEFILE_PDF_FILES=$(ONEFILE_TXT_FILES:.txt=.pdf)

HTML=altusmetrum.html micropeak.html telegps.html $(RELNOTES_HTML) $(ONEFILE_HTML_FILES)

HTML_REVHISTORY=\
	altusmetrum-revhistory.html \
	micropeak-revhistory.html \
	telegps-revhistory.html

PDF=altusmetrum.pdf micropeak.pdf telegps.pdf $(RELNOTES_PDF) $(ONEFILE_PDF_FILES) \
	$(OUTLINE_PDF_FILES)

FOP_STYLE=am-fo.xsl
HTML_STYLE=am-html.xsl
COMMON_STYLE=common.xsl
FOP_XCONF=fop.xconf
STYLESHEET=am.css

FONTS=\
	fonts/DejaVuSansMono-BoldOblique.ttf \
	fonts/DejaVuSansMono-Bold.ttf \
	fonts/DejaVuSansMono-Oblique.ttf \
	fonts/DejaVuSansMono.ttf \
	fonts/OpenSans-Light.ttf \
	fonts/OpenSans-LightItalic.ttf \
	fonts/OpenSans-Regular.ttf \
	fonts/OpenSans-Italic.ttf \
	fonts/OpenSans-Semibold.ttf \
	fonts/OpenSans-SemiboldItalic.ttf

TEMPLATES_TMPL=titlepage.templates.tmpl

TEMPLATES_XSL=$(TEMPLATES_TMPL:.tmpl=.xsl)

PDF_CONFIG_FILES=$(FOP_STYLE) $(COMMON_STYLE) $(FOP_XCONF) $(TEMPLATES_XSL)
HTML_CONFIG_FILES=$(HTML_STYLE) $(COMMON_STYLE) $(TEMPLATES_XSL)

DOC=$(HTML) $(HTML_REVHISTORY) $(PDF) $(IMAGES) $(STYLESHEET)

.SUFFIXES: .tmpl .xsl .inc .txt .raw .pdf .html

.txt.raw:
	sed -e 's/^[ 	]*//' -e 's/^\\//' $*.txt > $@

.inc.raw:
	sed -e 's/^[ 	]*//' -e 's/^\\//' $*.inc > $@

.raw.pdf:
	a2x --verbose -a docinfo -f pdf --xsltproc-opts "--stringparam toc.section.depth 2" --xsl-file $(FOP_STYLE) --fop --fop-opts="-c $(FOP_XCONF)" $*.raw

.raw.html:
	a2x --verbose -a docinfo -f xhtml --xsltproc-opts "--stringparam toc.section.depth 2" --xsl-file $(HTML_STYLE) --stylesheet=$(STYLESHEET) $*.raw

.tmpl.xsl:
	xsltproc --output $@ /usr/share/xml/docbook/stylesheet/docbook-xsl/template/titlepage.xsl $*.tmpl

all:	$(HTML) $(PDF)

$(HTML): $(PDF)

altusmetrum-revhistory.html: altusmetrum.html

micropeak-revhistory.html: micropeak.html

telegps-revhistory.html: telegps.html

altusmetrum.pdf altusmetrum.html: altusmetrum-docinfo.xml $(RAW_FILES) $(RAW_INC) $(IMAGES)

telegps.html telegps.pdf: telegps-docinfo.xml $(TELEGPS_RAW_FILES) $(IMAGES)

micropeak.pdf micropeak.html: micropeak-docinfo.xml $(MICROPEAK_RAW_FILES) $(IMAGES)

install:	all

publish:	$(DOC) $(FONTS)
	cp $(DOC) /home/bdale/web/altusmetrum/AltOS/doc/
	mkdir -p /home/bdale/web/altusmetrum/AltOS/doc/fonts/
	cp $(FONTS) /home/bdale/web/altusmetrum/AltOS/doc/fonts/
	(cd /home/bdale/web/altusmetrum ; \
	 git add /home/bdale/web/altusmetrum/AltOS/doc/* ; \
	 git add /home/bdale/web/altusmetrum/AltOS/doc/fonts/* ; \
	 echo "update docs" | \
	 git commit -F - /home/bdale/web/altusmetrum/AltOS/doc/* /home/bdale/web/altusmetrum/AltOS/doc/fonts/* ; \
	 git push)

publish-keithp:	$(DOC) $(FONTS)
	scp -p $(DOC) keithp.com:~keithp/public_html/altos
	scp -p $(FONTS) keithp.com:~keithp/public_html/altos/fonts

clean:
	rm -f $(HTML) $(HTML_REVHISTORY) $(PDF) $(TEMPLATES_XSL) $(RAW_FILES) $(TELEGPS_RAW_FILES) $(MICROPEAK_RAW_FILES)

distclean: clean
	rm -f $(HTML) $(PDF)

$(PDF): $(PDF_CONFIG_FILES)
$(HTML): $(HTML_CONFIG_FILES)
