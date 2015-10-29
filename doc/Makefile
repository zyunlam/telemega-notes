#
#	http://docbook.sourceforge.net/release/xsl/current/README
#

RELNOTES=\
	release-notes-0.7.1.html \
	release-notes-0.8.html \
	release-notes-0.9.html \
	release-notes-0.9.2.html \
	release-notes-1.0.1.html \
	release-notes-1.1.html \
	release-notes-1.1.1.html \
	release-notes-1.2.html \
	release-notes-1.2.1.html \
	release-notes-1.3.html \
	release-notes-1.3.1.html \
	release-notes-1.3.2.html \
	release-notes-1.4.html \
	release-notes-1.4.1.html \
	release-notes-1.5.html \
	release-notes-1.6.html \
	release-notes-1.6.1.html

PICTURES=\
	altosui.png \
	ascent.png \
	configure-altimeter.png \
	configure-altosui.png \
	configure-groundstation.png \
	configure-pyro.png \
	descent.png \
	device-selection.png \
	easymini-top.jpg \
	fire-igniter.png \
	graph-configure.png \
	graph-map.png \
	graph.png \
	graph-stats.png \
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
	telemega-v1.0-top.jpg \
	telemetrum-v1.1-thside.jpg \
	telemini-v1-top.jpg \
	telemini-v2-top.jpg

TXT_FILES=altusmetrum.txt
INC_FILES=\
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
	system-operation.inc \
	flight-data-recording.inc \
	handling.inc \
	specs.inc

RAW_FILES=$(TXT_FILES:.txt=.raw)

RAW_INC=$(INC_FILES:.inc=.raw)

AD_PDF=$(TXT_FILES:.txt=.pdf)

SVG=\
	easymini.svg \
	telemega.svg \
	telemetrum.svg \
	telemini.svg \
	easymega.svg

RELNOTES_XSL=$(RELNOTES:.html=.xsl)
HTML=altusmetrum.html altos.html telemetry.html companion.html micropeak.html telegps.html $(RELNOTES)
PDF=altusmetrum.pdf altos.pdf telemetry.pdf companion.pdf micropeak.pdf telegps.pdf \
	telemetrum-outline.pdf telemega-outline.pdf easymini-outline.pdf easymega-outline.pdf $(AD_PDF)
HTMLSTYLE=/usr/share/xml/docbook/stylesheet/docbook-xsl/html/docbook.xsl
FOSTYLE=xorg-fo.xsl
FOPCFG=fop-cfg.xml
TEMPLATES=titlepage.templates.xsl
PDFSTYLE=
IMAGES=$(PICTURES) $(SVG)
DOC=$(HTML) $(PDF) $(IMAGES)

.SUFFIXES: .inc .txt .raw .pdf .html

XSLTFLAGS=--stringparam section.autolabel 1 --xinclude

.txt.raw:
	sed -e 's/@@VERSION@@/$(VERSION)/' -e 's/@@DATE@@/$(DATE)/' -e 's/^[ 	]*//' -e 's/^\\//' $*.txt > $@

.inc.raw:
	sed -e 's/@@VERSION@@/$(VERSION)/' -e 's/@@DATE@@/$(DATE)/' -e 's/^[ 	]*//' -e 's/^\\//' $*.inc > $@

.raw.pdf:
	a2x --verbose -k -d book -a docinfo -f pdf --xsltproc-opts "--stringparam toc.section.depth 2" --xsl-file am-fo.xsl --fop --fop-opts="-c fop.xconf" $*.raw

.raw.html:
	a2x --verbose -k -d book -a docinfo -f xhtml --xsltproc-opts "--stringparam toc.section.depth 2" --stylesheet=am.css $*.raw

.xsl.html:
	xsltproc $(XSLTFLAGS) -o $@ $(HTMLSTYLE) $*.xsl

.xsl.pdf:
	xmlto -p '-c $(FOPCFG)' --searchpath `pwd` -x $(FOSTYLE) --with-fop pdf $*.xsl

.xml.xsl:
	xsltproc --output $@ /usr/share/xml/docbook/stylesheet/docbook-xsl/template/titlepage.xsl $*.xml

all:	$(HTML) $(PDF)

altusmetrum.pdf: altusmetrum-docinfo.xml $(RAW_FILES) $(RAW_INC)

install:	all

publish:	$(DOC)
	cp $(DOC) /home/bdale/web/altusmetrum/AltOS/doc/
	(cd /home/bdale/web/altusmetrum ; \
	 git add /home/bdale/web/altusmetrum/AltOS/doc/* ; \
	 echo "update docs" | \
	 git commit -F - /home/bdale/web/altusmetrum/AltOS/doc/* ; \
	 git push)

clean:
	rm -f $(HTML) $(PDF) $(TEMPLATES)

distclean:
	rm -f $(HTML) $(PDF)

altusmetrum.html: $(RELNOTES_XSL) $(IMAGES)
altusmetrum.pdf: $(RELNOTES_XSL) $(IMAGES)

telegps.html: $(RELNOTES_XSL) $(IMAGES)
telegps.pdf: $(RELNOTES_XSL) $(IMAGES)

$(PDF): $(FOSTYLE) $(TEMPLATES) $(FOPCFG)

indent:		altusmetrum.xsl
	xmlindent -i 2 < altusmetrum.xsl > altusmetrum.new

$(FOPCFG): Makefile
	(echo '<fop version="1.0">'; echo '  <base>'"`pwd`"'</base>'; echo '</fop>') > $@

