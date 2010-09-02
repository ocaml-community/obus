# Makefile
# --------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implementation of D-Bus.

PDF_FILES := ${subst .ml,.pdf,${wildcard *.ml}}

all: ${PDF_FILES}
	melt -no-link -pdf ${PDF_FILES} manual.mlt

%.pdf: %.html
	wkhtmltopdf $< $@
	pdfcrop $@ $@

%.html: %.ml
	chmod +x htmlize.sh
	./htmlize.sh $< $@

clean:
	melt -clean
	rm -f ${wildcard *.pdf} ${wildcard *.html}
