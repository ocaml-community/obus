# Makefile
# --------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implementation of D-Bus.

.PHONY: all clean clean-aux

all: manual.pdf

%.pdf: %.tex
	rubber --pdf $<

clean: clean-aux
	rm -f *.pdf

clean-aux:
	rm -f *.aux *.dvi *.log *.out *.toc *.html *.htoc *.haux
