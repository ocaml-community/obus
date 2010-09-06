# Makefile
# --------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implementation of D-Bus.

MELT := melt -latex xelatex -pdf -no-link myocamlbuild.ml

default: colored

all: manual.pdf manual-colored.pdf

normal:
	env OBUS_MANUAL_USE_COLORS=0 $(MELT) manual.mlt

colored:
	env OBUS_MANUAL_USE_COLORS=1 $(MELT) manual.mlt

manual.pdf:
	env OBUS_MANUAL_USE_COLORS=0 $(MELT) manual.mlt
	cp _melt/manual.pdf manual.pdf

manual-colored.pdf:
	env OBUS_MANUAL_USE_COLORS=1 $(MELT) manual.mlt
	cp _melt/manual.pdf manual-colored.pdf

clean-melt:
	melt -clean

clean:
	melt -clean
	rm -f *.pdf
