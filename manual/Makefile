# Makefile
# --------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implementation of D-Bus.

OC := ocamlbuild -classic-display

default: colored

all:
	$(OC) manual.pdf manual-colored.pdf

normal:
	$(OC) manual.pdf

colored:
	$(OC) manual-colored.pdf

clean:
	$(OC) -clean
