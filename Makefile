# Makefile
# --------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implementation of D-Bus.

include setup.data

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(pkg_name)-$(pkg_version)
