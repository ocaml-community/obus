# Makefile
# --------
# Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implemtation of dbus.

# +------------------------------------------------------------------+
# | Configuration                                                    |
# +------------------------------------------------------------------+

OC := ocamlbuild
OF := ocamlfind

# Use classic-display when compiling under a terminal which does not
# support ANSI sequence:
ifeq ($(TERM),dumb)
OC += -classic-display
endif

# Avoid compilation of native plugin if ocamlopt is not available
ifeq ($(shell if which ocamlopt >/dev/null; then echo yes; fi),)
OC += -byte-plugin
endif

# +------------------------------------------------------------------+
# | General rules                                                    |
# +------------------------------------------------------------------+

.PHONY: all
all:
	$(OC) all

.PHONY: byte
byte:
	$(OC) byte

.PHONY: native
native:
	$(OC) native

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name obus-`head -n 1 VERSION`

.PHONY: clean
clean:
	$(OC) -clean

# List all needed packages
.PHONY: list-deps
list-deps:
	@grep -o 'pkg_[^ ,]*' _tags | cut -c 5- | sort | uniq

.PHONY: tests
tests:
	$(OC) tests

.PHONY: test-syntax
test-syntax:
	$(OC) pa_obus.cma
	camlp4o \
	  `ocamlfind query -i-format type-conv.syntax` \
	  `ocamlfind query -predicates syntax,preprocessor -a-format type-conv.syntax` \
	  pa_obus.cma test/syntax_extension.ml

# +------------------------------------------------------------------+
# | Documentation                                                    |
# +------------------------------------------------------------------+

doc:
	$(OC) obus.docdir/index.html

dot:
	$(OC) obus.docdir/index.dot

# +------------------------------------------------------------------+
# | Installation stuff                                               |
# +------------------------------------------------------------------+

.PHONY: prefix
prefix:
	@if [ -z "$(PREFIX)" ]; then \
	  echo "please define PREFIX"; \
	  exit 1; \
	fi

.PHONY: install
install: prefix
	$(OF) install obus _build/META \
	 _build/pa_obus.cma \
	 src/*.mli \
	 bindings/*/*.mli \
	 _build/src/*.cmi \
	 _build/bindings/*/*.cmi \
	 _build/src/*.cmx \
	 _build/bindings/*/*.cmx \
	 _build/*.cma \
	 _build/*.cmxa \
	 _build/*.cmxs \
	 _build/*.a
	install -vm 755 _build/tools/obus_introspect.best $(PREFIX)/bin/obus-introspect
	install -vm 755 _build/tools/obus_binder.best $(PREFIX)/bin/obus-binder
	install -vm 755 _build/tools/obus_dump.best $(PREFIX)/bin/obus-dump
	mkdir -p $(PREFIX)/share/doc/obus/examples
	mkdir -p $(PREFIX)/share/doc/obus/html
	mkdir -p $(PREFIX)/share/doc/obus/scripts
	install -vm 0644 LICENSE $(PREFIX)/share/doc/obus
	install -vm 0644 _build/obus.docdir/* $(PREFIX)/share/doc/obus/html
	install -vm 0644 examples/*.ml $(PREFIX)/share/doc/obus/examples
	install -vm 0755 utils/scripts/* $(PREFIX)/share/doc/obus/scripts
	mkdir -p $(PREFIX)/share/man/man1
	install -vm 0644 _build/man/obus-introspect.1.gz $(PREFIX)/share/man/man1
	install -vm 0644 _build/man/obus-binder.1.gz $(PREFIX)/share/man/man1
	install -vm 0644 _build/man/obus-dump.1.gz $(PREFIX)/share/man/man1

.PHONY: uninstall
uninstall: prefix
	$(OF) remove obus
	rm -vf $(PREFIX)/bin/obus-introspect
	rm -vf $(PREFIX)/bin/obus-binder
	rm -vf $(PREFIX)/bin/obus-dump
	rm -rvf $(PREFIX)/share/doc/obus
	rm -vf $(PREFIX)/share/man/man1/obus-introspect.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-binder.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-dump.1.gz

.PHONY: reinstall
reinstall: uninstall install
