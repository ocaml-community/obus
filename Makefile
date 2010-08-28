# Makefile
# --------
# Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implementation of D-Bus.

# +------------------------------------------------------------------+
# | Configuration                                                    |
# +------------------------------------------------------------------+

OC := ocamlbuild -classic-display
OF := ocamlfind

# Avoid compilation of native plugin if ocamlopt is not available
ifeq ($(shell if which ocamlopt >/dev/null; then echo yes; fi),)
OC += -byte-plugin
endif

# The library version
VERSION := $(shell head -n 1 VERSION)

# +------------------------------------------------------------------+
# | General rules                                                    |
# +------------------------------------------------------------------+

# Compiles everything (libraries, tools, doc and examples).
#
# Libraries are compiled in byte-code and in native-code if possible,
# and binaries are compiled in native-code if possible and in
# byte-code otherwise
.PHONY: all
all:
	$(OC) all

# Same as "all" except that libraries and binaries are compiled only
# in byte-code
.PHONY: byte
byte:
	$(OC) byte

# Same as "all" except that libraries and binaries are compiled only
# in native-code
.PHONY: native
native:
	$(OC) native

# Same as "byte" except that everything is compiled with debugging
# support
.PHONY: debug
debug:
	$(OC) debug

# Compiles only libraries in byte-code and in native-code if possible
.PHONY: libs
libs:
	$(OC) libs

# Creates a distributable tarball
.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name obus-$(VERSION)

.PHONY: clean
clean:
	$(OC) -clean

# List all needed packages
.PHONY: list-deps
list-deps:
	@grep -o 'pkg_[^ ,]*' _tags | cut -c 5- | sort | uniq

# +------------------------------------------------------------------+
# | Tests                                                            |
# +------------------------------------------------------------------+

.PHONY: tests
tests:
	$(OC) tests/main.d.byte

.PHONY: test-syntax
test-syntax:
	$(OC) pa_obus.cma
	camlp4o \
	  `ocamlfind query -i-format type-conv.syntax` \
	  `ocamlfind query -predicates syntax,preprocessor -a-format type-conv.syntax` \
	  _build/pa_obus.cma tests/syntax_extension.ml -o _build/result.ml

# +------------------------------------------------------------------+
# | Documentation                                                    |
# +------------------------------------------------------------------+

doc:
	$(OC) doc

dot:
	$(OC) obus.docdir/index.dot

# +------------------------------------------------------------------+
# | Installation stuff                                               |
# +------------------------------------------------------------------+

.PHONY: install-libs
install-libs:
	$(OF) install obus _build/META \
	 _build/syntax/pa_obus.cmo \
	 _build/src/oBus_top.cmo \
	 src/*.mli \
	 bindings/*/*.mli \
	 $(wildcard _build/src/*.cmi) \
	 $(wildcard _build/bindings/*/*.cmi) \
	 $(wildcard _build/src/*.cmx) \
	 $(wildcard _build/bindings/*/*.cmx) \
	 $(wildcard _build/bindings/*/*.cma) \
	 $(wildcard _build/bindings/*/*.cmxa) \
	 $(wildcard _build/bindings/*/*.cmxs) \
	 $(wildcard _build/bindings/*/*.a) \
	 $(wildcard _build/*.cma) \
	 $(wildcard _build/*.cmxa) \
	 $(wildcard _build/*.cmxs) \
	 $(wildcard _build/*.a)

.PHONY: uninstall-libs
uninstall-libs:
	$(OF) remove obus

.PHONY: reinstall-libs
reinstall-libs: uninstall-libs install-libs

.PHONY: prefix
prefix:
	@if [ -z "$(PREFIX)" ]; then \
	  echo "please define PREFIX"; \
	  exit 1; \
	fi

.PHONY: install
install: prefix install-libs
	install -vm 755 _build/tools/obus_introspect.best $(PREFIX)/bin/obus-introspect
	install -vm 755 _build/tools/obus_dump.best $(PREFIX)/bin/obus-dump
	install -vm 755 _build/tools/obus_gen_interface.best $(PREFIX)/bin/obus-gen-interface
	install -vm 755 _build/tools/obus_gen_client.best $(PREFIX)/bin/obus-gen-client
	install -vm 755 _build/tools/obus_gen_server.best $(PREFIX)/bin/obus-gen-server
	install -vm 755 _build/tools/obus_xml2idl.best $(PREFIX)/bin/obus-xml2idl
	install -vm 755 _build/tools/obus_idl2xml.best $(PREFIX)/bin/obus-idl2xml
	mkdir -p $(PREFIX)/share/doc/obus/examples
	mkdir -p $(PREFIX)/share/doc/obus/html
	mkdir -p $(PREFIX)/share/doc/obus/scripts
	install -vm 0644 LICENSE $(PREFIX)/share/doc/obus
	install -vm 0644 _build/obus.docdir/* $(PREFIX)/share/doc/obus/html
	install -vm 0644 examples/*.ml $(PREFIX)/share/doc/obus/examples
	install -vm 0755 utils/scripts/* $(PREFIX)/share/doc/obus/scripts
	mkdir -p $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-introspect.1.gz $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-dump.1.gz $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-gen-interface.1.gz $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-gen-client.1.gz $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-gen-server.1.gz $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-xml2idl.1.gz $(PREFIX)/share/man/man1
	install -vm 755 _build/man/obus-idl2xml.1.gz $(PREFIX)/share/man/man1

.PHONY: uninstall
uninstall: prefix uninstall-libs
	rm -vf $(PREFIX)/bin/obus-introspect
	rm -vf $(PREFIX)/bin/obus-dump
	rm -vf $(PREFIX)/bin/obus-gen-interface
	rm -vf $(PREFIX)/bin/obus-gen-client
	rm -vf $(PREFIX)/bin/obus-gen-server
	rm -vf $(PREFIX)/bin/obus-xml2idl
	rm -vf $(PREFIX)/bin/obus-idl2xml
	rm -rvf $(PREFIX)/share/doc/obus
	rm -vf $(PREFIX)/share/man/man1/obus-introspect.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-dump.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-gen-interface.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-gen-client.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-gen-server.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-xml2idl.1.gz
	rm -vf $(PREFIX)/share/man/man1/obus-idl2xml.1.gz

.PHONY: reinstall
reinstall: uninstall install
