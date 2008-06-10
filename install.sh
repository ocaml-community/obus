#!/bin/bash

set -e

if [ $# -eq 1 ]; then
    PREFIX="$1"
else
    PREFIX=/usr/local/lib
fi

ocamlbuild lib.otarget tools.itarget obus.docdir/index.html lib-dist
cd _build
ocamlfind install obus ../META obus/OBus.cmi `cat lib.itarget lib-dist` obus.docdir/index.html
install -vm 0755 tools/obus-binder.byte $PREFIX/bin/obus-binder
install -vm 0755 tools/obus-introspect.byte $PREFIX/bin/obus-introspect
mkdir -p $PREFIX/share/doc/obus/{samples,html}
install -vm 0644 ../LICENSE $PREFIX/share/doc/obus
install -vm 0644 obus.docdir/* $PREFIX/share/doc/obus/html
install -vm 0644 ../samples/*.ml ../samples/*/*.ml ../interfaces/*.xml $PREFIX/share/doc/obus/samples
