#!/bin/bash

set -e

if [ $# -eq 1 ]; then
    PREFIX="$1"
else
    PREFIX=/usr/local/lib
fi

ocamlfind remove obus
rm -vf $PREFIX/bin/obus-binder $PREFIX/bin/obus-introspect
rm -rvf $PREFIX/share/doc/obus
