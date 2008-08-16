#!/bin/sh
#
# check-deps.sh
# -------------
# Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of obus, an ocaml implemtation of dbus.

set -e

err () {
    echo >&2 "`basename $0`: $1"
}

TAGS="_tags"
if [ ! -f $TAGS ]; then
    err "_tags file not found!"
    exit 2
fi

# Retreive the list of dependencies from the _tags file
DEPS=`grep -o 'pkg_[^ ,]*' $TAGS | cut -c 5- | sort | uniq`

# List all dependencies on stdout
if [ "$1" = "list" ]; then
    echo $DEPS
    exit 0
fi

# Verify that ocamlfind is present
echo -n "checking for ocamlfind: "
if which ocamlfind &> /dev/null; then
    echo "OK"
else
    echo "missing"
    err "obus cannot be built without ocamlfind!"
    exit 1
fi

# Check that all dependencies are present
MISSINGS=""
for pkg in $DEPS; do
    echo -n "checking for package $pkg: "
    if ocamlfind query $pkg &> /dev/null; then
        echo "OK"
    else
        echo "missing"
        MISSINGS="$MISSINGS $pkg"
    fi
done

if [ -n "$MISSINGS" ]; then
    err "the fillowing packages are missing: $MISSINGS"
    err "please install them to compile obus."
    exit 1
fi

[ -d _build ] || mkdir _build
touch _build/dependencies-checked
