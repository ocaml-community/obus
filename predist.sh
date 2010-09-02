#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
cd manual
unset OBUS_COLORED_MANUAL
make
mv _melt/manual.pdf ../manual.pdf

# Build also the colored version
make clean
OBUS_COLORED_MANUAL=1
export OBUS_COLORED_MANUAL
make
mv _melt/manual.pdf ../manual-colored.pdf

# Remove manual sources:
cd ..
rm -rf manual

# Remove this file
rm -f predist.sh
