#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
cd manual
make all
mv _build/manual.pdf _build/manual-colored.pdf .
make clean
cd ..

# The makefile contains no useful rules for the end user
rm -f Makefile

# Add OASIS stuff
OASIS setup

# Remove this file
rm -f predist.sh
