#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
cd manual
melt -pdf -no-link manual.mlt
mv _melt/manual.pdf .
# Remove intermediate files
melt -clean
cd ..

# Remove this file
rm -f predist.sh
