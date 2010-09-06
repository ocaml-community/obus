#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
make -C manual manual.pdf manual-colored.pdf clean-melt

# The makefile contains no useful rules for the end user
rm -f Makefile

# Add OASIS stuff
OASIS setup

# Remove this file
rm -f predist.sh
