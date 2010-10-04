#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
make -C manual manual.pdf clean-melt

# Add OASIS stuff
OASIS setup

# Cleanup
rm -f predist.sh make-dist.sh
