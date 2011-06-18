#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
make -C manual all clean-aux

# Add oasis stuff
oasis setup

# Cleanup
rm -f predist.sh boring dist.sh
