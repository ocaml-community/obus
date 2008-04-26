#!/bin/sh
exec ocamlbuild `echo samples/lowlevels/*.ml|sed -s 's/\.ml/.byte/g'`
