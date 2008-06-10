#!/bin/sh
exec ocamlbuild lib.otarget $(cat tools.itarget samples.itarget)
