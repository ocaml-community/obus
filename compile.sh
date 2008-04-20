#!/bin/sh
exec ocamlbuild binder/obus-caml-map.byte binder/obus-caml-gen.byte obus/OBus.cma
