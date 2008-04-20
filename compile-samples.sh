#!/bin/sh

deps="binder/obus-caml-map.byte binder/obus-caml-gen.byte obus/OBus.cma"
samples=samples/eject.byte

ocamlbuild $deps || exit 1

map=./obus-caml-map.byte
gen=./obus-caml-gen.byte

$map -allow-default samples/xmls/dbus.xml -o samples/maps/dbus.map.xml
sed -i 's/Org\.Freedesktop\.//' samples/maps/dbus.map.xml
$gen samples/maps/dbus.map.xml -o samples/DBus

generate () {
    dest=$1
    shift
    $map $* -o samples/maps/${dest}.map.xml
    sed -i 's/Org\.Freedesktop\.//' samples/maps/${dest}.map.xml
    $gen samples/maps/${dest}.map.xml -o samples/${dest}
}

generate hal samples/xmls/hal-*.xml

exec ocamlbuild -I obus $samples
