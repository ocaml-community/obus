#!/bin/sh
LOWLEVELS="hello hello2 notify"
PROXIES="eject bus-functions signals request-name"
samples=""
for i in $LOWLEVELS; do
    samples="$samples samples/lowlevels/$i.byte"
done
for i in $PROXIES; do
    samples="$samples samples/proxies/$i.byte"
done
exec ocamlbuild $samples
