#!/bin/sh
LOWLEVELS="hello hello2 notify"
PROXIES="eject"
samples=""
for i in $LOWLEVELS; do
    samples+=" samples/lowlevels/$i.byte"
done
for i in $PROXIES; do
    samples+=" samples/proxies/$i.byte"
done
exec ocamlbuild $samples
