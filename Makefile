OC = ocamlbuild
OF = ocamlfind
PREFIX = /usr/local

.PHONY: samples

all: samples

lib:
	$(OC) lib.otarget

doc:
	$(OC) obus.docdir/index.html

tools:
	$(OC) `cat tools.itarget`

samples:
	$(OC) `cat samples.itarget`

install:
	$(OC) META lib.otarget lib-dist tools.otarget obus.docdir/index.html
	cd _build
	$(OF) install obus META `cat lib-dist` `cat lib.itarget`
	install -vm 0755 tools/obus-binder.byte $(PREFIX)/bin/obus-binder
	install -vm 0755 tools/obus-introspect.byte $(PREFIX)/bin/obus-introspect
	mkdir -p $(PREFIX)/share/doc/obus/{samples,html}
	install -vm 0644 ../LICENSE $(PREFIX)/share/doc/obus
	install -vm 0644 obus.docdir/* $(PREFIX)/share/doc/obus/html
	install -vm 0644 ../samples/*.ml ../interfaces/*.xml $(PREFIX)/share/doc/obus/samples

uninstall:
	$(OF) remove obus
	rm -vf $(PREFIX)/bin/obus-binder $(PREFIX)/bin/obus-introspect
	rm -rvf $(PREFIX)/share/doc/obus

clean:
	$(OC) -clean

%:
	$(OC) $*
