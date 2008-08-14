OC = ocamlbuild
OF = ocamlfind
PREFIX = /usr/local

# Targets
SAMPLES = hello bus-functions eject notif monitor
LIB = obus
BINDINGS = hal notify
TOOLS = obus-introspect obus-binder
TEST = data dyn dumper

.PHONY: tools samples bindings all test lib default

default: samples-byte

all: lib bindings tools samples

# +------------------+
# | Specific targets |
# +------------------+

lib-byte:
	$(OC) $(LIB:=.cma)

lib-native:
	$(OC) $(LIB:=.cmxa)

lib:
	$(OC) $(LIB:=.cma) $(LIB:=.cmxa)

bindings-byte:
	$(OC) $(BINDINGS:=.cma)

bindings-native:
	$(OC) $(BINDINGS:=.cmxa)

bindings:
	$(OC) $(BINDINGS:=.cma) $(BINDINGS:=.cmxa)

samples-byte:
	$(OC) $(SAMPLES:%=samples/%.byte)

samples-native:
	$(OC) $(SAMPLES:%=samples/%.native)

samples:
	$(OC) $(SAMPLES:%=samples/%.byte) $(SAMPLES:%=samples/%.native)

tools-byte:
	$(OC) $(TOOLS:%=tools/%.byte)

tools-native:
	$(OC) $(TOOLS:%=tools/%.native)

tools:
	$(OC) $(TOOLS:%=tools/%.byte)  $(TOOLS:%=tools/%.native)

test:
	$(OC) $(TEST:%=test/%.d.byte)

test-syntax: syntax/pa_obus.cmo
	camlp4o _build/syntax/pa_obus.cmo test/syntax_extension.ml

# +---------------+
# | Documentation |
# +---------------+

doc:
	$(OC) obus.docdir/index.html

dot:
	$(OC) obus.docdir/index.dot

# +--------------------+
# | Installation stuff |
# +--------------------+

install: lib bindings tools doc
	$(OC) META lib-dist
	cd _build
	$(OF) install obus META `cat lib-dist` $(LIB:=.cma) $(LIB:=.cmxa) $(BINDINGS:=.cma) $(BINDINGS:=.cmxa)
	for tool in tools; do \
	  install -vm 0755 tools/$tool.native $(PREFIX)/bin/$tool \
	done
	mkdir -p $(PREFIX)/share/doc/obus/{samples,html}
	install -vm 0644 ../LICENSE $(PREFIX)/share/doc/obus
	install -vm 0644 obus.docdir/* $(PREFIX)/share/doc/obus/html
	install -vm 0644 ../samples/*.ml ../interfaces/*.xml $(PREFIX)/share/doc/obus/samples

uninstall:
	$(OF) remove obus
	rm -vf $(TOOLS:%=$(PREFIX)/bin/%)
	rm -rvf $(PREFIX)/share/doc/obus

# +-------+
# | Other |
# +-------+

clean:
	$(OC) -clean

# "make" is shorter than "ocamlbuild"...
%:
	$(OC) $*
