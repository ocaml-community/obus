OC = ocamlbuild
OF = ocamlfind
PREFIX = /usr/local

# Targets
SAMPLES = hello bus-functions eject notify monitor
LIB = obus
BINDINGS = hal notify
TOOLS = obus-introspect obus-binder

.PHONY: tools samples bindings all

all: samples-byte

# +------------------+
# | Specific targets |
# +------------------+

lib-byte:
	$(OC) $(LIB:=.cma)

lib-native:
	$(OC) $(LIB:=.cmxa)

lib:
	$(OC) $(LIB:=.{cma,cmxa})

bindings-byte:
	$(OC) $(BINDINGS:=.cma)

bindings-native:
	$(OC) $(BINDINGS:=.cmxa)

bindings:
	$(OC) $(BINDINGS:=.{cma,cmxa})

samples-byte:
	$(OC) $(SAMPLES:%=samples/%.byte)

samples-native:
	$(OC) $(SAMPLES:%=samples/%.native)

samples:
	$(OC) $(SAMPLES:%=samples/%.{byte,native})

tools-byte:
	$(OC) $(TOOLS:%=tools/%.byte)

tools-native:
	$(OC) $(TOOLS:%=tools/%.native)

tools:
	$(OC) $(TOOLS:%=tools/%.{byte,native})

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
	$(OF) install obus META `cat lib-dist` $(LIB:=.{cma,cmxa}) $(BINDINGS:=.{cma,cmxa})
	for tool in tools; do \
	  install -vm 0755 tools/$tool.byte $(PREFIX)/bin/$tool \
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
