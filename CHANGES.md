1.1.8 (unreleased)
------------------

* opam: add dependency on `oasis`, `lwt_react`, `lwt_camlp4`, `lwt_log`
* opam: `ocamlfind` is now a build dependency
* add support for OCaml 4.06 and `lwt` 3
* bump minimum OCaml version to 4.02.3
* enable travis tests
* fix missing signature validation

1.1.7 (2016-07-18)
------------------

* fix compatibility with OCaml 4.03.0

1.1.6 (2014-04-21)
------------------

* support for React 1.0.0

1.1.5 (2012-10-02)
------------------

* compatibility fix for type-conv

1.1.4 (2012-07-30)
------------------

* update oasis files
* minor fixes

1.1.3 (2011-07-29)
------------------

* depends on type-conv instead of type-conv.syntax
* implements version 0.18 of the specification:
    * add the `eavesdrop` match keyword

1.1.2 (2011-04-12)
------------------

* implement property monitoring for upower, udisks and network-manager
* implement new D-Bus errors (UnknownObject, UnknownInterface, ...)
* update and implement new argument filters (argNpath and argNnamespace)

1.1.1 (2011-02-14)
------------------

* Fix a race condition in servers that may causes authentication to hang
* Add support for launchd addresses

1.1 (2010-12-13)
----------------

  * First stable release
