(*
 * genInterf.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val print_for_doc : out_channel -> Introspect.module_tree -> unit
  (** [print_for_doc oc modules] generate a ocaml module signature but
      only for documentation purpose, i.e. remove mutiple function
      declaration for each methods, ... *)

val gen : Introspect.module_tree -> Camlp4.PreCast.Ast.sig_item
