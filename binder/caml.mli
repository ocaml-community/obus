(*
 * caml.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** This module generate the caml code for marshaling (resp. loading)
    caml values to (resp. from) dbus values *)

type expr = Camlp4.PreCast.Ast.expr

type caml_id = string
type 'a caml_type = (caml_id, 'a) Type.term

(** {6 Rules for converting types} *)

module Rules : sig
  open Type

  val int : 'a caml_type
  val int32 : 'a caml_type
  val int64 : 'a caml_type
  val float : 'a caml_type
  val bool : 'a caml_type
  val char : 'a caml_type
  val string : 'a caml_type
  val list : 'a caml_type -> 'a caml_type
  val array : 'a caml_type -> 'a caml_type
  val dbus_value : 'a caml_type
  val dbus_types : 'a caml_type
    (** functions for constructing standard caml types *)

  type rule_desc =
    | Array of var caml_type * expr * expr * expr
        (** args:
            - element type
            - empty dict expression
            - adding element expression
            - folding expresssion *)
    | Dict of var caml_type * var caml_type * expr * expr * expr
        (** args:
            - key type
            - values type
            - empty dict expression
            - adding element expression
            - folding expresssion *)
    | Record of (string * var caml_type) list
        (** args:
            - the list of name and type of fields of the record *)
    | Any of var caml_type * expr * expr
        (** Very basic rule, describe how to convert from and to a caml type.
            args:
            - the source caml type
            - reading expression
            - writing expression *)

  type convertion_rule = var caml_type * rule_desc
      (** Describe how to read and write the given caml type *)

  val map : var caml_type -> string -> convertion_rule
    (** [map key_type name] special rule for map created with
        Map.Make. [name] is the module name *)

  val default : convertion_rule list
    (** [default] default rules *)
end

(** {6 Caml modules} *)

module Make(R : sig val rules : Rules.convertion_rule list end) : Language.S
