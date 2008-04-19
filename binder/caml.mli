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

(** {6 Rules for converting types} *)

module Rules : sig
  type typ

  val v : string -> typ
  val int : typ
  val int32 : typ
  val int64 : typ
  val float : typ
  val bool : typ
  val char : typ
  val string : typ
  val list : typ -> typ
  val array : typ -> typ
  val dbus_value : typ
  val dbus_type : typ
  val tuple : typ list -> typ
    (** functions for constructing standard caml types *)

  val t0 : string -> typ
  val t1 : string -> typ -> typ
  val t2 : string -> typ -> typ -> typ
  val t3 : string -> typ -> typ -> typ -> typ
  val mkt : string -> typ list -> typ

  type rule_desc =
    | Array of typ * expr * expr * expr
        (** args:
            - element type
            - empty dict expression
            - adding element expression
            - folding expresssion *)
    | Dict of typ * typ * expr * expr * expr
        (** args:
            - key type
            - values type
            - empty dict expression
            - adding element expression
            - folding expresssion *)
    | Record of (string * typ) list
        (** args:
            - the list of name and type of fields of the record *)
    | Any of typ * expr * expr
        (** Very basic rule, describe how to convert from and to a caml type.
            args:
            - the source caml type
            - reading expression
            - writing expression *)

  type convertion_rule = typ * rule_desc
      (** Describe how to read and write the given caml type *)

  val map : typ -> string -> convertion_rule
    (** [map key_type name] special rule for map created with
        Map.Make. [name] is the module name *)

  val default : convertion_rule list
    (** [default] default rules *)
end

(** {6 Caml modules} *)

module Make(R : sig val rules : Rules.convertion_rule list end) : Language.S
