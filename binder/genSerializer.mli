(*
 * genSerializer.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Generate ocaml function for serializing values *)

open AbstractCode
open Types

type rule

val default_rules : rule list
  (** [default_rules] all default rules *)

val rule_alias : caml_type -> caml_type -> rule
  (** [rule_alias typ1 typ2] define a type aliasing *)

val rule_convert : caml_type -> caml_type -> expr -> expr -> rule
  (** [rule_convert typea typeb a_of_b b_of_a] rule for doing convertion
      between caml values of types [typea] and [typeb]. *)

val rule_array : caml_type -> caml_type -> bool -> expr -> (expr -> expr -> expr)
  -> (expr -> expr -> expr -> expr) -> (patt -> patt -> expr -> expr) -> rule
    (** [rule_array typ elt_type reverse empty_expr add_expr fold_expr
        make_elt_writer_expr] rule for marshaling/unmarshaling caml
        values of type [typ] into a dbus marshaled arrays.

        [elt_type] is the caml type of one element of the array.

        [empty] is a caml expression of type [typ] representing an empty
        value of type [typ].

        [add x acc] is an expression representing the addition of [x] to
        [acc].

        [fold f l x] is an expression that must behave like [Set.S.fold
        f l x], modulo arguments order for [f].

        [make_elt_writer_expr id_elt id_acc fun_expr] must construct an
        expression of a function which will be the first argument of
        [fold], this is for having correct argument order.

        if [reverse] is true then the element of the array will be added
        in the reverse order they are marshaled and the function doing
        that will not be tail-recursive. *)

val rule_dict : caml_type -> caml_type -> caml_type -> bool -> expr -> (expr -> expr -> expr -> expr)
  -> (expr -> expr -> expr -> expr) -> (patt -> patt -> patt -> expr -> expr) -> rule
    (** [rule_dict typ key_type val_type reverse empty_expr add_expr
        fold_expr make_elt_writer_expr] same as [rule_array] but for dictionaries *)

val rule_set : string -> caml_type -> rule
  (** [rule_set module_name elt_type] shorthand for array created with
      [Set.Make] *)

val rule_map : string -> caml_type -> rule
  (** [rule_map module_name key_type] shorthand for map created with
      [Map.Make] *)

val rule_record : caml_type -> (string * caml_type) list -> rule
  (** [rule_record typ fields] rule for reading/writing a sequence of
      dbus marshaled values into/from this caml record *)

val rule_variant : caml_type -> caml_type -> (patt * expr * string * caml_type list * dbus_type) list -> rule
  (** [rule_variant typ key_type variants] rule for reading/writing a
      dbus marshaled variant into/from this caml variant *)

val rule_record_option : caml_type -> caml_type -> (patt * expr * string * caml_type * dbus_type) list -> rule
  (** [rule_record_option typ key_type fields] rule for
      reading/writing a record where each field is an option, like for
      [OBus.Header.fields] *)

type env = (ident * expr) list
    (** Environment for generating a module implementation, it record
        all auxiliary generated functions *)

val gen_reader : bool -> rule list -> caml_type -> dbus_type -> env -> env * code
  (** [gen_reader trace rules caml_type dbus_type env] generate a
      function that read a dbus serialized value of type [dbus_type]
      into a caml value of type [caml_type].

      If [trace] is true then the trace of generation will be printed.

      It can generate auxiliary functions which are added to [env]. *)

val gen_writer : bool -> rule list -> caml_type -> dbus_type -> env -> env * code
  (** [gen_writer trace rules dbus_type caml_type env] like
      [gen_reader] but for writing *)
