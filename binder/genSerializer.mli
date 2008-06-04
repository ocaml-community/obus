(*
 * genSerializer.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Generate a function for marshaling or unmarshaling caml values *)

open Types

type eqn = caml_type * dbus_type
    (** An equation is a pair of a caml type x and a dbus type
        y.

        Solving the equation "x = y" consist on founding a caml
        expression which read (resp. write) a dbus marshaled value of
        type x into (resp. from) a caml value of type y *)

type sol = Compile.env Instruction.t list
    (** A solution to an equation is here a sequence of instruction
        representing an expression which read/write a sequence of
        values *)

type rule = (eqn, sol) Solver.rule

type e0 = expr
type e1 = expr -> expr
type e2 = expr -> expr -> expr
type e3 = expr -> expr -> expr -> expr
    (** [ex] represent a caml value which take [x] argument *)

val flat : Compile.env Instruction.t list -> Compile.env Instruction.t list
  (** Build a sequence of instructions which read or write only one
      value from instructions which read/write multiple values *)

(** {6 Common rules} **)

val rule_record : caml_type -> (string * caml_type) list -> rule
  (** [rule_record typ fields] rule for reading/writing a sequence of
      dbus marshaled values into this caml record *)

module Reading : sig

  (** {6 Reading rules} **)

  val default_rules : rule list
    (** All default rules *)

  val rule_array : caml_type -> caml_type -> ?reverse:bool -> e0 -> e2 -> rule
    (** [rule_array typ elt_type reverse empty add] rule for
        reading/writing arrays.

        [elt_type] is the caml type of one element of the array.

        [empty] is a caml expression of type [typ] representing an empty
        value of type [typ].

        [add] is a function which add it first argument to its second.

        if [reverse] is true then the element of the array will be added
        in the reverse order they are marshaled and the function doing
        that will not be tail-recursive. *)

  val rule_dict : caml_type -> caml_type -> caml_type -> ?reverse:bool -> e0 -> e3 -> rule
    (** [rule_dict typ key_type val_type reverse empty add] same as
        [rule_array] but for dictionaries *)
end

module Writing : sig

  (** {6 Writing rules} **)

  val default_rules : rule list
    (** All default rules *)

  val rule_array : caml_type -> caml_type -> ?mk_fold_func:(patt -> patt -> expr -> expr) -> e3 -> rule
      (** [rule_array typ elt_type mk_fold_func fold]
          rule for reading/writing arrays.

          [elt_type] is the caml type of one element of the array.

          [fold] is an expression that must behave like
          [List.fold_right].

          [mk_fold_func] must be used if argument order for [fold] is
          other than [elt -> acc -> acc] *)

  val rule_dict : caml_type -> caml_type -> caml_type -> ?mk_fold_func:(patt -> patt -> patt -> expr -> expr) -> e3 -> rule
end
(*
val variant_reader : (eqn -> sol) -> caml_type -> dbus_type ->
  (patt * string * caml_type list * dbus_type) list -> env -> ident * env
val variant_writer : (eqn -> sol) -> caml_type -> dbus_type ->
  (expr * string * caml_type list * dbus_type) list -> env -> ident * env
  (** [variant_action solver key_type key_dbus_type constructors env]
      build an expression for reading/writing a caml variant from/to a
      integer followed by a dbus variant. [solver] is used to generate
      marshaling/unmarshaling expressions for each field. *)

val record_option : (eqn -> sol) -> caml_type -> dbus_type ->
  (patt * string * caml_type * dbus_type) list -> env -> ident * env
val record_option : (eqn -> sol) -> caml_type -> dbus_type ->
  (expr * string * caml_type * dbus_type) list -> env -> ident * env
  (** [record_option key_type key_dbus_type fields env] expression for
      reading/writing a record where each field is an option, like for
      [OBus.Header.fields] *)
*)
