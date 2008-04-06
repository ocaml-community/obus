(*
 * caml.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type caml_expr = Camlp4.PreCast.Ast.expr
type caml_patt = Camlp4.PreCast.Ast.patt
type caml_type = Camlp4.PreCast.Ast.ctyp

type var = string

type yes
type no

type ('is_basic, 'is_single) dbus_reader
type basic_reader = (yes, yes) dbus_reader
type single_reader = (no, yes) dbus_reader
type sequence_reader = (no, no) dbus_reader
    (** Types for dbus values *)

val rint16 : basic_reader
val rint32 : basic_reader
val rint64 : basic_reader
val ruint16 : basic_reader
val ruint32 : basic_reader
val ruint64 : basic_reader
  (** Reader for the various dbus int types, this readers always
      return a caml int *)

val rint32_as_int32 : basic_reader
val rint64_as_int64 : basic_reader
val ruint32_as_int32 : basic_reader
val ruint64_as_int64 : basic_reader
  (** Same thing but return int32 and int64 caml value *)

val rsignature : basic_reader
  (** Read a dbus signature into a OBus.dbus_type *)

val rsignature_as_string : basic_reader
  (** Read a signature into a string representing the dbus type using
      dbus type codes *)

val rbyte : basic_reader
  (** Read a dbus byte into a caml char *)

val rboolean : basic_reader
  (** Read a dbus boolean into a caml bool *)

val rdouble : basic_reader
  (** Read a dbus double into a caml double *)

val rstring : basic_reader
  (** Read a dbus string into a caml string *)

val robject_path : basic_reader
  (** Other dbus basic value readers *)

val rarray : (_, yes) dbus_reader -> caml_expr -> caml_expr -> single_reader
  (** [rarray reader f x] correspond to a function that read a dbus
      array [|a1; a2; ...; an|] using [reader] and construct the caml
      value (f an (... (f a2 (f a1 x)) ...)) *)

val rdict : (yes, yes) dbus_reader -> (_, yes) dbus_reader -> caml_expr -> caml_expr -> single_reader
  (** Same thing as darray but for dbus dictionary *)

val rstruct : (_, _) dbus_reader -> single_reader
  (** [dstruct seq] read a sequence of dbus value packed into a dbus
      struct *)

val rvariant : single_reader
  (** [rvariant] read a dbus variant into a caml OBus.dbus_value *)

val rfixed_variant : caml_expr -> (caml_patt * (_, yes) dbus_reader) list -> single_reader
  (** [rfixed_variant e branches] read a variant into a caml value
      according to the value of [e].

      For example in a dbus message header, the additionnal header
      fields are represented as byte * variant dbus value and the
      content of the variant depend on the value of the first byte, so
      we can use this reader :

      dcons dbyte <:patt< b >> (dfixed_variant <:expr< b >>
        [ <:patt< 0 >> --> dbind dstring "x" <:expr< Sender(x) >>;
          <:patt< 1 >> --> dbind duint32 "x" <:expr< Serial(x) >>;
          ... ]

      Note: as the content type of the variant is not known, readers
      have to be "concrete readers", so they must not contains any
      variable or rcaml. An invalid argument is raised in other
      case.
  *)

val rcons : (_, yes) dbus_reader -> caml_patt -> (_, _) dbus_reader -> sequence_reader
  (** [rcons head var tail] read a single dbus value with [head], then
      bind its result to [var] and read others value with [tail] *)

val rbind : ('a, 'b) dbus_reader -> caml_patt -> caml_expr -> ('a, 'b) dbus_reader
  (** [rbind reader var expr] read a dbus value with [reader], bind
      its result to [var] in [expr].

      For example if you want to convert an dbus uint32 into a caml
      type:

      type t = A | B

      just write something like that:

      dbind duint32 <:patt< x >> <:expr< match x with
                                           | 0 -> A
                                           | 1 -> B >>
  *)

val rcaml : caml_type -> (_, _) dbus_reader
  (** [rcaml caml_type] read a dbus value into a caml value of type
      [caml_type] if we already know how to read it *)

val rv : string -> (_, _) dbus_reader
  (** [rv var] read any dbus value that we know how to read *)

(** {6 Reading rules} *)

type reader

val reader : ('a, 'b) dbus_reader -> caml_type -> reader
  (** [reader dbus_reader caml_type] create a rule for reading dbus
      value into caml value of type [caml_type] using [dbus_reader].
      The set of variables that appears in [dbus_reader] must be the
      same as the one who appears in [caml_type], in other case an
      [Invalid_argument] is raised *)

val default_readers : reader list
  (** Default reader rules *)

(** {6 Code generation} *)

val generate : reader list -> DBus.dbus_type -> caml_type -> caml_expr option

