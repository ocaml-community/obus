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

(** {6 Reading dbus marshaled value into caml value} *)

type reader
  (** Type of a dbus reader *)

val rint16 : reader
val rint32 : reader
val rint64 : reader
val ruint16 : reader
val ruint32 : reader
val ruint64 : reader
  (** Reader for the various dbus int types, these readers always
      return a caml int *)

val rint32_as_int32 : reader
val rint64_as_int64 : reader
val ruint32_as_int32 : reader
val ruint64_as_int64 : reader
  (** Same thing but return int32 and int64 caml value *)

val rsignature : reader
  (** Read a dbus signature into a OBus.dbus_type *)

val rsignature_as_string : reader
  (** Read a signature into a string representing the dbus type using
      dbus type codes *)

val rbyte : reader
  (** Read a dbus byte into a caml char *)

val rboolean : reader
  (** Read a dbus boolean into a caml bool *)

val rdouble : reader
  (** Read a dbus double into a caml float *)

val rstring : reader
  (** Read a dbus string into a caml string *)

val robject_path : reader
  (** Read a dbus object path into a caml string *)

val rarray : reader -> caml_expr -> caml_expr -> reader
  (** [rarray reader f x] correspond to a function that read a dbus
      array [|a1; a2; ...; an|] using [reader] and construct the caml
      value (f an (... (f a2 (f a1 x)) ...)) *)

val rdict : reader -> reader -> caml_expr -> caml_expr -> reader
  (** Same thing as rarray but for dbus dictionary *)

val rstruct : reader -> reader
  (** [rstruct seq] read a sequence of dbus value packed into a dbus
      struct *)

val rvariant : reader
  (** [rvariant] read a dbus variant into a caml OBus.dbus_value *)

val rfixed_variant : caml_expr -> (caml_patt * reader) list -> reader
  (** [rfixed_variant e branches] read a variant into a caml value
      according to the value of [e].

      For example in a dbus message header, the additionnal header
      fields are represented as byte * variant dbus value and the
      content of the variant depend on the value of the first byte, so
      we can use this reader :

      rseq [rbyte, <:patt< b >>; rfixed_variant <:expr< b >>
              [ <:patt< 0 >>, rbind rstring "x" <:expr< Sender(x) >>;
                <:patt< 1 >>, rbind ruint32 "x" <:expr< Serial(x) >>;
                ... ], <:patt< r >>] <:expr< r >>

      Note: as the content type of the variant is not known, readers
      have to be "concrete readers", so they must not contains any
      variable or rcaml. An invalid argument is raised in other
      case.
  *)

val rseq : (caml_patt * reader) list -> caml_expr -> reader
  (** [rseq readers expr] read a sequence of value using [readers],
      then evalutate [expr] *)

val rbind : reader -> caml_patt -> caml_expr -> reader
  (** [rbind reader var expr] read a dbus value with [reader], bind
      its result to [var] in [expr].

      For example if you want to convert an dbus uint32 into a caml
      type:

      type t = A | B

      just write something like that:

      rbind ruint32 <:patt< x >> <:expr< match x with
        | 0 -> A
        | 1 -> B >>
  *)

val rcaml : caml_type -> reader
  (** [rcaml caml_type] read a dbus value into a caml value of type
      [caml_type] if we already know how to read it *)

val rv : string -> reader
  (** [rv var] read any dbus value that we know how to read *)

(** {6 Reading rules} *)

type reading_rule

val make_reading_rule : reader -> caml_type -> reading_rule
  (** [make_reading_rule dbus_reader caml_type] create a rule for
      reading dbus value into caml value of type [caml_type] using
      [dbus_reader]. The set of variables that appears in
      [dbus_reader] must be the same as the one who appears in
      [caml_type], in other case an [Invalid_argument] is raised *)

val default_reading_rules : reading_rule list
  (** Default reading rules *)

(** {6 Writing caml value as dbus marshaled value} *)

type writer
  (** Type of a dbus writer *)

val wint16 : writer
val wint32 : writer
val wint64 : writer
val wuint16 : writer
val wuint32 : writer
val wuint64 : writer
  (** Writer for the various dbus int types, these writers always take
      a caml int *)

val wint32_from_int32 : writer
val wint64_from_int64 : writer
val wuint32_from_int32 : writer
val wuint64_from_int64 : writer
  (** Same thing but take int32 and int64 caml value *)

val wsignature : writer
  (** Write a dbus signature from a OBus.dbus_type *)

val wsignature_from_string : writer
  (** Write a signature from a string representing the dbus type using
      dbus type codes *)

val wbyte : writer
  (** Write a dbus byte from a caml char *)

val wboolean : writer
  (** Write a dbus boolean from a caml bool *)

val wdouble : writer
  (** Write a dbus double from a caml float *)

val wstring : writer
  (** Write a dbus string from a caml string *)

val wobject_path : writer
  (** Write a dbus object path from a caml string *)


val warray : writer -> caml_expr -> writer
  (** [warray expr writer fold] correspond to a function that write a dbus
      array using [fold] and [writer]. [fold] is act as
      List.fold_left *)

val wdict : writer -> writer -> caml_expr -> writer
  (** Same thing as warray but for dbus dictionary *)

val wstruct : writer -> writer
  (** [wstruct seq] write a sequence of dbus value *)

val wvariant : writer
  (** [wvariant] write a dbus variant from a caml OBus.dbus_value *)

val wfixed_variant : (caml_patt * caml_expr * caml_expr * writer) list -> writer -> writer
  (** [wfixed_variant e branches] read a variant from a caml value
      according to the value of [e].

      For example in a dbus message header, the additionnal header
      fields are represented as byte * variant dbus value and the
      content of the variant depend on the value of the first byte, so
      we can use this writer :

      dcons dbyte <:patt< b >> (dfixed_variant <:expr< b >>
        [ <:patt< 0 >>, dbind dstring "x" <:expr< Sender(x) >>;
          <:patt< 1 >>, dbind duint32 "x" <:expr< Serial(x) >>;
          ... ]

      Note: as the content type of the variant is not known, writers
      have to be "concrete writers", so they must not contains any
      variable or rcaml. An invalid argument is raised in other
      case.
  *)

val wcons : writer -> writer -> writer
  (** [wcons head var tail] read a single dbus value with [head], then
      bind its result to [var] and read others value with [tail] *)

(*val wnil : caml_expr -> sequence reader*)
  (** [wnil expr] Write the end of a sequence of value, for example
      the end of a structure *)

(*val wbind : ('a, 'b) dbus writer -> caml_patt -> caml_expr -> ('a, 'b) dbus writer*)
  (** [wbind writer var expr] read a dbus value with [writer], bind
      its result to [var] in [expr].

      For example if you want to convert an dbus uint32 from a caml
      type:

      type t = A | B

      just write something like that:

      dbind duint32 <:patt< x >> <:expr< match x with
                                           | 0 -> A
                                           | 1 -> B >>
  *)

val wconv : caml_expr -> writer -> writer
  (** [wconv expr writer] use [expr] to convert the value to write,
      then use [writer] to write it *)

val wcaml : caml_type -> writer
  (** [wcaml caml_type] read a dbus value from a caml value of type
      [caml_type] if we already know how to read it *)

val wv : string -> writer
  (** [wv var] read any dbus value that we know how to read *)

(** {6 Writing rules} *)

type writing_rule

val make_writing_rule : writer -> caml_type -> writing_rule
  (** [make_writing_rule dbus_writer caml_type] create a rule for
      writing dbus value from caml value of type [caml_type] using
      [dbus_writer]. The set of variables that appears in
      [dbus_writer] must be the same as the one who appears in
      [caml_type], in other case an [Invalid_argument] is raised *)

val default_writing_rules : writing_rule list
  (** Default writing rules *)

(** {6 Helpers} *)

val map_reading : string -> caml_type -> reading_rule
  (** [map_reading module_name key_type] create a rule for reading a map
      created with Map.Make. [module_name] is the name of the module,
      it is automatically capitalized. *)

val map_writing : string -> caml_type -> reading_rule
  (** Same as [map_reading] but for writing. *)

(** {6 Code generation} *)

val generate_reader : reading_rule list -> DBus.dbus_type -> caml_type -> caml_expr option
  (** [generate_reader rules dbus_type caml_type] generate a caml
      expression that read a dbus marshaled value of type [dbus_type]
      from a caml value of type [caml_type] *)

val generate_writer : writing_rule list -> caml_type -> DBus.dbus_type -> caml_expr option
  (** [generate_writer rules caml_type dbus_type] generate a caml
      expression that write a dbus marshaled value of type [dbus_type]
      from a caml value of type [caml_type] *)
