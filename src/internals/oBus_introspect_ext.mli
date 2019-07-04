(*
 * oBus_introspect_ext.mli
 * -----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** OBus extended introspection *)

(** By default, introspection documents do not convey semantical
    information, such as enumerations or flags. However it is possible
    to attach information to interfaces and members.

    This module implements an extended introspection format, which can
    be encoded into standard introspection documents by using
    annotations.
*)

(** {6 Annotations} *)

(** The following annotations are used to encode additional
    informations into D-Bus introspection documents *)

val obus_enum : string
  (** The [org.ocamlcore.forge.obus.Enum] annotation *)

val obus_flag : string
  (** The [org.ocamlcore.forge.obus.Flag] annotation *)

val obus_type : string
  (** The [org.ocamlcore.forge.obus.Type] annotation *)

val obus_itype : string
  (** The [org.ocamlcore.forge.obus.IType] annotation *)

val obus_otype : string
  (** The [org.ocamlcore.forge.obus.OType] annotation *)

(** {6 Extended types} *)

type basic =
    private
  | Byte
  | Boolean
  | Int16
  | Int32
  | Int64
  | Uint16
  | Uint32
  | Uint64
  | Double
  | String
  | Signature
  | Object_path
  | Unix_fd
  | Enum of OBus_value.T.basic * (OBus_value.V.basic * string) list
      (** An enumeration. The first argument is the real D-Bus type and
          the second is a list of [(constant, keyword)].

          For example:

          {[
            Enum(OBus_value.T.Uint32,
                 [(OBus_value.V.Uint32 1l, "ok");
                  (OBus_value.V.Uint32 2l, "fail")])
          ]}

          Note that the real D-Bus type must be {!OBus_value.T.Byte}
          or an integer type.
      *)
  | Flag of OBus_value.T.basic * (OBus_value.V.basic * string) list
      (** A flag. The first argument is the real type and the second
          is a list of [(bits, keyword)].

          For example:

          {[
            Flag(OBus_value.T.Uint32,
                 [(OBus_value.V.Uint32 0x01l, "flag1");
                  (OBus_value.V.Uint32 0x02l, "flag2");
                  (OBus_value.V.Uint32 0x04l, "flag3")])
          ]}

          Note that the real D-Bus type must be {!OBus_value.T.Byte}
          or an integer type.
      *)

type single =
  | Basic of basic
  | Structure of single list
  | Array of single
  | Dict of basic * single
  | Variant

type sequence = single list

(** {8 Constructors} *)

val byte : basic
val boolean : basic
val int16 : basic
val int32 : basic
val int64 : basic
val uint16 : basic
val uint32 : basic
val uint64 : basic
val double : basic
val string : basic
val signature : basic
val object_path : basic
val unix_fd : basic
val enum : OBus_value.T.basic -> (OBus_value.V.basic * string) list -> basic
val flag : OBus_value.T.basic -> (OBus_value.V.basic * string) list -> basic

val basic : basic -> single
val structure : single list -> single
val array : single -> single
val dict : basic -> single -> single
val variant : single

(** {6 Terms} *)

(** A term represent a type, where symbols have not been resolved. *)
type term =
    private
  | Term of string * term list
      (** A term. Arguments are
          - the symbol name, which is either the name of a D-Bus type
            or a user defined type
          - the arguments taken by the function associated to the
            symbol *)
  | Tuple of term list
      (** A list of terms, packed into a tuple. Tuples are always
          mapped to D-Bus structures. Moreover it is ensured that there
          is never a type of the form [Tuple[t]]. *)

val term : string -> term list -> term
  (** Construct a term *)

val tuple : term list -> term
  (** Construct a tuple. If the list is of length 1, the type itself
      is returned. *)

(** {6 Symbols} *)

(** Type of user-definable symbols *)
type symbol =
    private
  | Sym_enum of OBus_value.T.basic * (OBus_value.V.basic * string) list
  | Sym_flag of OBus_value.T.basic * (OBus_value.V.basic * string) list

val sym_enum : OBus_value.T.basic -> (OBus_value.V.basic * string) list -> symbol
  (** Create an enumeration *)

val sym_flag : OBus_value.T.basic -> (OBus_value.V.basic * string) list -> symbol
  (** Create a flag type *)

(** {6 Conversions} *)

(** {8 Stripping} *)

(** The following functions remove extension from types. *)

val strip_basic : basic -> OBus_value.T.basic
val strip_single : single -> OBus_value.T.single
val strip_sequence : sequence -> OBus_value.T.sequence

(** {8 Projections} *)

(** The following functions project standard D-Bus types into extended
    D-Bus types *)

val project_basic : OBus_value.T.basic -> basic
val project_single : OBus_value.T.single -> single
val project_sequence : OBus_value.T.sequence -> sequence

(** {8 Types to terms conversions} *)

(** The following functions returns the term associated to a standard
    D-Bus type *)

val term_of_basic : OBus_value.T.basic -> term
val term_of_single : OBus_value.T.single -> term
val term_of_sequence : OBus_value.T.sequence -> term

(** {8 Symbols resolution} *)

type env = (string * symbol) list
    (** An environment, mapping names to symbol *)

exception Resolve_error of string
  (** Exception raised when the resolution of symbols of a type
      fails. *)

val resolve : env -> term -> single
  (** [resolve env term] resolves symbols of [term] using [env], and
      returns the extended type it denotes. It raises {!Resolve_error}
      if a symbol of [term] is not found in [env]. *)

(** {6 Extended introspection ast} *)

type name = string

type annotation = name * string
type argument = name option * term

type access = OBus_introspect.access = Read | Write | Read_write

type member =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * term * access * annotation list

type interface = name * member list * (string * symbol) list * annotation list

(** {6 Encoding/decoding} *)

val encode : interface -> OBus_introspect.interface
  (** Encode the given interface into a standard one by using
      annotations *)

val decode : OBus_introspect.interface -> interface
  (** Decode the given standard interface into an extended one by
      decoding annotations *)
