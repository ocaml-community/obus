(*
 * types.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** {6 Representation and manipulation of dbus types} *)

type basic =
    [ `byte
    | `boolean
    | `int16
    | `int32
    | `int64
    | `uint16
    | `uint32
    | `uint64
    | `double
    | `string
    | `signature
    | `object_path ]

type t =
    [ basic
    | `array of t
    | `dict of basic * t
    | `structure of t list
    | `variant ]

type signature = string
    (** A DBus signature is a string representation of a list of DBus
        types *)

val to_signature : t list -> signature
val of_signature : signature -> t list
  (** Convertion between signature and types, [of_signature] raise an
      [Invalid_argument] if the signature is not a valid DBus
      signature. Valid signatures are described in the DBus
      specification. *)

val to_string : t -> string
  (** Print the type in caml style *)
