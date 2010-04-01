(*
 * oBus_uuid.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus universally-unique IDs *)

(** D-Bus uuid are used to distinguish message buses, addresses, and
    machines.

    Note that they are not compatible with RFC4122. *)

type t

val generate : unit -> t
  (** Generate a new uuid *)

val of_string : string -> t
  (** Create a uuid from a string. The string must contain an
      hex-encoded uuid, i.e. be of length 32 and only contain
      hexadecimal characters. It raise a failure otherwise.

      @raise Invalid_argument if the string does not contain a valid
      uuid. *)

val to_string : t -> string
  (** Return a hex-encoded string representation of an uuid. *)
