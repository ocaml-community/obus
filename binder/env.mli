(*
 * env.mli
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t
  (** Represent an environment for a marshaling/unmarshaling
      function.

      For a marshaling function it is the list of identifier binded to
      the values to write.

      For a unmarshaling functions it is the list of identifier binded
      to already readed values, which will be part of the result of
      unmarshaling *)

val empty : t
  (** [empty] empty environment *)

val init : int -> t
  (** [init n] return an environment with exactly [n] identifier *)

module type S = sig
  val size : t -> int
    (** [size env] current size of the environment *)

  val add : int -> t -> t
    (** [add n env] add [n] new identifier to [env]. [n] can be
        negative, in this case identifiers will be removed. *)

  val nth : int -> t -> Types.ident
    (** [nth n env] return the [n]th last identifier of the
        environment *)

  val last : t -> Types.ident
    (** [last env] equivalent to [nth 0 env] *)

  val lasts : int -> t -> Types.ident list
    (** [lasts n env] return the lasts [n] identifier of the
        environment *)

  val slice : int -> int -> t -> Types.ident list
    (** [slice n m env] return the lasts [m] identifier starting from
        [n] of the environment *)

  val all : t -> Types.ident list
    (** [all env] return all identifier in the environment *)
end

(** Operations on value varialbes *)
include S

(** Operations on type variables *)
module Type : S

(** Operations on pointer variables *)
module Index : S

(** Operation on length variables *)
module Length : S
