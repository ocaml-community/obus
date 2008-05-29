(*
 * transport.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus transport *)

(** A transport is basically a pair of functions for sending and
    receiving data. OBus implement by default unix transport, with
    communication over socket and know how to create it according to
    DBus addresses, but you can define your own transport and let OBus
    use it *)

(** {6 Errors handling} *)

type error =
  | Read_error
  | Write_error
  | Closed

exception Error of error * exn option
  (** A transport error contain the error type and the original
      exception if any. For example for unix transport this can be a
      [Unix_error]. *)

(** {6 Transport definition} *)

type backend =
  | Unix of Unix.file_descr
  | Unknown

type t = {
  backend : backend;

  recv : string -> int -> int -> int;
  (** [recv buffer pos count] must behave as [Unix.read fd]. *)

  send : string -> int -> int -> int;
  (** [send buffer pos count] must behave as [Unix.write fd]. *)

  close : unit -> unit;
  (** [close ()] shutdown the transport *)
}
    (** If something wrong appened, [Error.Error] must be raised *)

val fd : t -> Unix.file_descr
  (** [fd transport] return the file descriptor used by the transport,
      usefull for doing a select for example. If the transport does
      not a file descriptor then it raise an [Invalid_argument]. *)

(** {6 Creation} *)

val of_addresses : Address.t list -> t
  (** [create addresses] try to make a working transport from a list
      of addresses. *)

type maker = Address.t -> t option
  (** A maker is a function which take an address and create a
      transport from it. *)

val register_maker : maker -> unit
  (** [regsiter_maker maker] add a transport maker *)
