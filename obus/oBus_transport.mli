(*
 * oBus_transport.mli
 * ------------------
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

(** {6 Errors} *)

exception Error of string * exn option
  (** A transport error contain an error message and the original
      exception if any. For example for unix transport this can be a
      [Unix_error]. *)

(** {6 Transport definition} *)

type t

type backend =
  | Socket of Lwt_unix.file_descr
  | Other

type recv = string -> int -> int -> int Lwt.t
      (** A receiving function. Must behave as [Lwt_unix.read fd]. *)

type send = string -> int -> int -> int Lwt.t
      (** A receiving function. Must behave as [Lwt_unix.write fd]. *)

type close = unit -> unit Lwt.t
      (** A function to shutdown a transport *)

val make : backend:backend -> recv:recv -> send:send -> ?close:close -> unit -> t
  (** [make backend recv send close ()] make a transport from the
      following functions *)

val backend : t -> backend
val recv : t -> recv
val send : t -> send
val close : t -> close
  (** Access to basic functions of a transport, note that in case of
      error an [Error] is raised instead of the original error. *)

val recv_exactly : t -> string -> int -> int -> unit Lwt.t
val send_exactly : t -> string -> int -> int -> unit Lwt.t
  (** Same as [recv] and [send] but try to send/receive exactly the
      given amount of bytes, and raise an [Error(_)] if they
      failed. *)

val fd : t -> Lwt_unix.file_descr
  (** [fd transport] return the file descriptor used by the
      transport. If the transport does not have a file descriptor then
      it raise an [Invalid_argument]. *)

(** {6 Creation} *)

val of_addresses : OBus_address.t list -> t Lwt.t
  (** [create addresses] try to make a working transport from a list
      of addresses. This only works for transport which OBus
      internally handles *)
