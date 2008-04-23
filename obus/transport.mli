(*
 * transport.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus transport *)

(** {6 Errors handling} *)

type error =
  | Read_error
  | Write_error
  | Closed

exception Error of error * exn option
  (** An transport error contain the error type and the original
      exception if any. For example for unix transport this can be a
      [Unix_error]. *)

(** {6 Transport definition} *)

class type t = object
  method backend : [> ]
    (** transport backend *)

  method recv : string -> int -> int -> unit
    (** [recv buffer pos count] must receive exactly [count] bytes
        and store it in [buffer] starting from [pos]. *)

  method send : string -> int -> int -> unit
    (** [send buffer pos count] must send exactly [count] bytes from
        [buffer] starting at [pos]. *)

  method close : unit
    (** [close] shutdown the transport *)

(** If something wrong appened, [Error] must be raised *)
end

class virtual unix_like : object
  (** Transport with read and write which same behaviour of Unix read
      and write function *)
  inherit t
  method virtual backend : [> ]
  method virtual read : string -> int -> int -> int
  method virtual write : string -> int -> int -> int
end

(** {6 Creation} *)

val create : ([> Address.t ] as 'a) list -> (t * 'a list) option
  (** [create addresses] try to make a working transport from a list
      of addresses. It return also the list of untested addresses. *)

type maker = [> Address.t ] -> t option
  (** A maker is a function which take an address and create a
      transport from it. *)

val register_maker : maker -> unit
  (** [regsiter_maker maker] add a transport maker *)
