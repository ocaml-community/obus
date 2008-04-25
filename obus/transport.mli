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

type backend =
  | Unix of Unix.file_descr
  | Unknown

type t = {
  backend : backend;

  recv : string -> int -> int -> unit;
  (** [recv buffer pos count] must receive exactly [count] bytes and
      store it in [buffer] starting from [pos]. *)

  send : string -> int -> int -> unit;
  (** [send buffer pos count] must send exactly [count] bytes from
      [buffer] starting at [pos]. *)

  close : unit -> unit;
  (** [close ()] shutdown the transport *)

  lexbuf : unit -> Lexing.lexbuf;
  (** [lexbuf ()] return a lexing buffer, used by authentification *)

(** If something wrong appened, [Error.Error] must be raised *)
}

val unix_like : backend -> (string -> int -> int -> int) -> (string -> int -> int -> int) -> (unit -> unit) -> t
  (** [unix_like backend read write close] create a transport from two
      function [read] and [write] which behave as [Unix.read] and
      [Unix.write] *)

val fd : t -> Unix.file_descr
  (** [fd transport] return the file descriptor used by the transport,
      usefull for doing a select for example. If the transport does
      not a file descriptor then it raise an [Invalid_argument] *)

(** {6 Creation} *)

val create : Address.t -> t option
  (** [create addresses] try to make a working transport from an
      address *)

type maker = Address.t -> t option
  (** A maker is a function which take an address and create a
      transport from it. *)

val register_maker : maker -> unit
  (** [regsiter_maker maker] add a transport maker *)
