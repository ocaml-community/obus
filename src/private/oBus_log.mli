(*
 * oBus_log.mli
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Logging facility *)

val verbose_enable : bool ref
val debug_enable : bool ref
  (** Set from the environment variable OBUS_LOG, see [OBus_info] for
      explanation. *)

val logger : ([ `VERBOSE | `DEBUG | `ERROR ] -> string list -> unit) ref
  (** The logger used to really log messages. The default one prints
      things on [stderr]. *)

val log : ?section : string -> ('a, unit, string, unit) format4 -> 'a
  (** [log ?section fmt] log some message *)

val debug : ?section : string -> ('a, unit, string, unit) format4 -> 'a
  (** [log ?section fmt] log some debug message *)

val error : ?section : string -> ('a, unit, string, unit) format4 -> 'a
  (** [error ?section fmt] print an error message. If [stderr] is a
      tty, then it use color. *)

val failure : ?section : string -> exn -> ('a, unit, string, unit) format4 -> 'a
  (** [failure ?section fmt exn] log a failure, and print backtrace on
      [stderr] if possible *)
