(*
 * log.mli
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Logging facility *)

val verbose_enable : bool ref
val debug_enable : bool ref
val dump_enable : bool ref
  (** Set from the environment variable OBUS_LOG, see [OBus_info] for
      explanation. *)

module type Logger =
sig
  val log : ('a, out_channel, unit) format -> 'a
    (** [log fmt] print something on [stderr] if [verbose_enable] is
        [true] *)

  val error : ('a, out_channel, unit) format -> 'a
    (** [error fmt] print an error message. If [stderr] is a tty, then
        it use color. *)

  val debug : ('a, out_channel, unit) format -> 'a
    (** [debug fmt] print a debug message if [debug_enable] is
        [true] *)

  val failure : exn -> ('a, unit, string, unit) format4 -> 'a
    (** [failure fmt exn] log a failure, and print backtrace on
        [stderr] if possible *)
end

(** Logger without section *)
include Logger

(** Logger with a section *)
module Make(Module : sig val section : string end) : Logger
