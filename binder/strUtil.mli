(*
 * strUtil.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Various usefull function on string *)

val split_upper : string -> string list
  (** Apply the following transformation:

      "SetCPUFreqGovernor" -> ["set"; "cpufreq"; "governor"] *)

val dot_regexp : Str.regexp
val newline_regexp : Str.regexp
val split_dot : string -> string list
val split_lines : string -> string list
  (** Splitting often used *)

val gen_names : string -> 'a list -> string list
  (** [gen_names prefix l] generate a list of names of the same length
      of [l], with prefix [prefix] *)

val camlize_lid : string -> string
  (** Apply the following transformation:

      "SetCPUFreqGovernor" -> "set_cpufreq_governor" *)

val camlize_uid : string -> string
  (** Apply the following transformation:

      "SetCPUFreqGovernor" -> "Set_cpufreq_governor" *)
