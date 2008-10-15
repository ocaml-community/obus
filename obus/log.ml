(*
 * log.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf

let verbose_enable, debug_enable, dump_enable =
  try
    match String.lowercase (Sys.getenv "OBUS_LOG") with
      | "dump" -> (true, true, true)
      | "debug" -> (true, true, false)
      | _ -> (true, false, false)
  with
      Not_found -> (false, false, false)

let program_name = Filename.basename Sys.argv.(0)

let section_opt = function
  | Some s -> "(" ^ s ^ ")"
  | None -> ""

let log fmt = match verbose_enable with
  | true -> eprintf ("%s: obus: " ^^ fmt ^^ "\n%!") program_name
  | false -> ifprintf stderr fmt

let debug fmt = match debug_enable with
  | true -> eprintf ("%s: obus: " ^^ fmt ^^ "\n%!") program_name
  | false -> ifprintf stderr fmt

let error fmt =
  if Unix.isatty Unix.stdout then
    eprintf ("\027[1;31m%s: obus: " ^^ fmt ^^ "\n%!\027[0m") program_name
  else
    eprintf ("%s: obus: error: " ^^ fmt ^^ "\n%!") program_name

module Make(Module : sig val section : string end) =
struct
  open Module

  let log fmt = match verbose_enable with
    | true -> eprintf ("%s: obus(%s): " ^^ fmt ^^ "\n%!") program_name section
    | false -> ifprintf stderr fmt

  let debug fmt = match debug_enable with
    | true -> eprintf ("%s: obus(%s): " ^^ fmt ^^ "\n%!") program_name section
    | false -> ifprintf stderr fmt

  let error fmt =
    if Unix.isatty Unix.stdout then
      eprintf ("\027[1;31m%s: obus(%s): " ^^ fmt ^^ "\n%!\027[0m") program_name section
    else
      eprintf ("%s: obus(%s): error: " ^^ fmt ^^ "\n%!") program_name section
end
