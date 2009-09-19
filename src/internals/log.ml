(*
 * log.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


module type Logger =
sig
  val log : ('a, out_channel, unit) format -> 'a
  val error : ('a, out_channel, unit) format -> 'a
  val debug : ('a, out_channel, unit) format -> 'a
  val failure : exn -> ('a, unit, string, unit) format4 -> 'a
end

open Printf

let verbose_enable, debug_enable, dump_enable =
  let (x, y, z) =
    try
      match String.lowercase (Sys.getenv "OBUS_LOG") with
        | "dump" -> (true, true, true)
        | "debug" -> (true, true, false)
        | _ -> (true, false, false)
    with
        Not_found -> (false, false, false)
  in
  (ref x, ref y, ref z)

let program_name = Filename.basename Sys.argv.(0)

module Make_logger(Module : sig val section : string option end) =
struct
  open Module

  let print fmt = match section with
    | Some s -> eprintf ("%s: obus(%s): " ^^ fmt ^^ "\n%!") program_name s
    | None -> eprintf ("%s: obus: " ^^ fmt ^^ "\n%!") program_name

  let print_color bold color fmt =
    if Unix.isatty Unix.stderr then
      let bold = if bold then 1 else 0 in
      match section with
        | Some s -> eprintf ("\027[%d;%dm%s: obus(%s): " ^^ fmt ^^ "\027[0m\n%!") bold color program_name s
        | None -> eprintf ("\027[%d;%dm%s: obus: " ^^ fmt ^^ "\027[0m\n%!") bold color program_name
    else
      print fmt

  let red = 31

  let log fmt = match !verbose_enable with
    | true -> print fmt
    | false -> ifprintf stderr fmt

  let debug fmt = match !debug_enable with
    | true -> print fmt
    | false -> ifprintf stderr fmt

  let error fmt = print_color true red ("error: " ^^ fmt)

  let rec split str i =
    if i >= String.length str then
      []
    else
      let j = try String.index_from str i '\n' with _ -> String.length str in
      String.sub str i (j - i) :: split str (j + 1)

  let failure exn fmt = ksprintf begin fun msg ->
    let print fmt = print_color false red fmt in
    begin match msg with
      | "" -> print "failure: %s"
      | _ -> print "failure: %s: %s" msg
    end (Printexc.to_string exn);
    if Printexc.backtrace_status () then begin
      print "backtrace:";
      List.iter (print "  %s") (split (Printexc.get_backtrace ()) 0)
    end;
  end fmt
end

include Make_logger(struct let section = None end)
module Make(Module : sig val section : string end) =
  Make_logger(struct let section = Some Module.section end)
