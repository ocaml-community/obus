(*
 * oBus_log.ml
 * -----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

module type Logger =
sig
  val log : ('a, out_channel, unit) format -> 'a
  val error : ('a, out_channel, unit) format -> 'a
  val debug : ('a, out_channel, unit) format -> 'a
  val failure : exn -> ('a, unit, string, unit) format4 -> 'a
end

let verbose_enable, debug_enable =
  let (x, y) =
    try
      match String.lowercase (Sys.getenv "OBUS_LOG") with
        | "dump" -> (true, true)
        | "debug" -> (true, true)
        | _ -> (true, false)
    with Not_found ->
      (false, false)
  in
  (ref x, ref y)

let program_name = Filename.basename Sys.argv.(0)

let logger = ref(
  fun mode lines ->
    let prefix, suffix =
      if mode = `VERBOSE || not (try Unix.isatty Unix.stderr with _ -> false) then
        ("", "")
      else
        match mode with
          | `ERROR ->
              ("\027[31m", "\027[0m")
          | `DEBUG ->
              ("\027[34m", "\027[0m")
          | `VERBOSE ->
              assert false
    in
    ignore (Lwt_io.atomic
              (fun oc ->
                 Lwt_util.iter_serial
                   (fun line -> Lwt_io.fprintlf oc "%s%s: %s%s" prefix program_name line suffix)
                   lines)
              Lwt_io.stderr)
)

let make_line section msg =
  match section with
    | Some section ->
        Printf.sprintf "obus(%s): %s" section msg
    | None ->
        Printf.sprintf "obus: %s" msg

let log ?section fmt =
  Printf.ksprintf (fun msg -> !logger `VERBOSE [make_line section msg]) fmt

let debug ?section fmt =
  Printf.ksprintf (fun msg -> !logger `DEBUG [make_line section msg]) fmt

let error ?section fmt =
  Printf.ksprintf (fun msg -> !logger `ERROR [make_line section ("error: " ^ msg)]) fmt

let rec split str i =
  if i >= String.length str then
    []
  else
    let j = try String.index_from str i '\n' with _ -> String.length str in
    String.sub str i (j - i) :: split str (j + 1)

let failure ?section exn fmt =
  Printf.ksprintf
    (fun msg ->
       !logger `ERROR
         (make_line section
            ((match msg with
                | "" -> "failure: "
                | _ -> "failure: " ^ msg ^ ": ") ^ Printexc.to_string exn) ::
            if Printexc.backtrace_status () then
              (make_line section "backtrace:") :: List.map (make_line section) (split (Printexc.get_backtrace ()) 0)
            else
              []))
    fmt
