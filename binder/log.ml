(*
 * log.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf

let prog_name = Filename.basename Sys.argv.(0)
let at_bol = ref true

let eol_regexp = Str.regexp "\n"

let print_line line =
  if !at_bol
  then eprintf "%s: %s\n%!" prog_name line
  else begin
    eprintf "%s\n%!" line;
    at_bol := true
  end

let print fmt =
  Printf.ksprintf begin fun s ->
    match List.rev (Str.split eol_regexp s) with
      | [] -> ()
      | last :: lines ->
          List.iter print_line (List.rev lines);
          if s <> "" && s.[String.length s - 1] = '\n'
          then (print_line last; at_bol := true)
          else begin
            if !at_bol
            then eprintf "%s: %s" prog_name last
            else prerr_string last;
            at_bol := false
          end
  end fmt
