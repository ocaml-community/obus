(*
 * rules.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type message_type =
  | Method_call
  | Method_return
  | Error
  | Signal

type rule =
  | Type of message_type
  | Sender of Connection.name
  | Interface of Interface.name
  | Member of string
  | Path of Path.t
  | Destination of Connection.name
  | Arg of int * string

type t = rule list

let to_string l =
  let buf = Buffer.create 42 in
  let rec aux first = function
    | [] -> Buffer.contents buf
    | rule :: rules ->
        if not first
        then Buffer.add_char buf ',';
        let typ, v = match rule with
          | Type(x) ->
              ("type",
               match x with
                 | Method_call -> "method_call"
                 | Method_return -> "method_return"
                 | Error -> "error"
                 | Signal -> "signal")
          | Sender(x) -> ("sender", x)
          | Interface(x) -> ("interface", x)
          | Member(x) -> ("member", x)
          | Path(x) -> ("path", x)
          | Destination(x) -> ("destination", x)
          | Arg(n, x) -> ("arg" ^ string_of_int n, x)
        in
          Buffer.add_string buf typ;
          Buffer.add_string buf "='";
          Buffer.add_string buf v;
          Buffer.add_char buf '\'';
          aux false rules
  in
    aux true l
