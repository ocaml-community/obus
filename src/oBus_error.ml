(*
 * oBus_error.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type name = string
type message = string
exception DBus of exn * name * message

let () =
  Printexc.register_printer
    (function
       | DBus(key, name, message) -> Some(Printf.sprintf "%s: %s" name message)
       | _ -> None)

module Exn_map = Map.Make(struct
                            type t = exn
                            let compare = compare
                          end)

module Name_map = Map.Make(String)

let exn_to_name = ref Exn_map.empty
let name_to_exn = ref Name_map.empty

let register ~name ~exn =
  exn_to_name := Exn_map.add exn name !exn_to_name;
  name_to_exn := Name_map.add name exn !name_to_exn

exception Unknown
exception OCaml

exception Failed
exception Invalid_args
exception Unknown_method
exception No_memory
exception No_reply

let () =
  List.iter (fun (name, exn) -> register name exn) [
    ("org.freedesktop.DBus.Error.Failed", Failed);
    ("org.freedesktop.DBus.Error.InvalidArgs", Invalid_args);
    ("org.freedesktop.DBus.Error.UnknownMethod", Unknown_method);
    ("org.freedesktop.DBus.Error.NoMemory", No_memory);
    ("org.freedesktop.DBus.Error.NoReply", No_reply);
    ("org.ocamlcore.forge.obus.OCamlException", OCaml);
  ]

let make exn message = DBus(exn, Exn_map.find exn !exn_to_name, message)

let make_by_name name message =
  let exn =
    try
      Name_map.find name !name_to_exn
    with Not_found ->
      Unknown
  in
  DBus(exn, name, message)

let raise exn message = raise (make exn message)
let fail exn message = Lwt.fail (make exn message)

let name_of_exn exn = Exn_map.find exn !exn_to_name
let exn_of_name name = Name_map.find name !name_to_exn
