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

type error = {
  name : name;
  make : message -> exn;
  cast : exn -> message option;
}

exception DBus of name * message

let ocaml = "org.ocamlcore.forge.obus.OCamlException"

let () =
  Printexc.register_printer
    (function
       | DBus(name, message) -> Some(Printf.sprintf "%s: %s" name message)
       | _ -> None)

(* List of all registered D-Bus errors *)
let errors = ref []

(* +-----------------------------------------------------------------+
   | Creation/casting                                                |
   +-----------------------------------------------------------------+ *)

let make name message =
  let rec loop = function
    | [] ->
        DBus(name, message)
    | error :: errors ->
        if error.name = name then
          error.make message
        else
          loop errors
  in
  loop !errors

let cast exn =
  let rec loop = function
    | [] ->
        (ocaml, Printexc.to_string exn)
    | error :: errors ->
        match error.cast exn with
          | Some message -> (error.name, message)
          | None -> loop errors
  in
  match exn with
    | DBus(name, message) -> (name, message)
    | _ -> loop !errors

let name exn =
  let rec loop = function
    | [] ->
        ocaml
    | error :: errors ->
        match error.cast exn with
          | Some message -> error.name
          | None -> loop errors
  in
  match exn with
    | DBus(name, message) -> name
    | _ -> loop !errors

(* +-----------------------------------------------------------------+
   | Registration                                                    |
   +-----------------------------------------------------------------+ *)

module type Error = sig
  exception E of string
  val name : name
end

module Register(Error : Error) =
struct
  let () =
    errors := {
      name = Error.name;
      make = (fun message -> Error.E message);
      cast = (function
                | Error.E message -> Some message
                | _ -> None);
    } :: !errors
end

(* +-----------------------------------------------------------------+
   | Well-known exceptions                                           |
   +-----------------------------------------------------------------+ *)

exception Failed of message
  [@@obus "org.freedesktop.DBus.Error.Failed"]

exception Invalid_args of message
  [@@obus "org.freedesktop.DBus.Error.InvalidArgs"]

exception Unknown_method of message
  [@@obus "org.freedesktop.DBus.Error.UnknownMethod"]

exception Unknown_object of message
  [@@obus "org.freedesktop.DBus.Error.UnknownObject"]

exception Unknown_interface of message
  [@@obus "org.freedesktop.DBus.Error.UnknownInterface"]

exception Unknown_property of message
  [@@obus "org.freedesktop.DBus.Error.UnknownProperty"]

exception Property_read_only of message
  [@@obus "org.freedesktop.DBus.Error.PropertyReadOnly"]

exception No_memory of message
  [@@obus "org.freedesktop.DBus.Error.NoMemory"]

exception No_reply of message
  [@@obus "org.freedesktop.DBus.Error.NoReply"]
