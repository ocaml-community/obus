(*
 * transport.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open ThreadImplem

type error =
  | Read_error
  | Write_error
  | Closed
exception Error of error * exn option
type backend =
  | Unix of Unix.file_descr
  | Unknown

type t = {
  backend : backend;
  recv : string -> int -> int -> int;
  send : string -> int -> int -> int;
  close : unit -> unit;
}

let fd transport = match transport.backend with
  | Unix fd -> fd
  | _ -> raise (Invalid_argument "this transport has not file descriptor")

type maker = Address.t -> t option

let makers = Protected.make []

let safe f x =
  try
    f x
  with
      e ->
        let ((name, _), _, _) = x in
          LOG("failure while trying to make a transport from %s: %s" name (Printexc.to_string e));
          None

let register_maker maker = Protected.update (fun l -> safe maker :: l) makers

let of_addresses addresses =
  match Util.find_map (Util.try_all (Protected.get makers)) addresses with
    | Some(transport) -> transport
    | None -> raise (Failure "no working address found")
