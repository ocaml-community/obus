(*
 * transport.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

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
let safe f x = try f x with _ -> None
let register_maker maker = Protected.update (fun l -> safe maker :: l) makers
let create address = Util.try_all (Protected.get makers) address
