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
  recv : string -> int -> int -> unit;
  send : string -> int -> int -> unit;
  close : unit -> unit;
  lexbuf : unit -> Lexing.lexbuf;
}

let unix_like backend read write close =
  { backend = backend;
    recv = (fun buf pos count -> assert (read buf pos count = count));
    send = (fun buf pos count -> assert (read buf pos count = count));
    close = close;
    lexbuf = (fun () -> Lexing.from_function (fun buf count -> read buf 0 count)) }

let fd transport = match transport.backend with
  | Unix fd -> fd
  | _ -> raise (Invalid_argument "this transport has not file descriptor")

type maker = Address.t -> t option

let makers = Protected.make []
let safe f x = try f x with _ -> None
let register_maker maker = Protected.update (fun l -> safe maker :: l) makers
let create address = Util.try_all (Protected.get makers) address
