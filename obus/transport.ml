(*
 * transport.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open ThreadImplem

exception Error of string * exn option
type backend =
  | Unix of Unix.file_descr
  | Other
type recv = string -> int -> int -> int
type send = string -> int -> int -> int
type close = unit -> unit

type t = {
  backend : backend;
  recv : recv;
  send : send;
  close : close;
}

let make ~backend ~recv ~send ?close () = {
  backend = backend;
  recv = recv;
  send = send;
  close = match close with
    | None -> (fun _ -> ())
    | Some f -> f
}

let backend t = t.backend
let close t = t.close

open Printf

let recv { recv = recv } buffer ofs len =
  try
    recv buffer ofs len
  with
      exn -> raise (Error(sprintf "recv(%d)" len, Some exn))

let send { send = send } buffer ofs len =
  try
    send buffer ofs len
  with
      exn -> raise (Error(sprintf "send(%d)" len, Some exn))

let recv_exactly t buffer ofs len =
  let count = recv t buffer ofs len in
    if count <> len
    then raise (Error(sprintf "tried to receive %d, receive effectively %d" len count, None))

let send_exactly t buffer ofs len =
  let count = send t buffer ofs len in
    if count <> len
    then raise (Error(sprintf "tried to send %d, send effectively %d" len count, None))

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
