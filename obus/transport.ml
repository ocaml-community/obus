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
  | _ -> raise (Invalid_argument "this transport has no file descriptor")

let make_transport (_, known, _) = match known with
  | Address.Unix path ->
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Unix.connect fd (Unix.ADDR_UNIX(path));
        Some(make
               ~backend:(Unix fd)
               ~send:(Unix.write fd)
               ~recv:(Unix.read fd)
               ~close:(fun () ->
                         Unix.shutdown fd Unix.SHUTDOWN_ALL;
                         Unix.close fd)
               ())
  | _ -> None

let of_addresses addresses =
  match Util.find_map make_transport addresses with
    | Some(transport) -> transport
    | None -> raise (Failure "no working address found")
