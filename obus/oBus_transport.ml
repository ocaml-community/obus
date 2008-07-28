(*
 * oBus_transport.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

exception Error of string * exn option
type backend =
  | Unix of Lwt_unix.file_descr
  | Other
type recv = string -> int -> int -> int Lwt.t
type send = string -> int -> int -> int Lwt.t
type close = unit -> unit Lwt.t

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
    | None -> (fun _ -> return ())
    | Some f -> f
}

let backend t = t.backend
let close t = t.close

open Printf

let recv { recv = recv } buffer ofs len =
  catch (fun _ -> recv buffer ofs len)
    (fun exn -> fail (Error(sprintf "recv(%d)" len, Some exn)))

let send { send = send } buffer ofs len =
  catch (fun _ -> send buffer ofs len)
    (fun exn -> fail (Error(sprintf "send(%d)" len, Some exn)))

let recv_exactly t buffer ofs len =
  recv t buffer ofs len
  >>= (fun count ->
         if count <> len
         then fail (Error(sprintf "tried to receive %d, receive effectively %d" len count, None))
         else return ())

let send_exactly t buffer ofs len =
  send t buffer ofs len
  >>= (fun count ->
         if count <> len
         then fail (Error(sprintf "tried to send %d, send effectively %d" len count, None))
         else return ())

let fd transport = match transport.backend with
  | Unix fd -> fd
  | _ -> raise (Invalid_argument "this transport has no file descriptor")

let make_transport (_, known, _) = match known with
  | OBus_address.Unix path ->
      Some(fun _ ->
             let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
               Lwt_unix.connect fd (Unix.ADDR_UNIX(path))
               >>= (fun _ ->
                      return (make
                                ~backend:(Unix fd)
                                ~send:(Lwt_unix.write fd)
                                ~recv:(Lwt_unix.read fd)
                                ~close:(fun () ->
                                          Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
                                          Lwt_unix.close fd;
                                          return ())
                                ())))
  | _ -> None

let of_addresses addresses =
  match Util.find_map make_transport addresses with
    | Some(f) -> f ()
    | None -> fail (Failure "no working address found")
