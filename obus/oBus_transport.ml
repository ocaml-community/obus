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
  | Socket of Lwt_unix.file_descr
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
  | Socket fd -> fd
  | _ -> raise (Invalid_argument "this transport has no file descriptor")

open OBus_address
open Unix

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  try_bind
    (fun _ -> Lwt_unix.connect fd addr)
    (fun _ ->
       return (make
                 ~backend:(Socket fd)
                 ~send:(Lwt_unix.write fd)
                 ~recv:(Lwt_unix.read fd)
                 ~close:(fun () ->
                           Lwt_unix.shutdown fd SHUTDOWN_ALL;
                           Lwt_unix.close fd;
                           return ())
                 ()))
    (fun exn -> Lwt_unix.close fd; fail exn)

let rec try_one t fallback x =
  catch (fun _ -> t)
    (fun exn -> fallback x)

let rec of_addresses = function
  | [] -> fail (Failure "no working address found")
  | (desc, _) :: rest -> match desc with
      | Unix path ->
          try_one (make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX(path)))
            of_addresses rest

      | Tcp(host, service, family) ->
          let opts = [AI_SOCKTYPE SOCK_STREAM] in
          let opts = match family with
            | Some Ipv4 -> AI_FAMILY PF_INET :: opts
            | Some Ipv6 -> AI_FAMILY PF_INET6 :: opts
            | None -> opts in
          let rec try_all = function
            | [] -> of_addresses rest
            | ai :: ais ->
                try_one (make_socket ai.ai_family ai.ai_socktype ai.ai_addr)
                  try_all ais
          in
          try_all (getaddrinfo host service opts)
      | _ -> of_addresses rest
