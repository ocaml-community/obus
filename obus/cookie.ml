(*
 * cookie.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open ThreadImplem

type 'a content =
  | Waiting of (Mutex.t, Connection.t) if_thread
  | Value of 'a
  | Exn of exn

type 'a t = 'a content ref

let raw_send_message_with_cookie connection header writer reader =
  let w = if_thread
    (fun () ->
       let m = Mutex.create () in
         Mutex.lock m;
         m)
    (fun () -> connection) in
  let cookie = ref (Waiting w) in
    Connection.raw_send_message_async connection header writer begin fun header buf ptr ->
      cookie := begin try
        Value (reader header buf ptr)
      with
          e -> Exn e
      end;
      match w with
        | With_thread m -> Mutex.unlock m
        | Without_thread _ -> ()
    end;
    cookie

let send_message_with_cookie connection ?(raise_exn=true) (header, body) =
  raw_send_message_with_cookie connection header
    (Connection.write_values body header.Header.byte_order)
    (fun header buf ptr ->
       let values = Connection.read_values raise_exn header buf ptr in
         (header, values))

let rec get x = match !x with
  | Waiting w ->
      begin match w with
        | With_thread m ->
            Mutex.lock m;
            Mutex.unlock m
        | Without_thread c ->
            Connection.dispatch c
      end;
      get x
  | Value v -> v
  | Exn e -> raise e

let is_ready x = match !x with
  | Waiting _ -> false
  | _ -> true

let get_if_ready x = match !x with
  | Waiting _ -> None
  | Value v -> Some v
  | Exn e -> raise e
