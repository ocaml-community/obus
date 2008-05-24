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

type 'a t = 'a ref

let raw_send_message_with_cookie connection header writer reader =
  let cookie = ref (Waiting (if_thread Mutex.create (fun () -> connection))) in
    Mutex.lock m;
    Connection.raw_send_message_async connection header writer begin fun header buf ptr ->
      let old = !cookie in
        cookie := begin try
          Value (reader header buf ptr)
        with
            e -> Exn e
        end;
        match v with
          | With_thread m -> Mutex.unlock m
          | Without_thread _ -> ()
    end

let send_message_with_cookie connection (header, body) =
  raw_send_message_async_with_cookie connection header (Connection.write_values body) Connection.read_values

let rec get x = match !x with
  | Waiting x ->
      begin match x with
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
