(*
 * cookie.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a content =
  | Waiting of Mutex.t
  | Value of 'a
  | Exn of exn

type 'a t = 'a ref

let raw_send_message_with_cookie connection header writer reader =
  let m = Mutex.create () in
  let cookie = ref (Waiting m) in
    Mutex.lock m;
    Connection.raw_send_message_async connection header writer begin fun header buf ptr ->
      cookie := begin try
        Value (reader header buf ptr)
      with
          e -> Exn e
      end;
      Mutex.unlock m
    end

let send_message_with_cookie connection (header, body) =
  raw_send_message_async_with_cookie connection header (Wire.write_value body) Wire.read_value

let rec get x = match !x with
  | Waiting m ->
      Mutex.lock m;
      Mutex.unlock m;
      get x
  | Value v -> v
  | Exn e -> raise e

let is_ready x = match !x with
  | Waiting _ -> false
  | _ -> false

let get_if_ready x = match !x with
  | Waiting _ -> None
  | Value v -> Some v
  | Exn e -> raise e
