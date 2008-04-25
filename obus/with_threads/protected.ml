(*
 * protected.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a protected = {
  mutable protected_content : 'a;
  protected_mutex : Mutex.t;
}

let make x =
  { protected_content = x;
    protected_mutex = Mutex.create () }

let get value =
  Mutex.lock value.protected_mutex;
  let result = value.protected_content in
    Mutex.unlock value.protected_mutex;
    result

let set value v =
  Mutex.lock value.protected_mutex;
  value.protected_content <- v;
  Mutex.unlock value.protected_mutex

let process value f =
  Mutex.lock value.protected_mutex;
  let (result, process) = f value.protected_content in
    value.protected_content <- process;
    Mutex.unlock value.protected_mutex;
    result

let safe_process value f =
  try
    process value f
  with
      e ->
        Mutex.unlock value.protected_mutex;
        raise e

let update value f =
  Mutex.lock value.protected_mutex;
  value.protected_content <- f value.protected_content;
  Mutex.unlock value.protected_mutex

let safe_update value f =
  try
    update value f
  with
      e ->
        Mutex.unlock value.protected_mutex;
        raise e
