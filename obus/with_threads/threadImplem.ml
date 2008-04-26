(*
 * threadImplem.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

module Thread = Thread
module Mutex = Mutex

module Protected =
struct
  type 'a t = {
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

  let process f value =
    Mutex.lock value.protected_mutex;
    let (result, process) = f value.protected_content in
      value.protected_content <- process;
      Mutex.unlock value.protected_mutex;
      result

  let safe_process f value =
    try
      process f value
    with
        e ->
          Mutex.unlock value.protected_mutex;
          raise e

  let update f value =
    Mutex.lock value.protected_mutex;
    value.protected_content <- f value.protected_content;
    Mutex.unlock value.protected_mutex

  let safe_update f value =
    try
      update f value
    with
        e ->
          Mutex.unlock value.protected_mutex;
          raise e
end

module ThreadConfig =
struct
  let use_threads = true
end
