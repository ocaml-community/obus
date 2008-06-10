(*
 * threadImplem.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

module Thread = Thread
module Mutex = Mutex

let with_lock m f =
  Mutex.lock m;
  try
    let result = f () in
      Mutex.unlock m;
      result
  with
      e ->
        Mutex.unlock m;
        raise e

module Protected =
struct
  type 'a t = {
    mutable content : 'a;
    mutex : Mutex.t;
  }

  let make x =
    { content = x;
      mutex = Mutex.create () }

  let get value =
    value.content

  let set value v =
    Mutex.lock value.mutex;
    value.content <- v;
    Mutex.unlock value.mutex

  let process f value =
    with_lock value.mutex
      (fun () ->
         let (result, process) = f value.content in
           value.content <- process;
           result)

  let update f value =
    with_lock value.mutex
      (fun () -> value.content <- f value.content)

  let with_value f value =
    with_lock value.mutex
      (fun () -> f value.content)

  let if_none value f = match value.content with
    | Some v -> v
    | None ->
        with_lock value.mutex
          (fun () ->
             match value.content with
               | None ->
                   let v = f () in
                     value.content <- Some v;
                     v
               | Some v -> v)
end

module ThreadConfig =
struct
  let use_threads = true
end
