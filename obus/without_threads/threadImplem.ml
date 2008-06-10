(*
 * threadImplem.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

module Thread =
struct
  type t = unit
  let create _ _ = ()
  let self _ =  ()
  let id _ = 0
end

module Mutex =
struct
  type t = unit
  let create _ = ()
  let lock _ = ()
  let try_lock _ = true
  let unlock _ = ()
end

module Protected =
struct
  type 'a t = 'a ref

  let make x = ref x
  let get = ( ! )
  let set = ( := )
  let update f x = x := f !x
  let process f x = let y, z = f !x in x := z; y
  let with_value f x = f !x
  let if_none x f = match !x with
    | Some v -> v
    | None -> let v = f () in x := Some v; v
end

module ThreadConfig =
struct
  let use_threads = false
end

let with_lock _ f = f ()
