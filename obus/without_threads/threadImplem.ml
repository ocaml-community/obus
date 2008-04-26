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
  let safe_update f x = x := f !x
  let process f x = let y, z = f !x in x := z; y
  let safe_process = process
end

module ThreadConfig =
struct
  let use_threads = false
end
