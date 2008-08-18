(*
 * mSet.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

class ['a] node x set = object(self)
  val mutable prev = (set :> < set_next : 'a node option -> unit >)
  val mutable next = None
  val mutable enabled = false

  method data = x

  method set_prev p = prev <- p
  method set_next n = next <- n

  method iter f =
    (match enabled with
       | false -> ()
       | true -> f (self#data));
    match next with
      | Some n -> n#iter f
      | None -> ()

  method enabled = enabled

  method disable = match enabled with
    | false -> ()
    | true ->
        enabled <- false;
        prev#set_next next;
        match next with
          | Some n -> n#set_prev prev
          | None -> ()

  method enable = match enabled with
    | true -> ()
    | false ->
        enabled <- true;
        next <- set#first;
        set#set_next (Some (self :> 'a node));
        match next with
          | Some n -> n#set_prev (self :> < set_next : 'a node option -> unit >)
          | None -> ()

end and ['a] t = object(self)
  val mutable first : 'a node option = None
  method first = first
  method set_next n = first <- n

  method add x =
    let node = new node x (self :> 'a t) in
    node#enable;
    node

  method iter (f : 'a -> unit) = match first with
    | Some n -> n#iter f
    | None -> ()
end

let make _ = new t
let is_empty s = match s#first with
  | Some _ -> false
  | None -> true
let add s x = s#add x
let enabled n = n#enabled
let enable n = n#enable
let disable n = n#disable
let iter f n = n#iter f
