(*
 * seq.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ('a, 'b, 'c, 'd) t = 'a list * (('a -> 'b) -> 'c -> 'd)

let nil = ([], fun _ x -> x)
let cons x (l, f) = (x :: l, fun m g -> f m (g (m x)))
let apply f (l, g) = g (fun x -> x) f
let concat (la, fa) (lb, fb) = (la @ lb, fun m f -> fb m (fa m f))
let hd (l, _) = List.hd l
let tl (l, f) = match l with
  | e :: l -> (l, (fun m g -> f m (fun _ -> g)))
  | _ -> assert false (* never append *)
let to_list = fst
let map m (l, f) = (List.map m l,
                    (fun m' -> f (fun x -> m' (m x))))
