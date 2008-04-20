(*
 * sig.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type name = string

type 'a param = Arg of name * 'a
type 'a definition =
  | Method of name * 'a param list * 'a param list
  | Signal of name * 'a param list
type 'a t = Sig of name * 'a definition list
type 'a tree = Tree of ('a t * 'a tree) list

let empty = Tree []

let regexp = Str.regexp "\\."

let extract name defs =
  match List.partition (fun (Sig(n, _), _) -> n = name) defs with
    | [v], l -> (Some(v), l)
    | [], l -> (None, l)
    | _ -> assert false

let add name defs (Tree(nodes)) =
  let rec aux nodes = function
    | [] -> raise (Invalid_argument "invalid interface name")
    | [name] -> begin match extract name nodes with
        | Some(_, t), l -> (Sig(name, defs), t) :: l
        | None, l -> (Sig(name, defs), Tree []) :: l
      end
    | name :: names -> begin match extract name nodes with
        | Some(s, Tree(sons)), l -> (s, Tree(aux sons names)) :: l
        | None, l -> (Sig(name, []), Tree(aux [] names)) :: l
      end
  in
    Tree(aux nodes (Str.split regexp name))

let rec merge (Tree(a)) (Tree(b)) =
  Tree begin List.fold_left begin fun acc ((Sig(nameb, defsb) as sigb, treeb) as nodeb) ->
    match extract nameb acc with
      | Some(siga, treea), l -> (begin match defsb with
                                   | [] -> siga
                                   | _ -> sigb
                                 end,
                                 merge treea treeb) :: l
      | None, l -> nodeb :: l
  end a b end
