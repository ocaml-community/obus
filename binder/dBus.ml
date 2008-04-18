(*
 * dBus.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type typ =
  | Tbyte
  | Tboolean
  | Tint16
  | Tint32
  | Tint64
  | Tuint16
  | Tuint32
  | Tuint64
  | Tdouble
  | Tstring
  | Tsignature
  | Tobject_path
  | Tarray of typ
  | Tdict of typ * typ
  | Tstruct of typ list
  | Tvariant

let rec string_of_type = function
  | Tbyte -> "y"
  | Tboolean -> "b"
  | Tint16 -> "n"
  | Tuint16 -> "q"
  | Tint32 -> "i"
  | Tuint32 -> "u"
  | Tint64 -> "x"
  | Tuint64 -> "t"
  | Tdouble -> "d"
  | Tstring -> "s"
  | Tobject_path -> "o"
  | Tsignature -> "g"
  | Tarray(x) -> "a" ^ string_of_type x
  | Tdict(k, v) -> "a{" ^ string_of_type k ^ string_of_type v ^ "}"
  | Tstruct(l) -> "(" ^ List.fold_left (fun acc x -> string_of_type x ^ acc) "" l ^ ")"
  | Tvariant -> "v"

let type_of_string dtyp =
  let rec aux i = match dtyp.[i] with
    | 'y' -> (i + 1, true, Tbyte)
    | 'b' -> (i + 1, true, Tboolean)
    | 'n' -> (i + 1, true, Tint16)
    | 'q' -> (i + 1, true, Tuint16)
    | 'i' -> (i + 1, true, Tint32)
    | 'u' -> (i + 1, true, Tuint32)
    | 'x' -> (i + 1, true, Tint64)
    | 't' -> (i + 1, true, Tuint64)
    | 'd' -> (i + 1, true, Tdouble)
    | 's' -> (i + 1, true, Tstring)
    | 'o' -> (i + 1, true, Tobject_path)
    | 'g' -> (i + 1, true, Tsignature)
    | 'a' -> begin match dtyp.[i + 1] with
        | '{' ->
            let j, is_basic, tk = aux (i + 2) in
              if not is_basic
              then raise (Invalid_argument (Printf.sprintf "dict key type must be a basic type: %s" dtyp));
              let j, _, tv = aux j in
                if dtyp.[j] <> '}'
                then raise (Invalid_argument (Printf.sprintf "dict does not end with '}': %s" dtyp));
                (j + 1, false, Tdict(tk, tv))
        | _ -> let i, _, t = aux (i + 1) in
            (i, false, Tarray(t))
      end
    | '(' -> let i, tl = aux_until ')' (i + 1) in
        (i, false, Tstruct(tl))
    | 'v' -> (i + 1, false, Tvariant)
    | c -> raise (Invalid_argument (Printf.sprintf "invalid type code %c" c))
  and aux_until ch i = match dtyp.[i] with
    | c when c = ch -> (i + 1, [])
    | _ -> let i, _, t = aux i in
      let i, tl = aux_until ch i in
        (i, t :: tl)
  in
  let _, _, t = aux 0 in t


type name = string

type param = Arg of name * typ
type def =
  | Method of name * param list * param list
  | Signal of name * param list

type tree =
  | Node of (name * def list * tree) list

let regexp = Str.regexp "\\."

let rec add name x t =
  let rec aux (Node(interfs)) = function
    | [] -> raise (Invalid_argument "invalid interface name")
    | [name] -> Node(match List.partition (fun (n, _, _) -> n = name) interfs with
                       | [(name, defs, sons)], l -> (name, x, sons) :: l
                       | [], l -> (name, x, Node []) :: l
                       | _ -> assert false)
    | name :: names -> Node(match List.partition (fun (n, _, _) -> n = name) interfs with
                              | [(name, defs, sons)], l -> (name, defs, aux sons names) :: l
                              | [], l -> (name, [], aux (Node []) names) :: l
                              | _ -> assert false)
  in
    aux t (Str.split regexp name)

open Xparser

type direction = In | Out

let from_xml xml =
  let get_dir direction args = Util.filter_map
    (fun (dir, arg) -> if dir = direction then Some(arg) else None) args in
    parse (elt "node" p0
             (s2
                (any (elt "interface" (p1 "name")
                        (s1 (union
                               [elt "method" (p1 "name")
                                  (s1 (any (elt "arg" (p3 "name" "direction" "type")
                                              s0
                                              (fun name dir typ ->
                                                 (begin match dir with
                                                    | "in" -> In
                                                    | "out" -> Out
                                                    | _ -> raise Parse_failed
                                                  end, Arg(name, type_of_string typ))))))
                                  (fun name args -> Method(name, get_dir In args, get_dir Out args));
                                elt "signal" (p1 "name")
                                  (s1 (any (elt "arg" (p2 "name" "type")
                                              s0
                                              (fun name typ -> Arg(name, type_of_string typ)))))
                                  (fun name args -> Signal(name, args))]))
                        (fun name defs -> (name, defs))))
                (any (elt "node" (p1 "name")
                        s0
                        (fun x -> x))))
             (fun interfs _ -> interfs)) xml
