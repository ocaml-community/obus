(*
 * env.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t = {
  value : int;
  index : int;
  length : int;
  typ : int;
}

module type S = sig
  val size : t -> int
  val add : int -> t -> t
  val nth : int -> t -> Types.ident
  val last : t -> Types.ident
  val lasts : int -> t -> Types.ident list
  val slice : int -> int -> t -> Types.ident list
  val all : t -> Types.ident list
end

let init n = {
  value = n;
  index = 0;
  length = 0;
  typ = 0;
}
let empty = init 0

module Make(M : sig val prefix : string val get : t -> int val set : int -> t -> t end) =
struct
  open M
  let var_id n =
    if n >= 0
    then (<:ident< $lid:prefix ^ string_of_int n$ >>)
    else (<:ident< $lid:prefix ^ "_" ^ string_of_int (-n)$ >>)
  let var_ids n count = List.map var_id (Util.gen_list (fun x -> x) n count)
  let size env = get env
  let add n env = set (get env + n) env
  let nth n env = var_id (get env - 1 - n)
  let last = nth 0
  let lasts n env = var_ids (get env - n) n
  let slice n m env = var_ids (get env - n) m
  let all env = var_ids 0 (get env)
end

module Val = Make(struct let prefix = "v" let get env = env.value let set x env = { env with value = x } end)
module Type = Make(struct let prefix = "typ" let get env = env.typ let set x env = { env with typ = x } end)
module Index = Make(struct let prefix = "i" let get env = env.index let set x env = { env with index = x } end)
module Length = Make(struct let prefix = "len" let get env = env.length let set x env = { env with length = x } end)

include Val
