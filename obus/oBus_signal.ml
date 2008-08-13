(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


type id

open Wire
open OBus_internals

type ('a, 'b, 'c) filter = ('a, 'b, 'c) filter

let wrap filter f = match filter with
  | Some desc ->
      Some { desc with
               filter_reader = (filter.filter_reader
                                >>= fun g -> return (fun h -> h (g f))) }
  | None -> None

(* Since we will probably always make union of signals coming from the
   same interface we can optimize this case *)
let rec get_same_interface interface acc = function
  | [(interface', members)] :: rest when interface = interface' ->
      get_same_interface interface (Member_set.union acc members) rest
  | [] -> Some acc
  | _ -> None

let rec slow_union acc = function
  | [] -> acc
  | sigs :: rest ->
      slow_union (List.fold_left
                    (fun acc (interface, members) ->
                       

let union filters = match Util.filter_map (fun x -> x) filters with
  | [] -> None
  | (desc :: rest) as l ->
      Some { desc with
               filter_signals =
          match desc.filter_signals with
            | [(interface, members)] -> begin match get_same_interface interface members rest with
                | Some members -> [(interface, members)]
                | None -> slow_union l
              end
            | _ -> slow_union [] l }
