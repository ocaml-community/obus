(*
 * nm_monitor.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt_react
open Lwt
open OBus_value

module String_map = Map.Make(String)

let properties_changed interface =
  OBus_member.Signal.make
    ~interface
    ~member:"PropertiesChanged"
    ~args:(arg1 (Some "properties", C.dict C.string C.variant))
    ~annotations:[]

let monitor proxy interface switch =
  let%lwt event =
    OBus_signal.connect ~switch
      (OBus_signal.with_context
         (OBus_signal.make (properties_changed interface) proxy))
  and context, dict = OBus_property.get_all_no_cache proxy interface in
  return (S.fold_s ~eq:(String_map.equal (=))
            (fun map (context, updates) ->
               return (OBus_property.update_map context updates map))
            (OBus_property.map_of_list context dict)
            event)
