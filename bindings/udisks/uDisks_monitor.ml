(*
 * uDisks_monitor.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt_react
open Lwt

module String_map = Map.Make(String)

let changed interface =
  OBus_member.Signal.make
    ~interface
    ~member:"Changed"
    ~args:OBus_value.arg0
    ~annotations:[]

let monitor proxy interface switch =
  let%lwt event =
    OBus_signal.connect ~switch
      (OBus_signal.with_context
         (OBus_signal.make (changed interface) proxy))
  and context, dict = OBus_property.get_all_no_cache proxy interface in
  return (S.hold
            ~eq:(String_map.equal (=))
            (OBus_property.map_of_list context dict)
            (E.map_s
               (fun (context, ()) ->
                  let%lwt context, dict = OBus_property.get_all_no_cache proxy interface in
                  return (OBus_property.map_of_list context dict))
               event))
