(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_connection
open OBus_type

OBUS_type match_rule = string

type receiver = match_rule option * OBus_connection.t * signal_receiver

let call member bus mr =
  match mr with
    | Some x ->
        OBus_internals.lwt_with_bus bus
          (fun _ -> method_call bus
             ~destination:"org.freedesktop.DBus"
             ~path:"/org/freedesktop/DBus"
             ~interface:"org.freedesktop.DBus"
             ~member
             (<< match_rule -> unit >>)
             x)
    | None -> return ()

let add = call "AddMatch"
let rem = call "RemoveMatch"

let enable (mr, bus, id) =
  match signal_receiver_enabled id with
    | true -> return ()
    | false -> enable_signal_receiver id; add bus mr

let disable (mr, bus, id) =
  match signal_receiver_enabled id with
    | false -> return ()
    | true -> disable_signal_receiver id; rem bus mr

let add_receiver bus ?(no_match_rule=false) ?sender ?path ?interface ?member typ func =
  let id = add_signal_receiver bus ?sender ?path ?interface ?member typ func in
  match no_match_rule with
    | false ->
        let mr = Util.match_rule ~typ:`signal ?sender ?path ?interface ?member () in
        add bus (Some mr) >>= (fun _ -> return (Some mr, bus, id))
    | true ->
        return (None, bus, id)

let dadd_receiver bus ?(no_match_rule=false) ?sender ?path ?interface ?member func =
  let id = dadd_signal_receiver bus ?sender ?path ?interface ?member func in
  match no_match_rule with
    | false ->
        let mr = Util.match_rule ~typ:`signal ?sender ?path ?interface ?member () in
        add bus (Some mr) >>= (fun _ -> return (Some mr, bus, id))
    | true ->
        return (None, bus, id)

let receiver_enabled (mr, bus, id) = signal_receiver_enabled id
