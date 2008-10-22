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

let call member connection = function
  | None -> return ()
  | Some m ->
      OBus_internals.lwt_with_bus
        (fun bus -> method_call bus
           ~destination:"org.freedesktop.DBus"
           ~path:["org"; "freedesktop"; "DBus"]
           ~interface:"org.freedesktop.DBus"
           ~member
           (<< match_rule -> unit >>)
           m) connection

let add = call "AddMatch"
let rem = call "RemoveMatch"

let enable_receiver (mr, bus, id) =
  match signal_receiver_enabled id with
    | true -> return ()
    | false -> enable_signal_receiver id; add bus mr

let disable_receiver (mr, bus, id) =
  match signal_receiver_enabled id with
    | false -> return ()
    | true -> disable_signal_receiver id; rem bus mr

let add_receiver bus ?(global=true) ?sender ?destination ?path ?interface ?member ?args typ func =
  let id = add_signal_receiver bus ?sender ?destination ?path ?interface ?member ?args typ func in
  let mr = match global with
    | true -> Some(Rules.to_string ~typ:`signal ?sender ?destination ?path ?interface ?member ?args ())
    | false -> None in
  add bus mr >>= (fun _ -> return (mr, bus, id))

let dadd_receiver bus ?(global=true) ?sender ?destination ?path ?interface ?member ?args func =
  let id = dadd_signal_receiver bus ?sender ?destination ?path ?interface ?member ?args func in
  let mr = match global with
    | true -> Some(Rules.to_string ~typ:`signal ?sender ?destination ?path ?interface ?member ?args ())
    | false -> None in
  add bus mr >>= (fun _ -> return (mr, bus, id))

let receiver_enabled (mr, bus, id) = signal_receiver_enabled id
