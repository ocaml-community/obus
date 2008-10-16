(*
 * oBus_context.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open OBus_connection
open OBus_message

type t = OBus_connection.t * method_call

let tt = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, ({ typ = `Method_call _ } as msg)) -> (connection, msg)
     | _ -> raise Cast_failure)
  (fun _ -> ())

let tconnection = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, _) -> connection
     | _ -> raise Cast_failure)
  (fun _ -> ())

let tconnection = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, _) -> connection
     | _ -> raise Cast_failure)
  (fun _ -> ())

let tmessage = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, ({ typ = `Method_call _ } as msg)) -> msg
     | _ -> raise Cast_failure)
  (fun _ -> ())

let tfield f = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, ({ typ = `Method_call _ } as msg)) -> f msg
     | _ -> raise Cast_failure)
  (fun _ -> ())

let tsender = tfield sender
let tdestination = tfield destination
let tpath = tfield path
let tinterface = tfield interface

let connection = fst
let message = snd
let sender (c, m) = sender m
let destination (c, m) = destination m
let path (c, m) = path m
let interface (c, m) = interface m

type connection = OBus_connection.t
type sender = OBus_name.connection option
type destination = OBus_name.connection option
type path = OBus_path.t
type interface = OBus_name.interface option
type message = OBus_message.method_call
