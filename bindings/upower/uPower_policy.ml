(*
 * uPower_policy.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_pervasives

module Proxy = OBus_proxy.Make(struct
                                 type proxy = UPower.t
                                 let cast peer = OBus_proxy.make (UPower.to_peer peer) ["org"; "freedesktop"; "UPower"; "Policy"]
                                 let make proxy = UPower.of_peer (OBus_proxy.peer proxy)
                               end)

type cookie = uint with obus

type latency = [ `Cpu_dma | `Network ]

let obus_latency = OBus_type.map obus_string
  (function
     | "cpu_dma" -> `Cpu_dma
     | "network" -> `Network
     | latency -> raise (OBus_type.Cast_failure("UPower_policy.obus_latency", Printf.sprintf "unknown latency type (%S)" latency)))
  (function
     | `Cpu_dma -> "cpu_dma"
     | `Network -> "network")

type latency_request = {
  lr_cookie : cookie;
  lr_uid : uint;
  lr_pid : uint;
  lr_exec : string;
  lr_timespec : int64;
  lr_persistent : bool;
  lr_typ : latency;
  lr_reserved : string;
  lr_value : int;
} with obus

let op_interface = Proxy.make_interface "org.freedesktop.UPower.QoS"

OP_method GetLatencyRequests : latency_request structure list
OP_method GetLatency : latency : latency -> int
OP_method CancelRequest : latency : latency -> cookie : cookie -> unit
OP_method RequestLatency : latency : latency -> value : int -> persistent : bool -> cookie
OP_method SetMinimumLatency : latency : latency -> value : int -> unit

OP_signal RequestsChanged : unit
OP_signal LatencyChanged : (latency * bool)
