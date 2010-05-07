(*
 * nm_connection.mli
 * -----------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 *                 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** NetworkManager active connections *)

(** An active connection is a connection that is currently being used *)

include OBus_proxy.Private

(** The connection state *)
type state =
    [ `Unknown
        (** The active connection is in an unknown state. *)
    | `Activating
        (** The connection is activating. *)
    | `Activated
        (** The connection is activated. *) ]

(** {6 Signals} *)

val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t

(** {6 Properties} *)

val service_name : t -> string OBus_property.r
val connection : t -> Nm_settings.Connection.t OBus_property.r
val specific_object : t -> OBus_proxy.t OBus_property.r
val devices : t -> Nm_device.t list OBus_property.r
val state : t -> state OBus_property.r
val default : t -> bool OBus_property.r
val vpn : t -> bool OBus_property.r

type properties = {
  service_name : string;
  connection : Nm_settings.Connection.t;
  specific_object : OBus_proxy.t;
  devices : Nm_device.t list;
  state : state;
  default : bool;
  vpn : bool;
}

val properties : t -> properties OBus_property.r
