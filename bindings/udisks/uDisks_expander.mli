(*
 * uDisks_expander.mli
 * -------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UDisks expander interface *)

include OBus_proxy.Private

(** {6 Signals} *)

val changed : t -> unit OBus_signal.t

(** {6 Properties} *)

val native_path : t -> string OBus_property.r
val vendor : t -> string OBus_property.r
val model : t -> string OBus_property.r
val revision : t -> string OBus_property.r
val num_ports : t -> int OBus_property.r
val upstream_ports : t -> UDisks_port.t list OBus_property.r
val adapter : t -> UDisks_adapter.t OBus_property.r

type properties = {
  native_path : string;
  vendor : string;
  model : string;
  revision : string;
  num_ports : int;
  upstream_ports : UDisks_port.t list;
  adapter : UDisks_adapter.t;
}

val properties : t -> properties OBus_property.r
