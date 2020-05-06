(*
 * uDisks_port.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UDisks port interface *)

include OBus_proxy.Private

(** {6 Signals} *)

val changed : t -> unit OBus_signal.t

(** {6 Properties} *)

val connector_type : t -> string OBus_property.r

val number : t -> int OBus_property.r

val parent : t -> UDisks_adapter.t OBus_property.r

val adapter : t -> UDisks_adapter.t OBus_property.r

val native_path : t -> string OBus_property.r

val properties : t -> OBus_property.group
