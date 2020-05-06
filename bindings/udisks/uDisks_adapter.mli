(*
 * uDisks_adapter.mli
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UDisks adapter interface *)

include OBus_proxy.Private

(** {6 Signals} *)

val changed : t -> unit OBus_signal.t

(** {6 Properties} *)

val fabric : t -> string OBus_property.r

val num_ports : t -> int OBus_property.r

val driver : t -> string OBus_property.r

val model : t -> string OBus_property.r

val vendor : t -> string OBus_property.r

val native_path : t -> string OBus_property.r

val properties : t -> OBus_property.group
