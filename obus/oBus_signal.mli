(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of signals *)

type receiver
  (** Function which receive signals *)

val add_receiver : OBus_connection.t ->
  ?global:bool ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection_unique.t ->
  ?path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  ?member:OBus_name.Member.t ->
  ?args:(int * string) list ->
  ('a, unit, unit) OBus_type.ty_function -> 'a -> receiver Lwt.t
  (** [add_receiver connection sender destination path interface
      member typ func]

      Add a receiver on the given connection. If it is a message bus,
      then a matching rule is automatically added.

      [sender], [destination], [path], [interface] and [member],
      [args] and [typ] act as filters.

      [global] tell weather the signal is a ``global'' signal,
      i.e. destined to anyone who would want to get it, or if it is
      especially destined to us. This is basically to avoid to receive
      signal destined to other applications. This is basically of
      equivalent of passing our connection unqiue name as
      [destination] filter. The default value is [true].  *)

val dadd_receiver : OBus_connection.t ->
  ?global:bool ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection_unique.t ->
  ?path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  ?member:OBus_name.Member.t ->
  ?args:(int * string) list ->
  (OBus_message.body -> unit) -> receiver Lwt.t
  (** Same thing but the callback function receive a dynamically typed
      value, and there is no constraint on the message signature *)

val disable_receiver : receiver -> unit Lwt.t
  (** Disable a receiver. Do nothing if the receiver is already
      disabled *)

val enable_receiver : receiver -> unit Lwt.t
  (** Enable a receiver. Do nothing if the receiver is already
      enabled *)

val receiver_enabled : receiver -> bool
