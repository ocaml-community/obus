(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of signals *)

type 'a t
  (** A signal definition. ['a] is the type of signals contents. *)

(** {6 Receiving signals} *)

type receiver
  (** Id of a function which receive signals *)

val connect : 'a t -> ?serial:bool -> ?args:(int * string) list -> ('a -> unit Lwt.t) -> receiver Lwt.t
  (** [connect signal ?serial ?args func] connect [func] to signals
      [signal].

      [args] is a pattern for string argument of signals. For example
      [(0, "a"); (2, "b")] will match signals for which argument 0 is
      [Basic(String "a")] and argument 2 is [Basic(String "b")].

      The filtering with [args] is also done by the message bus so
      this may reduce the number of wakup.

      If [serial] is [true] then calls to [func] are serialized. It
      default to [false]. *)

val disconnect : receiver -> unit
  (** Disable a receiver. Do nothing if the receiver is already
      disabled *)

(** Notes:

    - if the name of the peer is a unique name, then when the peer
    exit the receiver is automatically disabled. Especially if the
    name is not owned, it is supposed that the peer already exited

    - it is guaranted that when {!connect}, {!connect_any} or
    {!enable} returns, signals will be immediatly catched.

    - it is guaranted that when {!disable} returns, signals will be
    immediatly discared. *)


(** {6 Signal creation} *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping of signal contents *)

val make :
  ?broadcast:bool ->
  interface:OBus_name.interface ->
  member:OBus_name.member ->
  ('a, _) OBus_type.cl_sequence ->
  (unit -> OBus_proxy.t Lwt.t) -> 'a t
  (** [make ?broadcast interface member typ proxy] create a signal.

      [broadcast] tell weather the signal is broadcasted or not. It
      default to [true]. *)

val dmake :
  ?broadcast:bool ->
  interface:OBus_name.interface ->
  member:OBus_name.member ->
  (unit -> OBus_proxy.t Lwt.t) -> OBus_message.body t
  (** Same thing but the value returned are dynamically typed and
      there is no constraint on the signal type *)

