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

val connect : OBus_proxy.t -> 'a t -> ?args:(int * string) list -> ('a -> unit) -> receiver Lwt.t
  (** [connect obj signal ?args func] connect [func] to signals
      [signal] emitted by [obj].

      [args] is a pattern for string argument of signals. For example
      [(0, "a"); (2, "b")] will match signals for which argument 0 is
      [Basic(String "a")] and argument 2 is [Basic(String "b")].

      The filtering with [args] is also done by the message bus so
      this may reduce the number of wakup. *)

val connect_any : OBus_peer.t -> 'a t -> ?args:(int * string) list -> (OBus_proxy.t -> 'a -> unit) -> receiver Lwt.t
  (** [connect_any peer signal ?args func] same as connect but [func]
      will receive signals emited by any objects of [peer]. *)

val disable : receiver -> unit Lwt.t
  (** Disable a receiver. Do nothing if the receiver is already
      disabled *)

val enable : receiver -> unit Lwt.t
  (** Enable a receiver. Do nothing if the receiver is already
      enabled *)

val enabled : receiver -> bool
  (** Tell weather the given receiver is currently active *)

(** {6 Signal creation} *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping of signal contents *)

val make :
  ?broadcast:bool ->
  interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  [< 'a OBus_type.cl_sequence ] -> 'a t
  (** [make ?broadcast interface member typ] create a signal.

      [broadcast] tell weather the signal is broadcasted or not. It
      default to [true]. *)

val dmake :
  ?broadcast:bool ->
  interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  OBus_message.body t
  (** Same thing but the value returned are dynamically typed and
      there is no constraint on the signal type *)

