(*
 * oBus_resolver.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Bus name resolving *)

(** This module implement bus names resolving and monitoring.

    - for a unique connection name, it means being notified when the
    peer owning this name exit

    - for a well-known name such as "org.domain.Serivce" it means
    knowing at each time who is the current owner and being notified
    when the service owner change (i.e. the process implementing the
    service change).

    It is basically an abstraction for {!OBus_bus.get_owner} and
    {!OBus_bus.name_owner_changed}. You should prefer using it instead
    of implementing your own name monitoring because resolver are
    shared and obus internally use them, so this avoid extra messages.

    Note that with a peer-to-peer connection, resolver will always act
    as if they is no owner. *)

type t
  (** Type of a resolver *)

val make : ?serial:bool -> ?on_change:(OBus_name.bus option -> unit Lwt.t) -> OBus_connection.t -> OBus_name.bus -> t Lwt.t
  (** [make ?serial ?on_change bus name] make a new resolver for
      [name]. Each time the name owner change [on_change] is called
      with the new owner, it it also called initially with the current
      name owner.

      [serial] tell wether calls to [on_change] must be serialized. It
      defaults to [false] *)

val owner : t -> OBus_name.bus option
  (** [owner m] returns the current name owner *)

val owned : t -> bool
  (** [owned m] returns wether the name is currently owned. *)

val disable : t -> unit
  (** [disable resolver] disable [resolver]. It does nothing if the
      resolver was already disabled.

      Notes:

      - {!owner} and {!owned} raise [Invalid_argument] when called
      with a disabled resolver

      - resolvers are automatically disabled when garbage collected *)

(**/**)
val internal_resolver : t -> OBus_internals.name_resolver_internal
