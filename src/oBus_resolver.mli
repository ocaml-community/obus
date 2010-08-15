(*
 * oBus_resolver.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
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
  (** Type fo a resolver *)

val make : OBus_connection.t -> OBus_name.bus -> t Lwt.t
  (** [make bus name] creates a resolver which will monitor the name
      [name] on [bus].

      Note that in case [bus] is not a message bus but a one-to-one
      connection, the resolver act as if the name has no owner. *)

val name : t -> OBus_name.bus
  (** Returns the name monitored by the givne resolver *)

val owner : t -> OBus_name.bus React.signal
  (** [owner resolver] returns a signal holding the current owner of
      the name. It holds [""] when there is no owner. *)

val disable : t -> unit Lwt.t
  (** Disable a resolver. Note that a resolver is disabled when its
      owner signal is garbage collected. *)
