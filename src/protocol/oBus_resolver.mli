(*
 * oBus_resolver.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Bus name resolving *)

(** This module implements bus name resolving and monitoring.

    - for a unique connection name, it means being notified when the
    peer owning this name exits

    - for a well-known name such as "org.domain.Serivce" it means
    knowing at each time who is the current owner and being notified
    when the service owner changes (i.e. the process implementing the
    service change).

    It is basically an abstraction for {!OBus_bus.get_owner} and
    {!OBus_bus.name_owner_changed}. You should prefer using it instead
    of implementing your own name monitoring because resolver are
    shared and obus internally uses them, so this avoids extra messages.

    Note that with a peer-to-peer connection, resolver will always act
    as if there is no owner. *)

val make : ?switch : Lwt_switch.t -> OBus_connection.t -> OBus_name.bus -> OBus_name.bus React.signal Lwt.t
  (** [make ?switch bus name] creates a resolver which will monitor
      the name [name] on [bus]. It returns a signal holding the
      current owner of the name. It holds [""] when there is no
      owner. *)
