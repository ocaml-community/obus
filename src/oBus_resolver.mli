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

class type t = object
  method name : OBus_name.bus option React.signal
    (** Signal holding the name owner *)

  method disable : unit Lwt.t
    (** Stop monitoring this name *)
end

val make : OBus_connection.t -> OBus_name.bus -> t Lwt.t
  (** [make ?serial ?on_change bus name] make a new resolver for
      [name]. Each time the name owner change [on_change] is called
      with the new owner, it it also called initially with the current
      name owner.

      [serial] tell wether calls to [on_change] must be serialized. It
      defaults to [false] *)
