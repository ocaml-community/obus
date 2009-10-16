(*
 * oBus_peer.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** A dbus peer *)

(** A D-Bus peer represent an application which can be reach though a
    D-Bus connection. It is the application at the end-point of the
    connection or, if the end-point is a message bus, any application
    connected to it. *)

type t = {
  connection : OBus_connection.t;
  (** Connection used to reach the peer. *)

  name : OBus_name.bus option;
  (** Name of the peer. This only make sense if the connection is a
      connection to a message bus. *)
} with projection

(** Note that it is possible to use either a unique connection name or
    a bus name as peer name.

    Both possibility have advantages and drawbacks:

    - using bus names such as "org.freedesktop.DBus.Hal" avoid the
    need to resolve the name. When doing the first method call the bus
    will automatically start the service if available. Also if the
    service restart the peer will still be valid.

    One drawback is that the owner may change over the time, and
    method calls may not be made on the same peer.

    - using unique name, which can be retreived with bus functions
    (see {!OBus_bus}), ensure that the peer won't change over time.
    By the way if the service exit, or another application replace it
    and we want to always use the default one, we have to write the
    code to handle owner change.

    So, one good strategy is to use bus name when calls do not involve
    side-effect on the service such as object creation, and use unique
    name for object create on our demand. Basically you can stick to
    this rule:

    Always use bus name for a well-known objects, such as
    "/org/freedesktop/Hal/Manager" on "org.freedesktop.Hal.Manager"
    and use unique name for objects for which the path is retreived
    from a method call.

    By the way, the type combinator {!OBus_proxy.obus_t} will awlays
    return a proxy using a peer using a unique name, so you do not
    have to care about this if you use it.
*)

val obus_t : t OBus_type.sequence
  (** This return the peer sending a message.

      This may be used in method type to get the sender of a call, for
      example:

      {[
        OBUS_method Foo : OBus_peer.t -> int -> string
      ]}
  *)

val make : OBus_connection.t -> OBus_name.bus -> t
  (** [make connection name] make a named peer *)

val anonymous : OBus_connection.t -> t
  (** [anonymous connection] make an anonymous peer *)

val ping : t -> t Lwt.t
  (** Ping a peer, and return the peer which really respond to the
      ping.

      For example, the fastest way to get the the peer owning a bus
      name, and start it if not running is:

      [ping (OBus_peer.make bus "well.known.name")] *)

val get_machine_id : t -> OBus_uuid.t Lwt.t
  (** Return the id of the machine the peer is running on *)

val wait_for_exit : t -> unit Lwt.t
  (** [wait_for_exit peer] wait until [peer] exit. If [peer] is not
      running then it returns immediatly. Raises [Invalid_argument] if
      the peer has no name. *)
