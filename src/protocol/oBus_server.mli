(*
 * oBus_server.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Servers for one-to-one communication *)

type t
  (** Type of a server *)

val addresses : t -> OBus_address.t list
  (** [addresses server] returns all the addresses the server is
      listenning on. These addresses must be passed to clients so they
      can connect to [server]. *)

val shutdown : t -> unit Lwt.t
  (** [shutdown server] shutdowns the given server. It terminates when
      all listeners (a server may listen on several addresses) have
      exited. If the server has already been shut down, it does
      nothing. *)

val make :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Server.mechanism list ->
  ?addresses : OBus_address.t list ->
  ?allow_anonymous : bool ->
  (t -> OBus_connection.t -> unit) -> t Lwt.t
  (** [make ?switch ?capabilities ?mechanisms ?addresses ?allow_anonymous f]
      Creates a server which will listen on all of the given addresses.

      @param capabilites is the set of the server's capabilities,
      @param mechanisms is the list of authentication mechanisms
             supported by the server,
      @param addresses default to
             [{ name = "unix"; args = [("tmpdir", "/tmp")]],
      @param allow_anonymous tell whether clients using anonymous
             authentication will be accepted. It defaults to [false],
      @param capabilities is the list of supported capabilities, it
             defaults to {!OBus_auth.capabilities}
      @param f is the callback which receive new clients. It takes
             as arguments the server and the connection for the client.

      About errors:
      - if no addresses are provided, it raises [Invalid_argument],
      - if an address is invalid, it raises [Invalid_argument]
      - if listening fails for one of the addresses, it fails with the
        exception reported for that address

      It succeeds if it can listen on at least one address.

      When a new client connects, the server handles authentication of
      this client, then it creates a transport and the connection on
      top of this transport.

      Note that connections passed to [f] are initially down. It is up
      to the user to set them up with {!OBus_connection.set_up}. *)

val make_lowlevel :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Server.mechanism list ->
  ?addresses : OBus_address.t list ->
  ?allow_anonymous : bool ->
  (t -> OBus_transport.t -> unit) -> t Lwt.t
  (** [make_lowlevel] is the same as {!make} except that [f] receives
      only the transport, and no connection is created for this
      transport. *)
