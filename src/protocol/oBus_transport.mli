(*
 * oBus_transport.mli
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Low-level transporting of messages *)

type t
  (** Type of message transport *)

val recv : t -> OBus_message.t Lwt.t
  (** [recv tr] receives one message from the given transport *)

val send : t -> OBus_message.t -> unit Lwt.t
  (** [send tr msg] sends [msg] over the transport [tr]. *)

val capabilities : t -> OBus_auth.capability list
  (** Returns the capabilities of the transport *)

val shutdown : t -> unit Lwt.t
  (** [shutdown tr] frees resources allocated by the given transport *)

val make :
  ?switch : Lwt_switch.t ->
  recv : (unit -> OBus_message.t Lwt.t) ->
  send : (OBus_message.t -> unit Lwt.t) ->
  ?capabilities : OBus_auth.capability list ->
  shutdown : (unit -> unit Lwt.t) -> unit -> t
  (** [make ?switch ~recv ~send ~support_unxi_fd ~shutdown ()] creates
      a new transport from the given functions. @param capabilities
      defaults to [[]].

      Notes:
      - message reading/writing are serialized by obus, so there is no
        need to handle concurrent access to transport
  *)

val loopback : unit -> t
  (** Loopback transport, each message sent is received on the same
      transport *)

val socket : ?switch : Lwt_switch.t -> ?capabilities : OBus_auth.capability list ->  Lwt_unix.file_descr -> t
  (** [socket ?switch ?capabilities socket] creates a socket
      transport.

      @param capabilities defaults to [[]]. For unix sockets, the
      [`Unix_fd] capability is accepted. *)

val of_addresses :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Client.mechanism list ->
  OBus_address.t list ->
  (OBus_address.guid * t) Lwt.t
    (** [of_addresses ?switch ?capabilities ?mechanisms addresses] tries to:

        - connect to the server using one of the given given addresses,
        - authenticate itself to the server using [mechanisms], which
          defaults to {!OBus_auth.Client.default_mechanisms},
        - negotiates [capabilities], which defaults to
          {!OBus_auth.capabilities}

        If all succeeded, it returns the server address guid and the
        newly created transport, which is ready to send and receive
        messages.

        Note about errors:
        - if one of the addresses is not valid, or [addresses = []],
          it raises [Invalid_argument],
        - if all connections failed, it raises the exception raised
          by the try on first address, which is either a [Failure] or
          a [Unix.Unix_error]
        - if the authentication failed, a {!OBus_auth.Auth_error} is
          raised
    *)
