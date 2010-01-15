(*
 * oBus_server.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Server for one-to-one communication *)

(** Type of servers *)
class type t = object
  method event : OBus_connection.t React.event
    (** Event which receive new connections.

        Note that new connections are initially down to avoid race
        condition. *)

  method addresses : OBus_address.t list
    (** Listenning addresses *)

  method shutdown : unit Lwt.t
    (** Shutdown the server *)
end

(** Type of lowlevel servers *)
class type lowlevel = object
  method event : OBus_transport.t React.event
    (** Event which receive new transports *)

  method addresses : OBus_address.t list
    (** Listenning addresses *)

  method shutdown : unit Lwt.t
    (** Shutdown the server *)
end

val make :
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Server.mechanism list ->
  ?addresses : OBus_address.address list -> unit -> t Lwt.t
  (** [make ?mechanisms ?addresses] Create a server which will listen
      on all of the given addresses.

      [mechanisms] is the list of authentication mechanisms supported
      by the server.

      [addresses] default to [OBus_address.Unix_tmpdir "/tmp"].

      @raise Invalid_argument if [addresses] is empty *)

val make_lowlevel :
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Server.mechanism list ->
  ?addresses : OBus_address.address list -> unit -> lowlevel Lwt.t
