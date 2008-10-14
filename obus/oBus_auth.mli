(*
 * oBus_auth.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handle authentification mechanisms *)

type data = string
    (** Data for an auth mechanism *)

exception Auth_failure of string
  (** Exception raise when authentication fail *)

(** {6 Client-side authentification mechanisms} *)

type client_mechanism_return =
    (** Value returned by the client-side of an auth mechanism *)
  | Client_mech_continue of data
      (** Continue the authentification with this response *)
  | Client_mech_ok of data
      (** Authentification done *)
  | Client_mech_error of string
      (** Authentification failed *)

class virtual client_mechanism_handler : object
  method virtual init : client_mechanism_return Lwt.t
    (** Initial return value of the mechanism *)

  method data : data -> client_mechanism_return Lwt.t
    (** [mech_data] must continue the mechanism process with the given
        data. Default implementation fail with an error message. *)

  method abort : unit
    (** Must abort the mechanism. *)
end

type client_mechanism = string * (unit -> client_mechanism_handler)
    (** A mechiansm consist on a mechanism name and a function to
        create the handlers *)

val client_mech_external : client_mechanism
val client_mech_anonymous : client_mechanism
val client_mech_dbus_cookie_sha1 : client_mechanism
val default_client_mechanisms : client_mechanism list

(** {6 Server-side authentification mechanims} *)

type server_mechanism_return =
    (** Value returned by the server-side of an auth mechanism *)
  | Server_mech_continue of data
      (** Continue the authentification with this challenge *)
  | Server_mech_ok
      (** The client is authentified *)
  | Server_mech_reject
      (** The client is rejected by the mechanism *)

class virtual server_mechanism_handler : object
  method init : data option Lwt.t
    (** Initial challenge *)

  method virtual data : data -> server_mechanism_return Lwt.t
    (** [mech_data] must continue the mechanism process with the given
        response. *)

  method abort : unit
    (** Must abort the mechanism *)
end

type server_mechanism = string * (unit -> server_mechanism_handler)

val server_mech_external : server_mechanism
val server_mech_anonymous : server_mechanism
val server_mech_dbus_cookie_sha1 : server_mechanism
val default_server_mechanisms : server_mechanism list

(** {6 Authentification} *)

val client_authenticate : ?mechanisms:client_mechanism list ->
  Lwt_chan.in_channel * Lwt_chan.out_channel -> OBus_address.guid Lwt.t
  (** Launch client-side authentication on the given input and
      output channels.

      If it succeed return the unique identifiant of the server
      address. *)

val server_authenticate : ?mechanisms:server_mechanism list -> OBus_address.guid ->
  Lwt_chan.in_channel * Lwt_chan.out_channel -> unit Lwt.t
  (** Launch server-side authentication on the given input and
      output channels. *)
