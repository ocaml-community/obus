(*
 * oBus_auth.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Handle authentication mechanisms *)

type data = string
    (** Data for an authentication mechanism *)

exception Auth_failure of string
  (** Exception raise when authentication fail *)

(** {6 Communication} *)

type stream
  (** Way of communication for a mechanism *)

val make_stream :
  get_char : (unit -> char Lwt.t) ->
  put_char : (char -> unit Lwt.t) ->
  flush : (unit -> unit Lwt.t) -> stream
  (** Creates a stream from lowlevel functions. *)

val stream_of_channels : Lwt_io.input_channel -> Lwt_io.output_channel -> stream
  (** Creates a stream from a pair of lwt channels *)

(** List of capatilities clients/servers may support *)
type capability =
    [ `Unix_fd
        (** The transport support unix fd passing *) ]

(** Client-side authentication *)
module Client : sig

  type mechanism_return =
      (** Value returned by the client-side of an auth mechanism *)
    | Mech_continue of data
        (** Continue the authentication with this response *)
    | Mech_ok of data
        (** Authentification done *)
    | Mech_error of string
        (** Authentification failed *)

  class virtual mechanism_handler : object
    method virtual init : mechanism_return Lwt.t
      (** Initial return value of the mechanism *)

    method data : data -> mechanism_return Lwt.t
      (** [mech_data] must continue the mechanism process with the given
          data. Default implementation fail with an error message. *)

    method abort : unit
      (** Must abort the mechanism. *)
  end

  type mechanism = string * (unit -> mechanism_handler)
      (** A mechiansm consist on a mechanism name and a function to
          create the handlers *)

  val mech_external : mechanism
  val mech_anonymous : mechanism
  val mech_dbus_cookie_sha1 : mechanism
  val default_mechanisms : mechanism list

  val authenticate :
    ?capabilities : capability list ->
    ?mechanisms : mechanism list ->
    stream -> (OBus_address.guid * capability list) Lwt.t
    (** Launch client-side authentication on the given stream. On
        success it returns the unique identifier of the server address
        and capabilities that were successfully negotiated with the
        server.

        @param capabilities defaults to []
        @param mechanisms defualts to {!default_mechanisms}
    *)
end

(** Server-side authentication *)
module Server : sig

  type mechanism_return =
      (** Value returned by the server-side of an auth mechanism *)
    | Mech_continue of data
        (** Continue the authentication with this challenge *)
    | Mech_ok
        (** The client is authentified *)
    | Mech_reject
        (** The client is rejected by the mechanism *)

  class virtual mechanism_handler : object
    method init : data option Lwt.t
      (** Initial challenge *)

    method virtual data : data -> mechanism_return Lwt.t
      (** [mech_data] must continue the mechanism process with the given
          response. *)

    method abort : unit
      (** Must abort the mechanism *)
  end

  type mechanism = string * (unit -> mechanism_handler)

  val mech_dbus_cookie_sha1 : mechanism
  val default_mechanisms : mechanism list

  val authenticate : ?capabilities : capability list -> ?mechanisms : mechanism list -> OBus_address.guid -> stream -> capability list Lwt.t
    (** Launch server-side authentication on the given stream. On
        success it returns the capabilities the client successfully
        negotiated.

        @param capabilities defaults to [[]]
        @param mechanisms default to {!default_mechanisms}
    *)
end
