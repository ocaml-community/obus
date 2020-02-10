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
  (** Exception raised when authentication fail *)

(** List of capabilities clients/servers may support *)
type capability =
    [ `Unix_fd
        (** The transport supports unix fd passing *) ]

val capabilities : capability list
  (** List of all capabilities *)

(** {6 Communication} *)

type stream
  (** A stream is a way of communication for an authentication
      procedure *)

val make_stream : recv : (unit -> string Lwt.t) -> send : (string -> unit Lwt.t) -> stream
  (** Creates a stream for authentication.

      @param recv must read a complete line, ending with ["\r\n"],
      @param send must send the given line. *)

val stream_of_channels : Lwt_io.input_channel * Lwt_io.output_channel -> stream
  (** Creates a stream from a pair of channels *)

val stream_of_fd : Lwt_unix.file_descr -> stream
  (** Creates a stream from a file descriptor. Note that the stream
      created by this function is not really efficient because it has
      to read characters one by one to ensure it does not consume too
      much. *)

val max_line_length : int
  (** Maximum length accepted for lines of the authentication
      protocol. Beyond this limit, authentication will fail. *)

(** Client-side authentication *)
module Client : sig

  (** {6 Mechanisms} *)

  type mechanism_return =
      (** Value returned by the client side of an auth mechanism *)
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

  (** An client-side authentication mechanism *)
  type mechanism = {
    mech_name : string;
    (** Name of the mechanism *)
    mech_exec : unit -> mechanism_handler;
    (** Mechanism creator *)
  }

  val mech_name : mechanism -> string
    (** [mech_name] projection *)

  val mech_exec : mechanism -> unit -> mechanism_handler
    (** [mech_exec] projection *)

  (** {8 Predefined mechanisms} *)

  val mech_external : mechanism
  val mech_anonymous : mechanism
  val mech_dbus_cookie_sha1 : mechanism
  val default_mechanisms : mechanism list

  (** {6 Authentication} *)

  val authenticate :
    ?capabilities : capability list ->
    ?mechanisms : mechanism list ->
    stream : stream -> unit -> (OBus_address.guid * capability list) Lwt.t
    (** Launch client-side authentication on the given stream. On
        success it returns the unique identifier of the server address
        and capabilities that were successfully negotiated with the
        server.

        Note: [authenticate] does not sends the initial null byte. You
        have to handle it before calling [authenticate].

        @param capabilities defaults to []
        @param mechanisms defualts to {!default_mechanisms}
    *)
end

(** Server-side authentication *)
module Server : sig

  (** {6 Mechanisms} *)

  type mechanism_return =
      (** Value returned by the server-side of an auth mechanism *)
    | Mech_continue of data
        (** Continue the authentication with this challenge *)
    | Mech_ok of int option
        (** The client is authenticated. The argument is the user id
            the client is authenticated with. *)
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

  (** A server-side authentication mechanism *)
  type mechanism = {
    mech_name : string;
    (** The mechanism name *)
    mech_exec : int option -> mechanism_handler;
    (** The mechanism creator. It receive the user id of the client,
        if available. *)
  }

  val mech_name : mechanism -> string
    (** [mech_name projection] *)
  val mech_exec : mechanism -> int option -> mechanism_handler
    (** [mech_exec projection] *)

  (** {8 Predefined mechanisms} *)

  val mech_anonymous : mechanism
  val mech_external : mechanism
  val mech_dbus_cookie_sha1 : mechanism
  val default_mechanisms : mechanism list

  (** {6 Authentication} *)

  val authenticate :
    ?capabilities : capability list ->
    ?mechanisms : mechanism list ->
    ?user_id : int ->
    guid : OBus_address.guid ->
    stream : stream -> unit -> (int option * capability list) Lwt.t
    (** Launch server-side authentication on the given stream. On
        success it returns the client uid and the list of capabilities
        that were successfully negotiated. A client uid of {!None}
        means that the client used anonymous authentication, and may
        be disconnected according to server policy.

        Note: [authenticate] does not read the first zero byte. You
        must read it by hand, and maybe use it to receive credentials.

        @param user_id is the user id determined by external method
        @param capabilities defaults to [[]]
        @param mechanisms default to {!default_mechanisms}
    *)
end
