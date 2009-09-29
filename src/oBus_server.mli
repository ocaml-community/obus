(*
 * oBus_server.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Server for one-to-one communication *)

type t

val make : ?mechanisms:OBus_auth.server_mechanism list -> ?addresses:OBus_address.desc list ->
  ?serial:bool -> (OBus_connection.t -> unit Lwt.t) -> t Lwt.t
  (** [make ?mechanisms ?addresses ?serial on_connection] Create a server
      which will listen on all the given addresses.

      [mechanisms] is the list of authentication mechanisms supported
      by the server.

      [addresses] default to [OBus_address.Unix_tmpdir "/tmp"].

      [on_connection] is the function which will be called when a new
      connection is created, i.e. a client connect to the server and
      successfully authenticate itself. [serial] tell wether calls to
      [on_connection] must be serialized. It defaults to [false].

      Note that the connection passed to [on_connection] is down to
      avoid race condition. It must be set up by this function.

      @raise Invalid_argument if [addresses] is empty *)

val make_lowlevel : ?mechanisms:OBus_auth.server_mechanism list -> ?addresses:OBus_address.desc list ->
  ?serial:bool -> (OBus_wire.transport -> unit Lwt.t) -> t Lwt.t
  (** Same thing but pass directly a transport to the callback
      function instead of a connection *)

val addresses : t -> OBus_address.t list
  (** Return the listening address of a server, with their associated
      guid. One guid is generated for each listening address at server
      creation time. *)

val shutdown : t -> unit Lwt.t
  (** Shutdown a server *)
