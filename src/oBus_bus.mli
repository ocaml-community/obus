(*
 * oBus_bus.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Interface to D-Bus message bus *)

type t = OBus_connection.t

(** {6 Well-known instances} *)

val session : unit -> t Lwt.t
  (** [session ()] returns a connection to the user session message
      bus. Subsequent calls to {!session} will return the same bus. *)

val system : unit -> t Lwt.t
  (** [system ()] returns a connection to the system message bus. As
      for {!session}, subsequent calls to {!system} will return the
      same bus. However, if the connection is closed or it crashes,
      {!system} will try to reopen it. *)

(** {6 Creation} *)

val of_addresses : OBus_address.t list -> t Lwt.t
  (** Establish a connection with a message bus. The bus must be
      accessible with at least one of the given addresses *)

val register_connection : ?set_on_disconnect : bool -> OBus_connection.t -> unit Lwt.t
  (** Register the given connection to a message bus. It has the side
      effect of requesting a name to the message bus if not already
      done.

      If [set_on_disconnect] is [true] (the default) then the
      {!OBus_connection.on_disconnect} function of the connection is
      replaced (see notes bellow).

      If the connection is a connection to a message bus, created with
      one of the function of {!OBus_connection} then
      {!register_connection} must be called on it before any other
      function.

      If this is not the case, it will (probably) raise an
      {!OBus_error.Unknown_method} *)

(** Notes:

    - when the connection to a message bus is lost
    {!OBus_connection.Connection_lost}, the program is exited with a
    return code of 0

    - when a fatal error happen, a message is printed on stderr and
    the program is exited with an exit code of 1

    This can be changed by overriding
    {!OBus_connection.on_disconnect} *)

(** {6 Peer/proxy} *)

val get_peer : t -> OBus_name.bus -> OBus_peer.t Lwt.t
  (** [get_peer bus name] return the peer owning the bus name
      [name]. If the service is not activated and is activable, then
      it is started *)

val get_proxy : t -> OBus_name.bus -> OBus_path.t -> OBus_proxy.t Lwt.t
  (** [get_proxy bus name path] resolve [name] with {!get_peer} and
      return a proxy for the object with path [path] on this
      service *)

(** {6 Bus names acquiring} *)

val acquired_names : t -> Set.Make(String).t React.signal
  (** Returns the list of names we currently own *)

val access_denied : OBus_error.name
  (** Exception raised when a name cannot be owned due to security
      policies *)

type request_name_result =
    [ `Primary_owner
        (** You are now the primary owner of the connection *)
    | `In_queue
        (** You will get the name when it will be available *)
    | `Exists
        (** Somebody else already have the name and nobody specify
            what to do in this case *)
    | `Already_owner
        (** You already have the name *) ]

val request_name : t ->
  ?allow_replacement:bool ->
  ?replace_existing:bool ->
  ?do_not_queue:bool ->
  OBus_name.bus -> request_name_result Lwt.t
  (** Request a name to the bus. This is the way to acquire a
      well-know name.

      All optionnal parameters default to [false], their meaning are:

      - [allow_replacement]: allow other application to steal you the name
      - [replace_existing]: replace any existing owner of the name
      - [do_not_queue]: do not queue if not available
  *)

type release_name_result =
    [ `Released
    | `Non_existent
    | `Not_owner ]

val release_name : t -> OBus_name.bus -> release_name_result Lwt.t

(** {6 Service starting/discovering} *)

val service_unknown : OBus_error.name
  (** Exception raised when a service is not present on a message bus
      and can not be started automatically *)

type start_service_by_name_result =
    [ `Success
    | `Already_running ]

val start_service_by_name : t -> OBus_name.bus -> start_service_by_name_result Lwt.t
  (** Start a service on the given bus by its name *)

val name_has_owner : t -> OBus_name.bus -> bool Lwt.t
  (** Return [true] if the service is currently running, i.e. some
      application offer it on the message bus *)

val list_names : t -> OBus_name.bus list Lwt.t
  (** List names currently running on the message bus *)

val list_activatable_names : t -> OBus_name.bus list Lwt.t
  (** List services that can be activated. A service is automatically
      activated when you call one of its method or when you use
      [start_service_by_name] *)

val name_has_no_owner : OBus_error.name

val get_name_owner : t -> OBus_name.bus -> OBus_name.bus Lwt.t
  (** Return the connection unique name of the given service. Raise a
      [Name_has_no_owner] if the given name does not have an owner. *)

val list_queued_owners : t -> OBus_name.bus -> OBus_name.bus list Lwt.t
  (** Return the connection unique names of applications waiting for a
      name *)

val service_unknown : OBus_error.name
  (** Raised when we try to contact a service which is not available
      and the bus do not known how to start it *)

(** {6 Messages routing} *)

val match_rule_invalid : OBus_error.name
  (** Exception raised when the program trey to send an invalid match
      rule. This should never happen since values of type
      {!OBus_match.rule} are always valid. *)

val add_match : t -> OBus_match.rule -> unit Lwt.t
  (** Add a matching rule on a message bus. This means that every
      message routed on the message bus matching this rule will be
      sent to us.

      It can raise an [Out_of_memory]. *)

val match_rule_not_found : OBus_error.name

val remove_match : t -> OBus_match.rule -> unit Lwt.t
  (** Remove a match rule from the message bus. It raise a
      [OBus_match_not_found] if the rule does not exists *)

(** {6 Other} *)

(** These functions are also offered by the message bus *)

val adt_audit_data_unknown : OBus_error.name
val selinux_security_context_unknown : OBus_error.name

val update_activation_environment : t -> (string * string) list -> unit Lwt.t
val get_connection_unix_user : t -> OBus_name.bus -> int Lwt.t
val get_connection_unix_process_id : t -> OBus_name.bus -> int Lwt.t
val get_adt_audit_session_data : t -> OBus_name.bus -> string Lwt.t
val get_connection_selinux_security_context : t -> OBus_name.bus -> string Lwt.t
val reload_config : t -> unit Lwt.t
val get_id : t -> OBus_uuid.t Lwt.t

(** {6 Signals} *)

val name_owner_changed : t -> (OBus_name.bus * OBus_name.bus option * OBus_name.bus option) OBus_signal.t
  (** This signal is emited each the owner of a name (unique
      connection name or service name) change.

      Connection message looks like: [(name, None, Some name)] and
      disconnection message looks like: [(name, Some name, None)]
      where is a connection unique name. *)

val name_lost : t -> OBus_name.bus OBus_signal.t
val name_acquired : t -> OBus_name.bus OBus_signal.t
