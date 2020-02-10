(*
 * oBus_bus.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message buses management *)

type t = OBus_connection.t

(** {6 Well-known instances} *)

val session : ?switch : Lwt_switch.t -> unit -> t Lwt.t
  (** [session ?switch ()] returns a connection to the user session
      message bus. Subsequent calls to {!session} will return the same
      bus.  OBus will automatically exit the program when an error
      happens on the session bus. You can change this behavior by
      calling {!OBus_connection.set_on_disconnect}. *)

val system : ?switch : Lwt_switch.t -> unit -> t Lwt.t
  (** [system ?switch ()] returns a connection to the system message
      bus. As for {!session}, subsequent calls to {!system} will
      return the same bus. However, if the connection is closed or
      crashes, {!system} will try to reopen it. *)

(** {6 Creation} *)

val of_addresses : ?switch : Lwt_switch.t -> OBus_address.t list -> t Lwt.t
  (** Establish a connection with a message bus. The bus must be
      accessible with at least one of the given addresses *)

val register_connection : OBus_connection.t -> unit Lwt.t
  (** Register the given connection to a message bus. It has the side
      effect of requesting a name to the message bus if not already
      done.

      If the connection is a connection to a message bus, created with
      one of the function of {!OBus_connection} then
      {!register_connection} must be called on it before any other
      functions. *)

val exit_on_disconnect : exn -> 'a
  (** Function which exit the program as follow:

      - if [exn] is {!OBus_connection.Connection_lost}, it exits the
      program with a return code of 0

      - if [exn] is a fatal error, it prints a message on stderr and
      exits the program with an exit code of 1
  *)

(** {6 Peer/proxy helpers} *)

val get_peer : t -> OBus_name.bus -> OBus_peer.t Lwt.t
  (** [get_peer bus name] returns the peer owning the bus name
      [name]. If the service is not activated and is activable, then
      it is started *)

val get_proxy : t -> OBus_name.bus -> OBus_path.t -> OBus_proxy.t Lwt.t
  (** [get_proxy bus name path] resolves [name] with {!get_peer} and
      returns a proxy for the object with path [path] on this
      service *)

(** {6 Bus names} *)

val name : t -> OBus_name.bus
  (** Same as {!OBus_connection.name}. *)

val names : t -> Set.Make(String).t React.signal
  (** [names bus] is the signal holding the set of all names we
      currently own. It raises [Invalid_argument] if the connection is
      not a connection to a message bus. *)

val hello : t -> OBus_name.bus Lwt.t
  (** [hello connection] sends an hello message to the message bus,
      which returns the unique connection name of the connection. Note
      that if the hello message has already been sent, it will
      fail. *)

exception Access_denied of string
  (** Exception raised when a name cannot be owned due to security
      policies *)

type request_name_result =
    [ `Primary_owner
        (** You are now the primary owner of the connection *)
    | `In_queue
        (** You will get the name when it will be available *)
    | `Exists
        (** Somebody else already have the name and nobody specified
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

      All optional parameters default to [false], their meaning are:

      - [allow_replacement]: allow other application to steal this name from you
      - [replace_existing]: replace any existing owner of the name
      - [do_not_queue]: do not queue if not available
  *)

type release_name_result =
    [ `Released
    | `Non_existent
    | `Not_owner ]

val release_name : t -> OBus_name.bus -> release_name_result Lwt.t

(** {6 Service starting/discovering} *)

exception Service_unknown of string
  (** Exception raised when a service is not present on a message bus
      and can not be started automatically *)

type start_service_by_name_result =
    [ `Success
    | `Already_running ]

val start_service_by_name : t -> OBus_name.bus -> start_service_by_name_result Lwt.t
  (** Start a service on the given bus by its name *)

val name_has_owner : t -> OBus_name.bus -> bool Lwt.t
  (** Returns [true] if the service is currently running, i.e. some
      application offers it on the message bus *)

val list_names : t -> OBus_name.bus list Lwt.t
  (** List names currently running on the message bus *)

val list_activatable_names : t -> OBus_name.bus list Lwt.t
  (** List services that can be activated. A service is automatically
      activated when you call one of its method or when you use
      [start_service_by_name] *)

exception Name_has_no_owner of string

val get_name_owner : t -> OBus_name.bus -> OBus_name.bus Lwt.t
  (** Return the connection unique name of the given service. Raise a
      [Name_has_no_owner] if the given name does not have an owner. *)

val list_queued_owners : t -> OBus_name.bus -> OBus_name.bus list Lwt.t
  (** Return the connection unique names of the applications waiting for a
      name *)

(** {6 Messages routing} *)

(** Note that you should prefer using {!OBus_match.export} and
    {!OBus_match.remove} since they do not add duplicated rules
    several times. *)

exception Match_rule_invalid of string
  (** Exception raised when the program tries to send an invalid match
      rule. This should never happen since values of type
      {!OBus_match.rule} are always valid. *)

val add_match : t -> OBus_match.rule -> unit Lwt.t
  (** Add a matching rule on a message bus. This means that every
      message routed on the message bus matching this rule will be
      sent to us.

      It can raise {!OBus_error.No_memory}.
  *)

exception Match_rule_not_found of string

val remove_match : t -> OBus_match.rule -> unit Lwt.t
  (** Remove a match rule from the message bus. It raises
      {!Match_rule_not_found} if the rule does not exists *)

(** {6 Other} *)

(** These functions are also offered by the message bus *)

exception Adt_audit_data_unknown of string
exception Selinux_security_context_unknown of string

val update_activation_environment : t -> (string * string) list -> unit Lwt.t
val get_connection_unix_user : t -> OBus_name.bus -> int Lwt.t
val get_connection_unix_process_id : t -> OBus_name.bus -> int Lwt.t
val get_adt_audit_session_data : t -> OBus_name.bus -> string Lwt.t
val get_connection_selinux_security_context : t -> OBus_name.bus -> string Lwt.t
val reload_config : t -> unit Lwt.t
val get_id : t -> OBus_uuid.t Lwt.t

(** {6 Signals} *)

val name_owner_changed : t -> (OBus_name.bus * OBus_name.bus * OBus_name.bus) OBus_signal.t
  (** This signal is emitted each time the owner of a name (unique
      connection name or service name) changes. *)

val name_lost : t -> OBus_name.bus OBus_signal.t
val name_acquired : t -> OBus_name.bus OBus_signal.t
