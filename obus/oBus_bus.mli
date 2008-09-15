(*
 * oBus_bus.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Interface to DBus message bus *)

type t = OBus_connection.t
    (** A bus is a connection with a special application which act as
        a router between several applications. *)

(** {6 Well-known instances} *)

val session : t Lwt.t Lazy.t
  (** The session message bus. This is the one which is started at the
      beginning of each user session. *)

val system : t Lwt.t Lazy.t
  (** The system message bus. It is unique given one system. *)

(** {6 Creation} *)

val of_addresses : OBus_address.t list -> t Lwt.t
  (** Establish a connection with a message bus. The bus must be
      accessible with at least one of the given addresses *)

val register_connection : OBus_connection.t -> unit Lwt.t
  (** This function just request a unique name to the other side of
      the connection which is supposed to be a message bus.

      If this is nqot the case, it will (probably) raise an
      [OBus_error.Unknown_method] *)

(** {6 Bus names acquiring} *)

type name = OBus_name.bus
    (** A bus name, for example "org.freedesktop.DBus" *)

type request_name_flag =
    [ `allow_replacement
        (** Allow other application to steal you the name *)
    | `replace_existing
        (** Replace any existing owner of the name *)
    | `do_not_queue
        (** Do not queue if not available *) ]

type request_name_result =
    [ `primary_owner
        (** You are now the primary owner of the connection *)
    | `in_queue
        (** You will get the name when it will be available *)
    | `exists
        (** Somebody else already have the name and nobody specify
            what to do in this case *)
    | `already_owner
        (** You already have the name *) ]

val request_name : t -> name -> request_name_flag list -> request_name_result Lwt.t
  (** Request a name to the bus. This allow you to acquire a well-know
      name so other applications can easily access to your service. *)

type release_name_result =
    [ `released
    | `non_existent
    | `not_owner ]

val release_name : t -> name -> release_name_result Lwt.t

(** {6 Service starting/discovering} *)

type start_service_flag
  (** These flags are currently unused. *)

type start_service_by_name_result =
    [ `success
    | `already_running ]

val start_service_by_name : t -> name -> start_service_flag list -> start_service_by_name_result Lwt.t
  (** Start a service on the given bus by its name *)

val name_has_owner : t -> name -> bool Lwt.t
  (** Return [true] if the service is currently running, i.e. some
      application offer it on the message bus *)

val list_names : t -> name list Lwt.t
  (** List names currently running on the message bus *)

val list_activable_names : t -> name list Lwt.t
  (** List services that can be activated. A service is automatically
      activated when you call one of its method or when you use
      [start_service_by_name] *)

exception Name_has_no_owner of string

val get_name_owner : t -> name -> OBus_name.connection_unique Lwt.t
  (** Return the connection unique name of the given service. Raise a
      [Name_has_no_owner] if the given name does not have an owner. *)

val list_queued_owners : t -> name -> OBus_name.connection_unique list Lwt.t
  (** Return the connection unique names of applications waiting for a
      name *)

(** {6 Messages routing} *)

type match_rule
  (** Type of a rule used to match a message *)

val match_rule :
  ?typ:[ `method_call | `method_return | `error | `signal ] ->
  ?sender:OBus_name.connection ->
  ?interface:OBus_name.interface ->
  ?member:OBus_name.member ->
  ?path:OBus_path.t ->
  ?destination:OBus_name.connection_unique ->
  ?args:(int * string) list ->
  unit -> match_rule
  (** Create a matching rule. Matching the argument [n] with string
      value [v] will match a message if its [n]th argument is a string
      and is equal to [v]. [n] must in the range 0..63 *)

val add_match : t -> match_rule -> unit Lwt.t
  (** Add a matching rule on a message bus. This means that every
      message routed on the message bus matching the rules will be
      sent to us.

      It can raise an [Out_of_memory]. *)

exception Match_rule_not_found of string

val remove_match : t -> match_rule -> unit Lwt.t
  (** Remove a match rule from the message bus. It raise a
      [Match_rule_not_found] if the rule does not exists *)

(** {6 Other} *)

(** These functions are also offered by the message bus *)

val get_connection_unix_user : t -> string -> int Lwt.t
val get_connection_unix_process_id : t -> string -> int Lwt.t
val get_connection_selinux_security_context : t -> string -> string Lwt.t
val reload_config : t -> unit Lwt.t
val get_id : t -> string Lwt.t

(** {6 Signals} *)

val on_name_owner_changed : t -> (name -> OBus_name.connection_unique -> OBus_name.connection_unique -> unit) -> OBus_signal.receiver Lwt.t
  (** This signal is emited each the owner of a name (unique
      connection name or service name) change.

      Connection message looks like: name: "" -> name
      and disconnection message looks like: name: name -> ""
      where is a connection unique name. *)

val on_name_lost : t -> (name -> unit) -> OBus_signal.receiver Lwt.t
val on_name_acquired : t -> (name -> unit) -> OBus_signal.receiver Lwt.t

(** {6 Service monotiring} *)

type status = [ `down | `up ]
    (** Service status *)

val on_service_status_change : t -> name -> (status * status -> unit) -> OBus_signal.receiver Lwt.t
  (** [on_service_status_change bus service f] call f each the status
      of the service [service] change. The first argument of [f] is
      [(old_status, new_status)]. *)
