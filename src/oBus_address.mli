(*
 * oBus_address.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Manipulation of D-Bus addresses *)

(** {6 Types} *)

type guid = OBus_uuid.t
    (** A unique address identifier. Each server's listening address
        has a unique one. *)

(** Type of an address *)
type t = {
  name : string;
  (** The transport name *)

  args : (string * string) list;
  (** Arguments of the address *)
}

val name : t -> string
  (** [name] projection *)

val args : t -> (string * string) list
  (** [args] Projection *)

val make : name : string -> args : (string * string) list -> t
  (** Creates an address *)

val arg : string -> t -> string option
  (** [arg key address] returns the value of argument [key], if any *)

val guid : t -> guid option
  (** Returns the address guid, if any *)

(** {6 To/from string conversion} *)

exception Parse_failure of string * int * string
  (** [Parse_failure(string, position, reason)] exception raised when
      parsing a string failed. *)

val of_string : string -> t list
  (** [of_string str] parse [str] and return the list of addresses
      defined in it.

      @raise Parse_failure if the string contains an invalid address
  *)

val to_string : t list -> string
  (** [to_string addresses] return a string representation of a list
      of addresses *)

(** {6 Well-known addresses} *)

val system : t list Lwt.t Lazy.t
  (** The list of addresses for system bus *)

val session : t list Lwt.t Lazy.t
  (** The list of addresses for session bus *)

val default_system : t list
  (** The default addresses for the system bus *)

val default_session : t list
  (** The default addresses for the session bus *)
