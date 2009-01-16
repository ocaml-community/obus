(*
 * oBus_address.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of DBus addresses *)

type name = string
type key = string
type value = string

type guid = OBus_uuid.t
    (** Unique address identifier. It is unique for a server listening
        address.  *)

type tcp_params = {
  tcp_host : string;
  (** For connecting, this is is the host to contact *)

  tcp_bind : string;
  (** For listening, this is the address to bind to *)

  tcp_port : string;
  (** Port number or tcp service name *)

  tcp_family : [ `Ipv4 | `Ipv6 ] option;
  (** Restrict to ipv4 or ipv6 *)
} with projection

type desc =
  | Unix_path of string
  | Unix_abstract of string
  | Unix_tmpdir of string
      (** A unix socket *)
  | Tcp of tcp_params
  | Autolaunch
  | Unknown of name * (key * value) list
      (** An address which is not known by obus *)
 with constructor

type t = desc * guid option

val obus_list : t list OBus_type.basic
  (** Type combinator *)

exception Parse_failure of string

val of_string : string -> t list
  (** [of_string str] parse [str] and return the list of addresses
      defined in it. It can raise a [Parse_error]. *)

val to_string : t list -> string
  (** [to_string addresses] return a string representation of a list
      of addresses *)

val system : t list Lwt.t Lazy.t
  (** [system] list of addresses for system bus *)

val session : t list Lwt.t Lazy.t
  (** [session] list of addresses for session bus *)
