(*
 * oBus_address.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of DBus addresses *)

type guid = OBus_uuid.t
    (** A unique address identifier. Each server listenning address'
        has a unique one. *)

(** Parameters for TCP addresses *)
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

type address =
  | Unix_path of string
  | Unix_abstract of string
  | Unix_tmpdir of string
      (** A unix socket *)
  | Tcp of tcp_params
  | Autolaunch
  | Unknown of string * (string * string) list
      (** [Unknown(name, values)] an address which is not known by
          obus. [values] is a list of [(key, value)]. *)
 with constructor

type t = {
  address : address;
  (** The address part of an address *)

  guid : guid option;
  (** An address may optionnally specifiy a guid *)
} with projection

val obus_list : t list OBus_type.basic
  (** Type combinator *)

exception Parse_failure of string

val of_string : string -> t list
  (** [of_string str] parse [str] and return the list of addresses
      defined in it.

      @raise [Parse_failure] if the string contains an invalid address
  *)

val to_string : t list -> string
  (** [to_string addresses] return a string representation of a list
      of addresses *)

val system : t list Lwt.t Lazy.t
  (** [system] list of addresses for system bus *)

val session : t list Lwt.t Lazy.t
  (** [session] list of addresses for session bus *)
