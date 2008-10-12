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

type family = Ipv4 | Ipv6

type desc =
  | Unix_path of string
  | Unix_abstract of string
  | Unix_tmpdir of string
      (** A unix socket *)
  | Tcp of string * string * family option
      (** [Tcp(host, service, family)] *)
  | Autolaunch
  | Unknown of name * (key * value) list
      (** An address which is not known by obus *)

type t = desc * guid option

exception Parse_failure of string

val of_string : string -> t list
  (** [of_string str] parse [str] and return the list of addresses
      defined in it. It can raise a [Parse_error]. *)

val system : t list Lazy.t
  (** [system] list of addresses for system bus *)

val session : t list Lazy.t
  (** [session] list of addresses for session bus *)
