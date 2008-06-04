(*
 * proxy.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Representation of DBus proxies *)

(** A proxy is an object on which live on a different processus, but
    which behave as a native ocaml value.

    Operation on this object are handled by the low-level api. *)

type 'a t
  (** Representation of proxy for a value of type ['a] *)

type name = string
type path = string

val make : Connection.t -> 'a Interface.t -> ?sender:name -> ?destination:name -> path -> 'a t
  (** [make connection interface destination path] create a proxy with
      interface [interface] and [connection] as backend.  If [sender]
      and/or [destination] are provided they will be used in sent
      messages *)

val path : 'a t -> path
  (** [path proxy] get the path of a proxy *)

val sender : 'a t -> name option
  (** [sender proxy] get the sender name of [proxy] *)

val name : 'a t -> name option
  (** [name proxy] get the connection bus name of [proxy] *)

val connection : 'a t -> Connection.t
  (** [connection proxy] return the connection used for [proxy] *)

(**/**)

type ('a, 'b, 'c) method_call_desc = {
  method_interface : 'a Interface.t;
  method_member : string;
  method_in_sig : string;
  method_out_sig : string;
  method_le_writer : Connection.writer;
  method_be_writer : Connection.writer;
  method_le_reader : ('b -> 'c Connection.reader);
  method_be_reader : ('b -> 'c Connection.reader);
}

val proxy_call_sync : Connection.t -> ('a, 'b, 'c) method_call_desc -> 'b -> 'a t -> 'c
val proxy_call_async : Connection.t -> ('a, 'b, unit) method_call_desc -> 'b -> 'a t -> unit
val proxy_call_with_cookie : Connection.t -> ('a, 'b, 'c) method_call_desc -> 'b -> 'a t -> 'c Cookie.t
