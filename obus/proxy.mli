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

val make : Connection.t -> 'a Interface.t -> ?destination:Connection.name -> Path.t -> 'a t
  (** [make connection interface destination path] create a proxy with
      interface [interface] and [connection] as backend.  If
      [destination] is provided it will be used in sent messages *)

val path : 'a t -> Path.t
  (** [path proxy] get the path of a proxy *)

val name : 'a t -> Connection.name option
  (** [name proxy] get the connection bus name of [proxy] *)

val connection : 'a t -> Connection.t
  (** [connection proxy] return the connection used for [proxy] *)

(**/**)

open Wire

type ('a, 'b) intern_method_call_desc = {
  intern_mcd_interface : 'a Interface.t;
  intern_mcd_member : string;
  intern_mcd_input_signature : string;
  intern_mcd_output_signature : string;
  intern_mcd_reader : 'b body_reader;
}

val intern_proxy_call_sync : 'a t -> ('a, 'b) intern_method_call_desc -> body_writer -> 'b
val intern_proxy_call_async : 'a t -> ('a, 'b) intern_method_call_desc -> body_writer -> ?on_error:(exn -> unit) -> ('b -> unit) -> unit
val intern_proxy_call_cookie : 'a t -> ('a, 'b) intern_method_call_desc -> body_writer -> 'b Cookie.t
val intern_proxy_call_no_reply : 'a t -> ('a, 'b) intern_method_call_desc -> body_writer -> unit
