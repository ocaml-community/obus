(*
 * oBus_proxy.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Representation of DBus proxies *)

(** A proxy is an object on which live on a different processus, but
    behave as a native ocaml value. *)

type t = OBus_internals.proxy

val compare : t -> t -> int
  (** Proxy comparaison function *)

val make : connection:OBus_connection.t -> ?service:string -> path:OBus_path.t -> t
  (** [make connection service path] create a proxy,

      - [connection] is used for serializing method calls on the object
      - [service] is the application on which the object is living
      - [path] is the path of the object on the application owning it *)

val connection : t -> OBus_connection.t
val service : t -> string option
val path : t -> OBus_path.t
  (** Access to proxy informations *)

val method_call : t -> ?interface:string -> member:string -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> 'a
  (** Send a method call on a proxy *)

val kmethod_call : ((t -> ?interface:string -> member:string -> 'b Lwt.t) -> 'c) -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  (** Same thing but with continuation *)

val umethod_call : t -> ?interface:string -> member:string -> OBus_value.sequence -> OBus_value.sequence Lwt.t
  (** Send a method call with dynamically typed datas *)

(*val connect : t -> interface:string -> member:string -> ('a, unit, unit) OBus_comb.func -> 'a -> OBus_signal.id*)
  (** Connect a callback function to the given signal. It is possible
      to connect multiple functions to the same signal. *)
