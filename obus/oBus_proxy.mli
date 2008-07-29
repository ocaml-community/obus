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

type t = {
  connection : OBus_connection.t;
  (** Connection used for serializing method calls on the object *)

  service : string option;
  (** Service on which the object is living *)

  path : OBus_path.t;
  (** Path of the object on the application owning it *)
}

val ob_t : (t, _, OBus_annot.dobject_path) OBus_comb.one
  (** Type combinator *)

val make : connection:OBus_connection.t -> ?service:string -> path:OBus_path.t -> t
val connection : t -> OBus_connection.t
val path : t -> OBus_path.t
val service : t -> string option

val method_call : t -> ?interface:string -> member:string -> ('a, 'b Lwt.t, 'b) OBus_comb.func -> 'a
  (** Send a method call on a proxy *)

val kmethod_call : ('b -> 'c Lwt.t) -> t -> ?interface:string -> member:string -> ('a, 'c Lwt.t, 'b) OBus_comb.func -> 'a
  (** Same thing but with continuation *)

val umethod_call : t -> ?interface:string -> member:string -> OBus_value.sequence -> OBus_value.sequence Lwt.t
  (** Send a method call with dynamically typed datas *)
