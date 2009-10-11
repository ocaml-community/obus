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
  peer : OBus_peer.t;
  (** Peer owning the object *)

  path : OBus_path.t;
  (** Path of the object on the peer *)
} with projection, obus(basic)

val make : OBus_peer.t -> OBus_path.t -> t
  (** [make peer path] create a proxy *)

val connection : t -> OBus_connection.t
  (** [connection proxy = OBus_peer.connection proxy.peer] *)

val name : t -> OBus_name.bus option
  (** [connection proxy = OBus_peer.name proxy.peer] *)

(** {6 Introspection} *)

val introspect : t -> (OBus_introspect.interface list * t list) Lwt.t
  (** Introspect the proxy *)

val raw_introspect : t -> OBus_introspect.document Lwt.t
  (** Same as [introspect] but do not creates proxies for sub-nodes *)

val children : t -> t list Lwt.t
  (** [children proxy] returns the children of a proxy *)

(** {6 Method calls} *)

val method_call : t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
  (** Call a method of the given proxy *)

val method_call_no_reply : t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, unit Lwt.t, unit) OBus_type.func -> 'a
  (** Same as call but do not wait for a reply *)

val method_call' : t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body ->
  ('a, _) OBus_type.cl_sequence -> 'a Lwt.t
  (** Take the body of the call as a dynamically-typed value. This can
      be used to write some generic function, for example:

      {[
        let f member ty =
          OBus_type.make_func ty
            (fun body ->
               lwt bus = Lazy.force OBus_bus.session in
               lwt peer = OBus_bus.get_peer "some.well.known.bus.name" in
               method_call' { peer = peer;
                              path = [ "some"; "well"; "known"; "path" ] }
                 ~interface:"some.well.known.interface"
                 ~member
                 body
                 (OBus_type.func_reply ty))
      ]}
  *)

val dyn_method_call : t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body ->
  OBus_message.body Lwt.t
  (** Use only dynamically typed values *)

val dyn_method_call_no_reply : t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body -> unit Lwt.t
