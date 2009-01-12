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
}

val tt : t OBus_type.ty_basic
  (** Type combinator *)

(** {6 Creation/informations} *)

val make : OBus_peer.t -> OBus_path.t -> t
  (** [make peer path] create a proxy *)

val peer : t -> OBus_peer.t
val path : t -> OBus_path.t
  (** Functionnal version of proxy filed *)

(** {6 Introspection} *)

val introspect : t -> (OBus_introspect.interface list * t list) Lwt.t
  (** Introspect the proxy *)

val raw_introspect : t -> OBus_introspect.document Lwt.t
  (** Same as [introspect] but do not create proxy for sub-nodes *)

(** {6 Method calls} *)

val call : t -> ?interface:OBus_name.interface -> member:OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> 'a
  (** Call a method of the given proxy *)

val call_no_reply : t -> ?interface:OBus_name.interface -> member:OBus_name.member -> ('a, unit Lwt.t, unit) OBus_type.ty_function -> 'a
  (** Same as call but do not wait for a reply *)

val call' : t -> ?interface:OBus_name.interface -> member:OBus_name.member -> OBus_message.body -> [< 'a OBus_type.cl_sequence ] -> 'a Lwt.t
  (** Take the body of the call as a dynamically-typed value. This can
      be used to write some generic function, for example:

      {[
        let f member ty =
          OBus_type.make_func ty
            (fun body -> perform
               bus <-- Lazy.force OBus_bus.session;
               peer <-- OBus_bus.get_peer "some.well.known.bus.name";
               call' { peer = peer;
                       path = [ "some"; "well"; "known"; "path" ] }
                 ~interface:"some.well.known.interface"
                 ~member
                 body
                 (OBus_type.func_reply ty))
      ]}
  *)

val dcall : t -> ?interface:OBus_name.interface -> member:OBus_name.member -> OBus_message.body -> OBus_message.body Lwt.t
  (** Use only dynamically typed values *)
