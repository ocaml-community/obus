(*
 * oBus_proxy.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Remote D-Bus objects *)

(** A proxy is an object on which live on a different processus, but
    behave as a native ocaml value. *)

(** The default type for proxies *)
type t = {
  peer : OBus_peer.t;
  (** Peer owning the object *)

  path : OBus_path.t;
  (** Path of the object on the peer *)
}

val compare : t -> t -> int
  (** Same as [Pervasives.compare]. It allows this module to be used
      as argument to the functors [Set.Make] and [Map.Make]. *)

val make : peer : OBus_peer.t -> path : OBus_path.t -> t
  (** Creates a proxy from the given peer and path *)

(** {6 Informations} *)

val peer : t -> OBus_peer.t
  (** Returns the peer pointed by a proxy *)

val path : t -> OBus_path.t
  (** Returns the path of a proxy *)

val connection : t -> OBus_connection.t
  (** [connection proxy = OBus_peer.connection (peer proxy)] *)

val name : t -> OBus_name.bus
  (** [connection proxy = OBus_peer.name (peer proxy)] *)

val introspect : t -> OBus_introspect.document Lwt.t
  (** [introspect proxy] introspects the given proxy *)

(** {6 Method calls} *)

val call : t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  o_args : 'b OBus_value.C.sequence -> 'a -> 'b Lwt.t
  (** [call proxy ~interface ~member ~i_args ~o_args args] calls the
      given method on the given proxy and wait for the reply. *)

val call_with_context : t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  o_args : 'b OBus_value.C.sequence -> 'a -> (OBus_context.t * 'b) Lwt.t
  (** [call_with_context] is like {!call} except that is also returns
      the context of the method return *)

val call_no_reply : t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence -> 'a -> unit Lwt.t
  (** [call_no_reply] is the same as {!call} except that it does not
      wait for a reply *)

(** {6 Private proxies} *)

(** The two following module interface and implementations are helpers
    for using private proxies. A private proxy is just a normal proxy
    but defined as a private type, to avoid incorrect use. *)

type proxy = t

(** Minimal interface of private proxies *)
module type Private = sig
  type t = private proxy
  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end

(** Minimal implementation of private proxies *)
module Private : sig
  type t = proxy
  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end
