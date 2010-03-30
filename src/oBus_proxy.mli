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

val obus_t : t OBus_type.basic
  (** The type combinator for proxies *)

val make : peer : OBus_peer.t -> path : OBus_path.t -> t
  (** Creates a proxy from the given peer and path *)

(** Note: if you are just interested in the event, it is safe to
    write:

    {[
      (OBus_proxy.connect ...)#event
    ]}

    The signal is automatically disconnected when the event is garbage
    collected *)

(** Interface definition *)
module Interface : sig

  (** This module allow you to easily define members of a D-Bus
      interface. It is aimed to be used in conjunction with the
      [obus.syntax] syntax extension.

      Here is a typical example:

      {[
        let op_interface = OBus_proxy.make_interface "org.foo.bar"

        OP_method Foo : int -> int
        OP_method Bar : strign -> unit
        ...
      ]}

      where [OP_xxx] stands for "OBus Proxy xxx".

      Note that interface contained in XML introspection files can be
      automatically converted with [obus-binder] *)

  type 'proxy t
    (** Type of an interface. ['proxy] is the type of proxies used. *)

  val name : 'proxy t -> OBus_name.interface
    (** Name of the interface *)

  val method_call : 'proxy t -> OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> 'proxy -> 'a
    (** [method_call iface member typ] defines a method call.

        A method call definition looks like:

        {[
          let caml_name = OBus_proxy.Interface.method_call obus_proxy_interface "DBusName" method_call_type
        ]}

        Or even simpler, with the syntax extension:

        {[
          OP_method DBusName : method_call_type
        ]}
      *)

  val signal : 'proxy t -> OBus_name.member -> ('a, _) OBus_type.cl_sequence -> 'proxy -> 'a OBus_signal.t
    (** [signal iface member typ] defines a signal.

        A signal defintion looks like:

        {[
          let caml_name = OBus_proxy.Interface.signal obus_proxy_interface "DBusName" signal_type
        ]}

        Or, with the syntax extension:

        {[
          OP_signal DBusName : signal_type
        ]}
    *)

  val  property : 'proxy t -> OBus_name.member -> 'access OBus_property.access -> ('a, _) OBus_type.cl_single -> 'proxy -> ('a, 'access) OBus_property.t
    (** [property member typ] defines a property.

        A property definition looks like:

        {[
          let set_caml_name = OBus_proxy.Interface.property obus_proxy_interface access "DBusName" property_type
        ]}

        With the syntax extension, read-only properties
        (resp. write-only properties, resp. read and write properties)
        can be defined like this:

        {[
          OP_property_r DBusName : property_type
          OP_property_w DBusName : property_type
          OP_property_rw DBusName : property_type
        ]}
    *)
end

(** Proxy signature *)
module type S = sig

  type proxy
    (** Type of proxy objects *)

  val obus_proxy : proxy OBus_type.basic
    (** Type combinator for this proxy type *)

  type broken = proxy
    (** Type of broken proxies. See {!obus_broken}. *)

  val obus_broken : broken OBus_type.basic
    (** Same as {!OBus_pervasives.obus_broken_path} but for proxies *)

  val make_interface : ?changed : OBus_name.member -> OBus_name.interface -> proxy Interface.t
    (** [make_interface ?changed name] create an interface using
        proxies of type {!proxy}.

        [changed] is the name of the signal which is sent each time a
        property changes. Setting this parameter will make it possible
        to use {!OBus_property.contents} on properties. *)

  (** {6 Informations} *)

  val peer : proxy -> OBus_peer.t
    (** Returns the peer pointed by a proxy *)

  val path : proxy -> OBus_path.t
    (** Returns the path of a proxy *)

  val connection : proxy -> OBus_connection.t
    (** [connection proxy = OBus_peer.connection (peer proxy)] *)

  val name : proxy -> OBus_name.bus option
    (** [connection proxy = OBus_peer.name (peer proxy)] *)

  (** {6 Introspection} *)

  val introspect : proxy -> OBus_introspect.document Lwt.t
    (** Introspect the given proxy *)

  (** {6 Method calls} *)

  val method_call : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
    (** Call a method of the given proxy *)

  val method_call_no_reply : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, unit Lwt.t, unit) OBus_type.func -> 'a
    (** Same as call but do not wait for a reply *)

  val method_call' : proxy ->
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

  val dyn_method_call : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_message.body ->
    OBus_message.body Lwt.t
    (** Use only dynamically typed values *)

  val dyn_method_call_no_reply : proxy ->
    ?interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_message.body -> unit Lwt.t

  (** {6 Signals} *)

  val connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence -> 'a OBus_signal.t
    (** [connect proxy ~interface ~member typ] connect to given signals
        emitted by [proxy]. *)

  val dyn_connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.sequence OBus_signal.t
    (** Same thing but return signals as a dynamically typed values *)

  (** {6 Properties} *)

  val property : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    access : 'access OBus_property.access ->
    ?changed : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> ('a, 'access) OBus_property.t

  val dyn_property : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    access : 'access OBus_property.access ->
    ?changed : OBus_name.member -> unit -> (OBus_value.single, 'access) OBus_property.t
end

(** The default proxy implementation *)
include S with type proxy = t

(** Custom proxy type: *)
module type Custom = sig
  type proxy
    (** Type of custom proxy objects *)

  val cast : proxy -> t
    (** Returns the underlying obus proxy *)

  val make : t -> proxy
    (** Create a custom proxy from the given proxy. This function is
        only used in the type combinator. *)
end

module Make(Proxy : Custom) : S with type proxy = Proxy.proxy
  (** [Make(Proxy)] creates a custom proxy module *)

(** {6 Private proxies} *)

(** The two following module interface and implementation are helpers
    for using private proxies. A private proxy is just a bormal proxy
    but defined as a private type, to avoid incorrect use. *)

(** Minimal interface of private proxies *)
module type Private = sig
  type t = private proxy
  val obus_t : t OBus_type.basic

  type broken = t
  val obus_broken : broken OBus_type.basic

  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end

(** Minimal implementation of private proxies *)
module Private : sig
  type t = proxy
  val obus_t : t OBus_type.basic

  type broken = t
  val obus_broken : broken OBus_type.basic

  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end
