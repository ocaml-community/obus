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

(** Type default proxy type *)
type t = {
  peer : OBus_peer.t;
  (** Peer owning the object *)

  path : OBus_path.t;
  (** Path of the object on the peer *)
} with obus(basic)

val make : peer : OBus_peer.t -> path : OBus_path.t -> t
  (** Creates a proxy from the given peer and path *)

(** Type of an D-Bus interface anme *)
module type Interface_name = sig
  val name : OBus_name.interface
end

(** Proxy signature *)
module type S = sig

  type proxy with obus(basic)
    (** Type of proxy objects *)

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

  (** A signal definition. ['a] is the type of signals contents. *)
  class type ['a] signal = object
    method event : 'a React.event
      (** The signal itself *)

    method disconnect : unit
      (** Stop receiving the signal *)
  end

  val connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence -> 'a signal
    (** [connect proxy ~interface ~member typ] connect to given signals
        emitted by [proxy]. *)

  val dyn_connect : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.sequence signal
    (** Same thing but return signals as a dynamically typed values *)

  (** {6 Properties} *)

  val get : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> 'a Lwt.t
    (** [get proxy ~interface ~member typ] returns the value of the
        given property *)

  val set : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_single -> 'a -> unit Lwt.t
    (** [set proxy ~interface ~member typ value] sets the value of the
        given property *)

  val dyn_get : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member -> OBus_value.single Lwt.t
    (** [dyn_get proxy ~interface ~member] returns the value of the
        given property as a dynamically typed value *)

  val dyn_set : proxy ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    OBus_value.single -> unit Lwt.t
    (** [dyn_set proxy ~interface ~member value] sets the value of the
        given property *)

  val dyn_get_all : proxy -> interface : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
    (** [dyn_get_all t ~interface] returns the list of all properties of
        the given proxy with their values *)

  (** {6 interface definition} *)

  (** This is the ocaml version of a D-Bus interface for proxy code.

      Note that interface contained in XML introspection files can be
      automatically converted with [obus-binder] *)
  module Make_interface(Name : Interface_name) : sig
    val op_interface : OBus_name.interface
      (** Name of the interface *)

    val op_method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> proxy -> 'a
      (** [op_method_call member typ] defines a method call.

          A method call definition looks like:

          {[
            let caml_name = op_method_call "DBusName" method_call_type
          ]}

          Or even simpler, with the syntax extension:

          {[
            OP_method DBusName : method_call_type
          ]}

          where [OP_method] stands for "OBus Proxy method".
      *)

    val op_signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> proxy -> 'a signal
      (** [op_signal member typ] defines a signal.

          A signal defintion looks like:

          {[
            let caml_name = signal "DBusName" signal_type
          ]}

          Or, with the syntax extension:

          {[
            OBUS_signal DBusName : signal_type
          ]}
      *)

    val op_property_reader : OBus_name.member -> ('a, _) OBus_type.cl_single -> proxy -> 'a Lwt.t
      (** [op_property_reader member typ] defines a property reader.

          A property reader definition looks like:

          {[
            let caml_name = property_reader "DBusName" property_type
          ]}
      *)

    val op_property_writer : OBus_name.member -> ('a, _) OBus_type.cl_single -> proxy -> 'a -> unit Lwt.t
      (** [property_writer member typ] defines a property writer.

          A property writer definition looks like:

          {[
            let set_caml_name = property_writer "DBusName" property_type
          ]}
      *)

    (** With the syntax extension, read-only properties
        (resp. write-only properties, resp. read and write properties)
        can be defined like this:

        {[
          OBUS_property_r DBusName : property_type
          OBUS_property_w DBusName : property_type
          OBUS_property_rw DBusName : property_type
        ]}
    *)
  end
end

(** The default proxy implementation *)
include S with type proxy = t

(** Custom proxy type: *)
module type Custom = sig
  type proxy
    (** Type of proxy objects *)

  val get : proxy -> t
    (** Returns the underlying obus proxy *)

  val make : t -> proxy
    (** Create a custom proxy from the given proxy. This function is
        only used in the type combinator. *)
end

(** Create a custom proxy module: *)
module Make(Proxy : Custom) : S with type proxy = Proxy.proxy
