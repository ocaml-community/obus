(*
 * oBus_interface.mli
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Proxy interface definition *)

(** Name of an interface *)
module type Name = sig
  val name : OBus_name.interface
end

(** This is the ocaml version of a D-Bus interface for proxy code.

    Note that interface contained in XML introspection files can be
    automatically converted with [obus-binder] *)
module Make(Name : Name) : sig
  val op_interface : OBus_name.interface
    (** Name of the interface *)

  val op_method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> OBus_proxy.t -> 'a
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

  val op_signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> OBus_proxy.t -> 'a OBus_signal.t
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

  val op_property_reader : OBus_name.member -> ('a, _) OBus_type.cl_single -> OBus_proxy.t -> 'a Lwt.t
    (** [op_property_reader member typ] defines a property reader.

        A property reader definition looks like:

        {[
          let caml_name = property_reader "DBusName" property_type
        ]}
    *)

  val op_property_writer : OBus_name.member -> ('a, _) OBus_type.cl_single -> OBus_proxy.t -> 'a -> unit Lwt.t
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
