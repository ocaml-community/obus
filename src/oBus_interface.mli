(*
 * oBus_interface.mli
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Proxy interface definition *)

(** This is the ocaml version of a DBus interface for proxy code.

    Note that interface contained in XML introspection files can be
    automatically converted with [obus-binder] *)
module type S = sig
  type t

  val interface : OBus_name.interface
    (** Name of the interface *)

  val method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> t -> 'a
    (** [call member typ] defines a method call.

        A method call definition looks like:

        {[
          let caml_name = call "DBusName" method_call_type
        ]}

        Or even simpler, with the syntax extension:

        {[
          OBUS_method DBusName : method_call_type
        ]}
    *)

  val signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> t -> 'a OBus_signal.t
    (** [signal member typ] defines a signal.

        A signal defintion looks like:

        {[
          let caml_name = signal "DBusName" signal_type
        ]}

        Or, with the syntax extension:

        {[
          OBUS_signal DBusName : signal_type
        ]}
    *)

  val property_reader : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> 'a Lwt.t
    (** [property_reader member typ] defines a property reader.

        A property reader definition looks like:

        {[
          let caml_name = property_reader "DBusName" property_type
        ]}
    *)

  val property_writer : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> 'a -> unit Lwt.t
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

(** Name of an interface *)
module type Name = sig
  val name : OBus_name.interface
end

module Make(Name : Name) : S with type t = OBus_proxy.t

(** {6 Interface using a custom proxy type} *)

module type Custom_proxy = sig
  type t
  val make_proxy : t -> OBus_proxy.t Lwt.t
end

module Make_custom(Proxy : Custom_proxy)(Name : Name) : S with type t = Proxy.t

(** {6 Interface for a single object} *)

module type Single_proxy = sig
  val proxy : OBus_proxy.t Lwt.t Lazy.t
end

module Make_single(Proxy : Single_proxy)(Name : Name) : sig
  val interface : OBus_name.interface
  val method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
  val signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> unit -> 'a OBus_signal.t
  val property_reader : OBus_name.member -> ('a, _) OBus_type.cl_single -> unit -> 'a Lwt.t
  val property_writer : OBus_name.member -> ('a, _) OBus_type.cl_single -> 'a -> unit Lwt.t
end
