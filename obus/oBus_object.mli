(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus objects *)

(** {6 Abstract interface} *)

(** Note: abstract interface can be defined with the [pa_obus] syntax
    extension.

    It looks like:

    {[
      OBUS_class iface "org.mydomain.iface" = object
        OBUS_method Foo : int -> int
        OBUS_signal bar : string * int
      end
    ]}

    This create the virtual class [iface] where methods are replaced by
    virtual methods which return a lwt value and signals by methods
    which emit a signal.

    With the previous example, the created class will have the following
    signature:

    {[
      class virtual iface : object
        inherit OBus_object.interface
        method virtual foo : int -> int Lwt.t
        method bar : ?connection:OBus_connection.t ->
                     ?destination:OBus_name.connection ->
                     string * int -> unit Lwt.t
      end
    ]}

    And to implement an object with this dbus interface we just have to
    inherit from it:

    {[
      class toto = object
        inherit OBus_object.t
        inherit iface

        method foo x = return (x * 42)
      end
    ]}

    Note: since interface member may start with a capital letter but
    caml methods can not, the caml version is always uncapitalized,
    but the dbus version is kept unchanged.
*)

type member_desc
  (** Describe an interface member *)

class virtual interface : object
  method virtual obus_emit_signal : 'a 'b.
    ?connection:OBus_connection.t ->
    ?destination:OBus_name.connection ->
    OBus_name.interface -> OBus_name.member ->
    ([< 'a OBus_type.cl_sequence ] as 'b) -> 'a -> unit Lwt.t
    (** Emit a signal *)

  method virtual obus_add_interface : OBus_name.interface -> member_desc list -> unit
    (** Attach dbus description to the object *)
end

val md_method : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> (unit -> 'a) -> member_desc
val md_signal : OBus_name.member -> [< 'a OBus_type.cl_sequence ] -> member_desc
val md_property_r : OBus_name.member -> [< 'a OBus_type.cl_single ] -> (unit -> 'a Lwt.t) -> member_desc
val md_property_w : OBus_name.member -> [< 'a OBus_type.cl_single ] -> ('a -> unit Lwt.t) -> member_desc
val md_property_rw : OBus_name.member -> [< 'a OBus_type.cl_single ] -> (unit -> 'a Lwt.t) -> ('a -> unit Lwt.t) -> member_desc

(** {6 Objects} *)

class t : object
  method obus_path : OBus_path.t
    (** Object path of the object. By default it is computed from its
        object id. *)

  method obus_handle_call : OBus_connection.t -> OBus_message.method_call -> unit
    (** Handle a method call *)

  method introspect : OBus_connection.t -> OBus_introspect.document Lwt.t
    (** Self introspection *)

  method get : OBus_name.interface -> OBus_name.member -> OBus_value.single Lwt.t
  method set : OBus_name.interface -> OBus_name.member -> OBus_value.single -> unit Lwt.t
  method getAll : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
    (** Object properties *)

  method obus_emit_signal : 'a 'b.
    ?connection:OBus_connection.t ->
    ?destination:OBus_name.connection ->
    OBus_name.interface -> OBus_name.member ->
    ([< 'a OBus_type.cl_sequence ] as 'b) -> 'a -> unit Lwt.t
    (** Emit a signal.

        If [connection] is specified, it will be sent on this
        connection, otherwise it is sent on any connection the object
        is exported on.

        If [destnation] is specified then the signal will be sent to
        this specific destination, otherwise it will be broadcasted *)

  method obus_add_interface : OBus_name.interface -> member_desc list -> unit
    (** Add the given interface, for introspection *)

  method obus_export : OBus_connection.t -> unit
    (** [obus_export connection] export the object on [connection]. *)

  method obus_remove : OBus_connection.t -> unit
    (** [obus_remove connection] remove the object from
        [connection] *)

  method obus_clear : unit
    (** Remove the object from any connection it is exported on *)

  method obus_connection_closed : OBus_connection.t -> unit
    (** This method is called by the connection when it is closed (by
        {!OBus_connection.close} or for an unexpected reason).

        Warning : /!\ You must not call this method directly /!\

        By the way it is ok to override this if you still call the
        original one. *)
end

val get_by_path : OBus_connection.t -> OBus_path.t -> t
  (** [get_by_path connection path] return the object with path [path]
      exported on [connection]

      @raise Not_found if no such object exists *)

val opt_get_by_path : OBus_connection.t -> OBus_path.t -> t option
  (** Same thing but return an option instead of raise an exception *)

val tt : t OBus_type.ty_basic
  (** Type combinator *)

(** Object ``owned'' by someone else. This is for when the object is
    created for a specific client.

    This means that, when the client exit:

    - the object is destroyed, i.e. [obus_clear] is called
    - signals will be sent only to this client *)
class owned : OBus_bus.t -> OBus_name.connection_unique -> t
