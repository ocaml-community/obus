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
      class virtual iface = OBUS_interface iface "org.mydomain.iface"
        OBUS_method FooBar : int -> int
        OBUS_signal plop : string * int
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
        method virtual foo_bar : int -> int Lwt.t
        method plop : ?peer:OBus_peer.t -> string * int -> unit Lwt.t
      end
    ]}

    And to implement an object with this dbus interface we just have to
    inherit from it:

    {[
      class toto = object
        inherit OBus_object.t
        inherit iface

        method foo_bar x = return (x * 42)
      end
    ]}

    Note: It is possible to do it without the syntax extension but it
    is really more verbose. Look at the result of [obus-binder
    -service -no-sugar <file.xml>] to see how it works.
*)

type member_desc
  (** Describe an interface member *)

class virtual interface : object
  method virtual obus_emit_signal : 'a 'b. OBus_name.interface -> OBus_name.member ->
    ([< 'a OBus_type.cl_sequence ] as 'b) -> ?peer:OBus_peer.t -> 'a -> unit Lwt.t
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

  method obus_handle_call : OBus_connection.t -> OBus_message.t -> unit
    (** Handle a method call *)

  method obus_introspect : OBus_connection.t -> OBus_introspect.document Lwt.t
    (** Self introspection *)

  method obus_get : OBus_name.interface -> OBus_name.member -> OBus_value.single Lwt.t
  method obus_set : OBus_name.interface -> OBus_name.member -> OBus_value.single -> unit Lwt.t
  method obus_get_all : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
    (** Object properties *)

  method obus_emit_signal : 'a 'b. OBus_name.interface -> OBus_name.member ->
    ([< 'a OBus_type.cl_sequence ] as 'b) -> ?peer:OBus_peer.t -> 'a -> unit Lwt.t
    (** Emit a signal.

        If [peer] is specified it will be sent to this peer only,
        otherwise it will be broadcasted on any connection the object
        is exported on *)

  method obus_add_interface : OBus_name.interface -> member_desc list -> unit
    (** Add the given interface, for introspection *)

  method obus_export : OBus_connection.t -> unit
    (** [obus_export connection] export the object on [connection]. *)

  method obus_remove : OBus_connection.t -> unit
    (** [obus_remove connection] remove the object from
        [connection] *)

  method obus_destroy : unit
    (** Remove the object from any connection it is exported on *)

  method obus_connection_closed : OBus_connection.t -> unit
    (** This method is called by the connection when it is closed (by
        {!OBus_connection.close} or for an unexpected reason).

        Warning : /!\ You must not call this method directly /!\

        By the way it is ok to override this if you still call the
        original one. *)
end

(** Object ``owned'' by someone else. This is for when the object is
    created for a specific client.

    This means that, when the client exit:

    - the object is destroyed, i.e. [obus_destroy] is called
    - signals will be sent only to this client *)
class owned : OBus_peer.t -> t
