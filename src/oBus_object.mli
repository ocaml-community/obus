(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Local D-Bus objects *)

(** This module allow you to create D-Bus objects and export them on a
    connection, allowing other programs to acccess them. *)

type t with obus(basic)
  (** Default type for local D-Bus objects. It contains informations
      needed by obus to export it on a connection and dispatch
      incomming method calls. *)

val remove_by_path : OBus_connection.t -> OBus_path.t -> unit
  (** [remove_by_path connection path] removes the object with path
      [path] on [connection]. Works for normal objects and dynamic
      nodes. *)

(** Interface definition *)
module Interface : sig
  (** This module is aimed to be use with the [obus.syntax] syntax
      extension.

      It allow you to easily register members.

      For example, instead of:

      {[
        module M = OBus_object.Make(...)

        let iface = M.make_interface "org.mydomain"

        let foo a b c = return (a + b + c)
        let bar a b = return (a ^ b)

        let () =
          OBus_object.Interface.method_call
            obus_local_interface
            "Foo"
            <:obus_func< int -> int -> int -> int >>;
          OBus_object.Interface.method_call
            obus_local_interface
            "Bar"
            <:obus_func< string -> string -> string >>
      ]}

      You can simply write:

      {[
        module M = OBus_object.Make(...)

        let obus_local_interface = new M.interface "org.mydomain"

        let foo a b c = return (a + b + c)
        OL_method Foo : int -> int -> int -> int
      ]}

      or:

      {[
        module M = OBus_object.Make(...)

        let obus_local_interface = M.make_interface "org.mydomain"

        OL_method Foo : int -> int -> int -> int =
          fun a b c -> return (a + b + c)
        OL_method Bar : int -> int -> int -> int =
          fun a b c -> return (a + b + c)
      ]}

      where [OL_method] stands for "OBus Local method".
  *)

  type 'obj t
    (** Type of an interface. ['obj] is the type of objects used. *)

  val name : 'obj t -> OBus_name.interface
    (** Name of the interface *)

  val close : 'obj t -> unit
    (** [close iface] `cloes'' the interface. This means that no more
        members can be added. *)

  val introspect : 'obj t -> OBus_introspect.interface
    (** Returns introspection data of the interface.

        Note: it also closes the interface. *)

  (** {6 Member registration} *)

  val method_call : 'obj t -> OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> ('obj -> 'a) -> unit
    (** Registers a method call *)

  val signal : 'obj t -> OBus_name.member -> ('a, _) OBus_type.cl_sequence -> unit
    (** Registers a signal *)

  val emit : 'obj t -> OBus_name.member -> ('a, _) OBus_type.cl_sequence -> 'obj -> ?peer : OBus_peer.t -> 'a -> unit Lwt.t
    (** [emit member typ obj ?peer x] emits a signal *)

  (** Note that the syntax extension both registers the signal with
      {!signal} and defines the emitter with {!emit}. *)

  val property_r : 'obj t -> OBus_name.member -> ('a, _) OBus_type.cl_single -> ('obj -> 'a Lwt.t) -> unit
    (** Registers a read-only property *)

  val property_w : 'obj t -> OBus_name.member -> ('a, _) OBus_type.cl_single -> ('obj -> 'a -> unit Lwt.t) -> unit
    (** Registers a write-only property *)

  val property_rw : 'obj t -> OBus_name.member -> ('a, _) OBus_type.cl_single -> ('obj -> 'a Lwt.t) -> ('obj -> 'a -> unit Lwt.t) -> unit
    (** Registers a read and write property *)
end

(** Local object signature *)
module type S = sig

  type obj with obus(basic)
    (** The type of objects *)

  (** {6 Interfaces} *)

  val make_interface : OBus_name.interface -> obj Interface.t
    (** [make_interface name] creates an empty interface. New members
        can be added using the module {!Interface}. *)

  val add_interface : obj -> obj Interface.t -> unit
    (** [add_interface obj iface] adds suport for the interface
        described by [iface] to the given object. If an interface with
        the same name is already attached to the object, then it is
        replaced by the new one. *)

  val remove_interface : obj -> obj Interface.t -> unit
    (** [remove_interace obj iface] removes informations about the
        given interface from [obj]. If [obj] do not implement the
        interface, it does nothing. *)

  val remove_interface_by_name : obj -> OBus_name.interface -> unit
    (** Same as {!remove_interface} by takes only the interface name
        as argument. *)

  (** {8 Well-known interfaces} *)

  val introspectable : obj Interface.t
    (** The [org.freedesktop.DBus.Introspectable] interface *)

  val properties : obj Interface.t
    (** The [org.freedesktop.DBus.Properties] interface *)

  (** {6 Constructors} *)

  val make : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : obj Interface.t list -> OBus_path.t -> t
    (** [make ?owner ?interfaces path] creates a new object with path
        [path].

        If [owner] is specified, then all signals will be sent to it
        by default, and it will be removed from all its exports when
        the owner exit.

        [interfaces] is the list of interfaces implemented by the
        object. New interfaces can be added latter with
        {!add_interface}. If [common] is [true] (the default) then
        [introspectable] and [properties] are automatically aded.

        Note that the returned value is of type {!t} and not
        {!obj}. Typically, the creation of an object of type {!obj}
        will look like this:

        {[
          type my_object = {
            obus : OBus_object.t;
            foo : string;
            bar : int;
            ...
          }

          module M = OBus_object.Make(struct
                                        type obj = my_object
                                        let get obj = obj.obus
                                      end)

          let make foo bar ... = {
            obus = M.make ~interfaces:[iface1; iface2; ...] ["some"; "path"];
            foo = foo;
            bar = bar;
            ...
          }
    *)

  val make' : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : obj Interface.t list -> unit -> t
    (** Same as [make] but generate a unique path *)

  (** {6 Properties} *)

  val path : obj -> OBus_path.t
    (** [path obj] returns the path of the object *)

  val owner : obj -> OBus_peer.t option
    (** [owner obj] returns the owner of the object, if any *)

  val exports : obj -> Set.Make(OBus_connection).t React.signal
    (** [exports obj] is the signal holding the list of connection on
        which the object is exported *)

  val introspect : obj -> OBus_introspect.interface list
    (** [introspect obj] returns the introspection of all interfaces
        implemented by [obj] *)

  (** {6 Exports} *)

  val export : OBus_connection.t -> obj -> unit
    (** [export connection obj] exports [obj] on [connection] *)

  val remove : OBus_connection.t -> obj -> unit
    (** [remove connection obj] removes [obj] from [connection] *)

  val destroy : obj -> unit
    (** [destroy obj] removes [obj] from all connection it is exported
        on *)

  val dynamic : connection : OBus_connection.t -> prefix : OBus_path.t -> handler : (OBus_path.t -> obj Lwt.t) -> unit
    (** [dynamic ~connection ~prefix ~handler] defines a dynamic node
        it the tree of object. This means that objects with a path
        prefixed by [prefix], will be created on the fly by [handler]
        when a process try to access them.

        [handler] receive the rest of path after the prefix.

        Note: if you manually export an object with a path prefixed by
        [prefix], it will be prefered to the one created by
        [handler].

        Here is an example (a very basic VFS):

        {[
          (* The type of a file *)
          type file = {
            obus : OBus_object.t;
            name : string;
            owner : int;
            group : int;
            ...
          }

          module File = OBus_object.Make(struct
                                           type obj = file
                                           let get obj = obj.obus
                                         end)

          (* Definition and implementation of interfaces *)
          let obus_local_interface = File.make_interface "org.foo.File"

          OL_method owner : uint = fun file -> return file.owner
          OL_method group : uint = fun file -> return file.group

          let size file = Lwt_io.file_length file.name
          OL_method Size : uint64

          ...

          (* The prefix used for the VFS: *)
          let prefix = ["org"; "foo"; "VFS"]

          (* Registration *)
          let () =
            File.dynamic ~connection ~prefix
              ~handler:(function
                          | [escaped_name] ->
                              return { obus = OBus_object.make (prefix @ [escaped_name]);
                                       name = OBus_path.unescape escaped_name;
                                       owner = ...;
                                       group = ...;
                                       ... }
                          | _ ->
                              fail (Failure "invalid path"))
        ]}
    *)

  (** {6 Signals} *)

  val emit : obj ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence ->
    ?peer : OBus_peer.t -> 'a -> unit Lwt.t
    (** [emit obj ~interface ~member typ ?peer x] emits a signal. If
        [peer] is specified then the signal is sent only to it,
        otherwise it is broadcasted. *)
end

(** The default object implementation *)
include S with type obj = t

(** Signature of custom objects *)
module type Custom = sig
  type obj
    (** Type of custom object *)

  val cast : obj -> t
    (** [cast obj] should returns the obus object attached to the given
        custom object. *)

(** Typical example:

    {[
      type t = {
        obus : OBus_object.t;
        x : int;
        y : int;
        ...
      }

      module M = OBus_object.Make(struct
                                    type obj = t
                                    let info obj = obj.obus
                                  end)

*)
end

module Make(Object : Custom) : S with type obj = Object.obj
  (** [Make(Object)] creates a custom object module *)
