(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus objects *)

(** This module allow you to create D-Bus objects and export them on a
    connection, allowing other programs to acccess them. *)

type t
  (** Type of an obus object. It contains informations needed by obus
      to export it on a connection and dispatch incomming method
      calls. *)

(** {6 Creation} *)

val make : ?owner : OBus_peer.t -> OBus_path.t -> t
  (** [make ?owner path] creates a new object with path [path].

      If [owner] is specified, then all signals will be sent to it by
      default, and it will be removed from all its exports when the
      owner exit. *)

val make' : ?owner : OBus_peer.t -> unit -> t
  (** Same as [make] but generate a unique path *)

(** {6 Properties} *)

val path : t -> OBus_path.t
  (** [path obj] returns the path of the object *)

val owner : t -> OBus_peer.t option
  (** [owner obj] returns the owner of the object, if any *)

val exports : t -> Set.Make(OBus_connection).t React.signal
  (** [exports obj] is the signal holding the list of connection on
      which the object is exported *)

(** {6 Suppression} *)

val remove_by_path : OBus_connection.t -> OBus_path.t -> unit
  (** [remove_by_path connection path] removes the object with path
      [path] on [connection]. Works for normal objects and dynamic
      nodes. *)

(** Signature of custom objects *)
module type Object = sig
  type obj
    (** Type of custom object *)

  val get : obj -> t
    (** [get obj] should returns the obus object attached to the
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
                                    let get obj = obj.obus
                                  end)

*)
end

module Make(Object : Object) : sig

  type t = Object.obj
    with obus(basic)
    (** The type of objects, with its type combinator *)

  val export : OBus_connection.t -> t -> unit
    (** [export connection obj] exports [obj] on [connection] *)

  val remove : OBus_connection.t -> t -> unit
    (** [remove connection obj] removes [obj] from [connection] *)

  val destroy : t -> unit
    (** [destroy obj] removes [obj] from all connection it is exported
        on *)

  val dynamic : connection : OBus_connection.t -> prefix : OBus_path.t -> handler : (OBus_path.t -> t Lwt.t) -> unit
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
          include File.MakeInterface(struct let name = "org.foo.File" end)

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

  val emit : t ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence ->
    ?peer : OBus_peer.t -> 'a -> unit Lwt.t
    (** [emit obj ~interface ~member typ ?peer x] emits a signal. If
        [peer] is specified then the signal is sent only to it, otherwise
        it is broadcasted. *)

  module Make_interface(Name : OBus_proxy.Interface_name) : sig

    (** This module is aimed to be use with the [obus.syntax] syntax
        extension.

        It allow you to easily register members.

        For example, instead of:

        {[
          module M = OBus_object.Make(...)

          let foo a b c = return (a + b +c)

          let () =
            M.register_method
              ~interface : "org.mydomain"
              ~member : "Foo"
              ~typ : <:obus_func< int -> int -> int -> int >>
              ~handler : foo
        ]}

        You can simply write:

        {[
          module M = OBus_object.Make(...)

          include M.MakeInterface(struct let name = "org.mydomain" end)

          let foo a b c = return (a + b +c)
          OL_method Foo : int -> int -> int -> int
        ]}

        or:

        {[
          module M = OBus_object.Make(...)

          include M.MakeInterface(struct let name = "org.mydomain" end)

          OL_method Foo : int -> int -> int -> int =
            fun a b c -> return (a + b + c)
        ]}

        where [OL_method] stands for "OBus Local method".
    *)

    val ol_interface : OBus_name.interface
      (** Name of the interface *)

    val ol_method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> (t -> 'a) -> unit
      (** Registers a method call *)

    val ol_signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> (t -> ?peer : OBus_peer.t -> 'a -> unit Lwt.t)
      (** Registers a signal and define the signal emiting
          function.

          Note: [ol_signal] has two effect, it register the signal for
          introspection purpose, and returns the signal emiting
          function. So it is an error to write something like that:

          {[
            let foo obj x y = ol_signal "Foo" <:obus_type< int * int >> obj (x, y)
          ]}

          The only valid use of [ol_signal] is:

          {[
            let foo = ol_signal "Foo" <:obus_type< int * int >>
          ]}

          or, with the syntax extension:

          {[
            OL_signal Foo : int * int
          ]}
      *)

    val ol_property_r : OBus_name.member ->
      ('a, _) OBus_type.cl_single ->
      (t -> 'a Lwt.t) -> unit
      (** Registers a read-only property *)

    val ol_property_w : OBus_name.member ->
      ('a, _) OBus_type.cl_single ->
      (t -> 'a -> unit Lwt.t) -> unit
      (** Registers a write-only property *)

    val ol_property_rw : OBus_name.member ->
      ('a, _) OBus_type.cl_single ->
      (t -> 'a Lwt.t) ->
      (t -> 'a -> unit Lwt.t) -> unit
      (** Registers a read and write property *)
  end
end
