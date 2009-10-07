(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus objects *)

type t
  (** Type of obus object *)

val path : t -> OBus_path.t
  (** [path obj] returns the path of the object *)

val make : OBus_path.t -> t
  (** [make path] creates a new object with path [path] *)

val make' : unit -> t
  (** Same as [make] but generate a unique path *)

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
  val obus_t : Object.obj OBus_type.basic
    (** The type combinator *)

  val export : OBus_connection.t -> Object.obj -> unit
    (** [export connection obj] exports [obj] on [connection] *)

  val emit : Object.obj ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence ->
    ?peer : OBus_peer.t -> 'a -> unit Lwt.t
    (** [emit obj ~interface ~member typ ?peer x] emits a signal. If
        [peer] is specified then the signal is sent only to it, otherwise
        it is broadcasted. *)

  (** This functor is aimed to be used with the syntax extension. *)
  module MakeInterface(Name : OBus_interface.Name) : sig
    val ol_interface : OBus_name.interface
      (** Name of the interface *)

    val ol_method_call : OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.func -> (Object.obj -> 'a) -> unit
      (** Registers a method call *)

    val ol_signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> (Object.obj -> ?peer : OBus_peer.t -> 'a -> unit Lwt.t)
      (** Registers a signal and define the signal emiting
          function. *)

    val ol_property_r : OBus_name.member ->
      ('a, _) OBus_type.cl_single ->
      (Object.obj -> 'a Lwt.t) -> unit
      (** Registers a read-only property *)

    val ol_property_w : OBus_name.member ->
      ('a, _) OBus_type.cl_single ->
      (Object.obj -> 'a -> unit Lwt.t) -> unit
      (** Registers a write-only property *)

    val ol_property_rw : OBus_name.member ->
      ('a, _) OBus_type.cl_single ->
      (Object.obj -> 'a Lwt.t) ->
      (Object.obj -> 'a -> unit Lwt.t) -> unit
      (** Registers a read and write property *)
  end
end
