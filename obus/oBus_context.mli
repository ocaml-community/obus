(*
 * oBus_context.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Method call context *)

(** When handling a method call it can be usefull to retreive more
    information about the context, like for example:

    - from which connection the call come
    - from which sender
    - ...

    To do this we just have to add [OBus_context.t] in the method name
    and the context will automatically be passed to the method,
    without modifying the signature:

    {[
      OBUS_method Foo : OBus_context.t -> int -> string
    ]}
*)

type t
val tt : t OBus_type.ty_sequence
  (** Abstract type for context *)

(** {6 Context informations} *)

val connection : t -> OBus_connection.t
  (** Connection from which come the call *)

val sender : t -> OBus_name.connection option
  (** Sender of the call *)

val destination : t -> OBus_name.connection option
  (** Name used to contact us *)

val path : t -> OBus_path.t
  (** Path used to access to the object *)

val interface : t -> OBus_name.interface option
  (** Interface used to access the method *)

val message : t -> OBus_message.method_call
  (** The raw method call *)

(** {6 Type combinators} *)

(** It is also possible to retreive directly one single information
    with the following types combinators *)

type connection = OBus_connection.t
val tconnection : OBus_connection.t OBus_type.ty_sequence

type sender = OBus_name.connection option
val tsender : OBus_name.connection option OBus_type.ty_sequence

type destination = OBus_name.connection option
val tdestination : OBus_name.connection option OBus_type.ty_sequence

type path = OBus_path.t
val tpath : OBus_path.t OBus_type.ty_sequence

type interface = OBus_name.interface option
val tinterface : OBus_name.interface option OBus_type.ty_sequence

type message = OBus_message.method_call
val tmessage : OBus_message.method_call OBus_type.ty_sequence
