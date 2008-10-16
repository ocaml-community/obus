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
  (** Abstract type for context *)

val tt : t OBus_type.ty_sequence
  (** The type combinator *)

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
