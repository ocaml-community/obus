(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Exported DBus objects *)

(** {6 Interface description} *)

(** Recquirement for a DBus object *)
class t : object
  method path : OBus_path.t
    (** DBus path of the object *)

  method handle_call : OBus_connection.t -> OBus_message.method_call -> bool
    (** Handle a method call on the object. It must return:

        - [true] if the call has been handled, i.e. a method_return or
        an error has been sent to the caller (or [no_auto_start] is
        set and no further processing is needed)

        - [false] if not *)
end

val expose : OBus_connection.t -> #t -> unit
  (** [expose connection obj] Expose an object on a connection. If an
      object with the same path than [obj] already exists it is
      replaced. *)

val remove : OBus_connection.t -> #t -> unit
  (** [remove connection obj] Remove the following object from a
      connection. Do nothing if the object is not exported on the
      connection. *)

val remove_by_path : OBus_connection.t -> OBus_path.t -> unit
  (** [remove_by_path connection path] Remove any object with [path]
      as path on the connection *)

