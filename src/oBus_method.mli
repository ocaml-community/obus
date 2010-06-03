(*
 * oBus_method.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus methods *)

(** {6 Calling methods} *)

exception Invalid_reply of string
  (** Exception raised when the signature of the reply to a method
      call does not match the expected signature. The argument is an
      error message. *)

val call : ('a, 'b) OBus_member.Method.t -> OBus_proxy.t -> 'a -> 'b Lwt.t
  (** [call meth proxy args] calls the method [meth] on the object
      pointed by [proxy], and wait for the reply. *)

val call_with_context : ('a, 'b) OBus_member.Method.t -> OBus_proxy.t -> 'a -> (OBus_context.void OBus_context.t * 'b) Lwt.t
  (** [call_with_context meth proxy args] is like {!call} except that
      is also returns the context of the method return *)

val call_no_reply : ('a, 'b) OBus_member.Method.t -> OBus_proxy.t -> 'a -> unit Lwt.t
  (** [call_no_reply meth proxy args] is the same as {!call} except
      that it does not wait for a reply *)

(** {6 Sending replies} *)

val return : 'a OBus_context.t -> 'a -> [> `Replied ] Lwt.t
  (** [return ~context args] sends a reply using the given context *)

val fail : 'a OBus_context.t -> OBus_error.name -> OBus_error.message -> [> `Replied ] Lwt.t
  (** [fail ~context name message] sends an error using the given
      context. *)
