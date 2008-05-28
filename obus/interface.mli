(*
 * interface.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of dbus interfaces *)

type name = string

type 'a t
  (** Abstract type representing an interface *)

val name : 'a t -> name
  (** [name interface] get the name of an interface *)

(** Signature of an interface *)

type value = string
type annotation = Annotation of name * value
type argument = Arg of name * Values.dtype
type access = Read | Write | Read_write
type definition =
  | Method of name * (*in*) argument list * (*out*) argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * Values.dtype * access * annotation list
type signature = Interface of name * definition list * annotation list

val print_xml : Buffer.t -> signature -> unit
  (** Marshal a interface into introspection data *)

(** For auto-generated code *)

type property_handlers = {
  property_set : string -> Header.recv -> string -> int -> unit;
  property_get : string -> Header.recv -> string -> int -> unit;
  property_getall : Header.recv -> string -> int -> unit;
}

type handlers = {
  method_call : Header.recv -> string -> int -> bool;
  (* Note: the interface method call handler must return [true] if it
     can handle the message, otherwise it return [false]. This is used
     for method which do not have an interface field since this is not
     considered to be an error *)
  introspecter : string -> (signature * string list) option;
  (* an introspection handler return a interface signature and sons
     if the object with given path has this interface *)
  property : property_handlers option;
  (* If not none, object having this interface will also have
     org.freedesktop.DBus.Properties *)
}

val make_interface_for_proxy : 'a -> name -> 'a t
val make_interface_for_server : 'a -> name -> handlers -> 'a t
val get_handlers : 'a t -> handlers
