(*
 * oBus_path.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of dbus object paths *)

type t = string
    (** A complete path *)

type elt = string
    (** A path component *)

(** {6 Construction} *)

val empty : t
  (** empty path *)

val append : t -> elt -> t
val (/) : t -> elt -> t
  (** [append path x] append [x] to [t]. [x] must not contain ['/'] *)

val split : t -> elt list
  (** return all components of a path *)

val make : elt list -> t
  (** make a path from a list of component *)

(** {6 Combinators (for the syntax extension)} *)

val obr_t : (t, _, OBus_wire.dobject_path) OBus_conv.rsingle
val obw_t : (t, _, OBus_wire.dobject_path) OBus_conv.wsingle
