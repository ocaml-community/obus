(*
 * auth.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handle authentification mechanisms *)

type data = string
    (** Data for an auth mechanism *)

type mechanism_return =
    (** Value returned by an auth mechanism *)
  | Continue of data
      (** Continue the authentification with this data *)
  | OK of data
      (** Authentification done *)
  | Error of string
      (** Authentification failed *)

class type mechanism =
object
  (** A mechanism for authentification *)

  method init : mechanism_return
    (** [init] initialize the mechanism *)
  method data : data -> mechanism_return
    (** [data d] continue the mechanism with this data *)
  method shutdown : unit
    (** [shutdown] shutdown mechanism *)
end

class virtual immediate :
object
  (** Immediate authentification *)
  method virtual init : mechanism_return
  method data : data -> mechanism_return
  method shutdown : unit
end

(** {6 Registration} *)

val register_mechanism : string -> (unit -> mechanism) -> unit
  (** [resgister_mechanism name mech] add support for a new mechanism. *)

(** {6 Make authentification} *)

val launch : Transport.t -> Address.guid option
  (** [launch transport] launch authentification on the given
      transport *)
