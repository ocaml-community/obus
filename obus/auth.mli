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

(** Handlers for an authentification mechanism *)
type mechanism_handlers = {
  mech_init : mechanism_return;
  (** must be the initial return value of the mechanism. *)

  mech_data : data -> mechanism_return;
  (** [mech_data] must continue the mechanism process with the given
      data. *)

  mech_shutdown : unit -> unit;
  (** shutdown the mechanism *)
}

val make :
  init:mechanism_return ->
  ?data:(data -> mechanism_return) ->
  ?shutdown:(unit -> unit) -> unit -> mechanism_handlers
  (** Create handers for an authentification mechanism.

      The default for [data] is to return an error telling that no
      data are expected.

      The default for [shutdown] is to do nothing. *)

type mechanism = string * (unit -> mechanism_handlers)
    (** A mechiansm consist on a mechanism name and a function to
        create the handlers *)

(** {6 Predefined mechanisms} *)

val mech_external : mechanism

(** {6 Registration} *)

val default_mechanisms : mechanism list
  (** All the default mechanisms. *)

(** {6 Make authentification} *)

val launch : ?mechanisms:mechanism list -> Transport.t -> Address.guid option
  (** [launch transport] launch authentification on the given
      transport *)
