(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Signals definition *)

(** A signal definition. ['a] is the type of signals contents. *)
class type ['a] t = object
  method event : 'a React.event
    (** The signal itself *)

  method disconnect : unit
    (** Stop receiving the signal *)
end

val connect : OBus_proxy.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, _) OBus_type.cl_sequence -> 'a t
  (** [connect proxy ~interface ~member typ] connect to
      given signals emitted by [proxy]. *)

val dyn_connect : OBus_proxy.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member -> OBus_value.sequence t
  (** Same thing but return signals as a dynamically typed values *)
