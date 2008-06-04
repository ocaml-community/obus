(*
 * solver.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** This module is used to generate serialization and deserialization
    functions. *)

type ('eqn, 'sol) result
  (** Result of a rule *)

type ('eqn, 'sol) rule = 'eqn -> ('eqn, 'sol) result
  (** A rule is a function which take an equation, and either fail,
      succeed and return immediately a value or generate a list of
      dependencies. *)

(** The following functions must be used to construct the result of a
    rule *)

val fail : ('eqn, 'sol) result
  (** [fail] means that the rule failed immediately *)

val success : 'sol -> ('eqn, 'sol) result
  (** [success sol] means that the rule succeed immediately with
      solution [sol] *)

val dep : ('eqn, 'sol, 'sol -> 'a, 'sol) Seq.t -> ('sol -> 'a) -> ('eqn, 'sol) result
  (** [dep deps f] means that the rule generate new dependencies *)

val solve : ?printer:('eqn -> string) -> ('eqn, 'sol) rule list -> 'eqn -> 'sol option
  (** [solve printer rules eqn] try to solve [eqn] with the given set
      of rules. The solution is minimal in term of the depth of
      applied rules.

      If [printer] is provided then a trace of the resolution is
      printed on the standart output. *)
