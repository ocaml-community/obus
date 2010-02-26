(*
 * oBus_match.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Matching rules *)

(** Type of an argument filter. Argument filters are used in match
    rules to match message arguments. *)
type argument_filter =
  | AF_string of string
      (** [AF_string str] matches any string argument which is equal
          to [str] *)
  | AF_string_path of string
      (** [AF_string_path path] matches any string argument [arg] such
          that one of the following conditions hold:

          - [arg] is equal to [path]
          - [path] ends with ['/'] and is a prefix of [arg]
          - [arg] ends with ['/'] and is a prefix of [path] *)

(** Type of a rule used to match a message *)
type rule = private {
  typ : [ `Signal | `Error | `Method_call | `Method_return ] option;
  sender : OBus_name.bus option;
  interface : OBus_name.interface option;
  member : OBus_name.member option;
  path : OBus_path.t option;
  destination : OBus_name.bus option;
  arguments : (int * argument_filter) list;
  (** [arguments] is always a sorted list. *)
} with projection, obus(basic)

val rule :
  ?typ : [ `Signal | `Error | `Method_call | `Method_return ] ->
  ?sender : OBus_name.bus ->
  ?interface : OBus_name.interface ->
  ?member : OBus_name.member ->
  ?path : OBus_path.t ->
  ?destination : OBus_name.bus ->
  ?arguments : (int * argument_filter) list ->
  unit -> rule
    (** Create a matching rule. It raises [Invalid_argument] if one of
        the argument filters use a number outside of the range
        [1..63] *)

val match_message : rule -> OBus_message.t -> bool
  (** [match_message rule message] returns wether [message] is matched
      by [rule] *)

exception Parse_failure of string * int * string
  (** [Parse_failure(string, position, reason)] is raised when parsing
      a rule failed *)

val string_of_rule : rule -> string
  (** Return a string representation of a matching rule. *)

val rule_of_string : string -> rule
  (** Parse a string representation of a matching rule.

      @raise [Failure] if the given string does not contain a valid
      matching rule. *)
