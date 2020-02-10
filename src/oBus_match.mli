(*
 * oBus_match.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Matching rules *)

(** {6 Rules} *)

(** Type of an argument filter. Argument filters are used in match
    rules to match message arguments. *)
type argument_filter =
  | AF_string of string
      (** [AF_string str] matches any string argument which is equal
          to [str] *)
  | AF_string_path of string
      (** [AF_string_path path] matches any string or object-path
          argument [arg] such that one of the following conditions
          hold:

          - [arg] is equal to [path]
          - [path] ends with ['/'] and is a prefix of [arg]
          - [arg] ends with ['/'] and is a prefix of [path] *)
  | AF_namespace of string
      (** [AF_namespace namespace] matches any string argument [arg]
          such that [arg] is a bus or interface name in the namespace of
          [namespace]. For example [AF_namespace "a.b.c"] matches any
          string of the form ["a.b.c"], ["a.b.c.foo"],
          ["a.b.c.foo.bar"], ... *)

type arguments = private (int * argument_filter) list
    (** Type of lists of argument filters. The private type ensures
        that such lists are always sorted by argument number, do not
        contain duplicates and indexes are in the range [0..63].. *)

val make_arguments : (int * argument_filter) list -> arguments
  (** Creates an arguments filter from a list of filters. It raises
      [Invalid_argument] if one of the argument filters use a number
      outside of the range [1..63] *)

external cast_arguments : arguments -> (int * argument_filter) list = "%identity"
  (** Returns the list of filters for the given arguments filter. *)

(** Type of a rule used to match a message *)
type rule = {
  typ : [ `Signal | `Error | `Method_call | `Method_return ] option;
  sender : OBus_name.bus;
  interface : OBus_name.interface;
  member : OBus_name.member;
  path : OBus_path.t option;
  destination : OBus_name.bus;
  arguments : arguments;
  eavesdrop : bool option;
}

(** {8 Rule projections} *)

val typ : rule -> [ `Signal | `Error | `Method_call | `Method_return ] option
val sender : rule -> OBus_name.bus
val interface : rule -> OBus_name.interface
val member : rule -> OBus_name.member
val path : rule -> OBus_path.t option
val destination : rule -> OBus_name.bus
val arguments : rule -> arguments
val eavesdrop : rule -> bool option

(** {8 Rule construction} *)

val rule :
  ?typ : [ `Signal | `Error | `Method_call | `Method_return ] ->
  ?sender : OBus_name.bus ->
  ?interface : OBus_name.interface ->
  ?member : OBus_name.member ->
  ?path : OBus_path.t ->
  ?destination : OBus_name.bus ->
  ?arguments : arguments ->
  ?eavesdrop : bool ->
  unit -> rule
    (** Create a matching rule. *)

(** {6 Matching} *)

val match_message : rule -> OBus_message.t -> bool
  (** [match_message rule message] returns wether [message] is matched
      by [rule] *)

val match_values : arguments -> OBus_value.V.sequence -> bool
  (** [match_values filters values] returns whether [values] are
      matched by the given list of argument filters. *)

(** {6 Comparison} *)

(** Result of the comparisong of two rules [r1] and [r2]: *)
type comparison_result =
  | More_general
      (** [r1] is more general than [r2], i.e. any message matched by
          [r2] is also matched by [r1] *)
  | Less_general
      (** [r1] is less general than [r2], i.e. any message matched by
          [r1] is also matched by [r2] *)
  | Equal
      (** [r1] and [r2] are equal *)
  | Incomparable
      (** [r1] and [r2] are incomparable, i.e. there exists two
          message [m1] and [m2] such that:

          - [m1] is matched by [r1] but not by [r2]
          - [m2] is matched by [r2] but not by [r1]
      *)

val compare_rules : rule -> rule -> comparison_result
  (** [compare_rules r1 r2] compares the two matching rules [r1] and
      [r2] *)

(** {6 Parsing/printing} *)

exception Parse_failure of string * int * string
  (** [Parse_failure(string, position, reason)] is raised when parsing
      a rule failed *)

val string_of_rule : rule -> string
  (** Returns a string representation of a matching rule. *)

val rule_of_string : string -> rule
  (** Parse a string representation of a matching rule.

      @raise Failure if the given string does not contain a valid
      matching rule. *)

(** {6 Rules and message buses} *)

val export : ?switch : Lwt_switch.t -> OBus_connection.t -> rule -> unit Lwt.t
  (** [export ?switch connection rule] registers [rule] on the message
      bus. If another rule more general than [rule] is already
      exported, then it does nothihng.

      You can provide a switch to manually disable the export. *)
