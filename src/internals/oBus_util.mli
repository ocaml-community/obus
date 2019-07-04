(*
 * oBus_util.mli
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** This module contain various functions used by both the library and
    the tools *)

val assoc : 'a -> ('a * 'b) list -> 'b option
  (** Same as List.assoc but return an option *)

val assq : 'a -> ('a * 'b) list -> 'b option
  (** Same as List.assq but return an option *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** [find_map f l] Apply [f] on each element of [l] until it return
      [Some x] and return that result or return [None] *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] apply [f] on each element of [l] and return the
      list ef element for which [f] succeed (i.e. return [Some x]) *)

val part_map : ('a -> 'b option) -> 'a list -> 'b list * 'a list
  (** [part_map f l] apply [f] on each element of [l] and return the
      list of success and the list of failure *)

type ('a, 'b) either =
  | InL of 'a
  | InR of 'b

val split : ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
  (** Split a list *)

val map_option : 'a option -> ('a -> 'b) -> 'b option

val sha_1 : string -> string
  (** Compute the sha1 of a string *)

val hex_encode : string -> string
val hex_decode : string -> string
  (** A hex-encoded string is a string where each character is
      replaced by two hexadecimal characters which represent his ascii
      code *)

val homedir : string Lwt.t Lazy.t
  (** The home directory *)

(** {6 Random number generation} *)

(** All the following functions try to generate random numbers using
    /dev/urandom and can fallback to pseudo-random generator *)

val fill_random : bytes -> int -> int -> unit
  (** [fill_random str ofs len] Fill the given string from [ofs] to
      [ofs+len-1] with random bytes.  *)

val random_string : int -> string
val random_int : unit -> int
val random_int32 : unit -> int32
val random_int64 : unit -> int64
