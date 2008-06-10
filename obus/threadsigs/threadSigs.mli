(*
 * threadSigs.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

module type S = sig
  module Protected : sig
    type 'a t
      (** A mutable value protected by a mutex *)

    val make : 'a -> 'a t
      (** [make v] create a new protected value *)

    val set : 'a t -> 'a -> unit
      (** [set p v] change the content of a protected value *)

    val get : 'a t -> 'a
      (** [get p] get the content of a protected value *)

    val update : ('a -> 'a) -> 'a t -> unit
      (** [update f p] update the content of a protected value and
          handle the case when [f] raise exception *)

    val process : ('a -> ('b * 'a)) -> 'a t -> 'b
      (** [process f p] same as update but also return a value. *)

    val with_value : ('a -> 'b) -> 'a t -> 'b
      (** [with_value f p] use the content of a protected value with
          [f] *)

    val if_none : 'a option t -> (unit -> 'a) -> 'a
      (** [if_none p f] return the content of proctected
          option. Retreive its value with [f] if it is [None] *)
  end

  module ThreadConfig : sig
    val use_threads : bool
      (** [use_threads] tell if we are using threads or not *)
  end

  module Thread : sig
    type t
    val create : ('a -> 'b) -> 'a -> t
    val self : unit -> t
    val id : t -> int
  end

  module Mutex : sig
    type t
    val create : unit -> t
    val lock : t -> unit
    val try_lock : t -> bool
    val unlock : t -> unit
  end

  val with_lock : Mutex.t -> (unit -> 'a) -> 'a
    (** [with_lock m f] execute [f] while [m] is acquired and release
        it after even if [f] raise an exception *)
end
