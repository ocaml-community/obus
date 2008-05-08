(*
 * threadSigs.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

module type S = sig

  type ('a, 'b) if_thread =
    | With_thread of 'a
    | Without_thread of 'b

  val if_thread : (unit -> 'a) -> (unit -> 'b) -> ('a, 'b) if_thread
    (** [if_thread then else] create an if_thread value, depending on
        weather we are using thread *)

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
      (** [update p f] update the content of a protected value, [f]
          must not raise exception *)

    val safe_update : ('a -> 'a) -> 'a t -> unit
      (** same as [update] but handle the case when [f] raise
          exception *)

    val process : ('a -> ('b * 'a)) -> 'a t -> 'b
      (** [process p f] same as process but also return a value *)

    val safe_process : ('a -> ('b * 'a)) -> 'a t -> 'b
      (** same as [process] but handle the case when [f] raise
          exception *)
  end

  module ThreadConfig : sig
    val use_threads : bool
      (** [use_threads] tell if we are using threads or not *)
  end

  module Thread : sig
    type t
    val create : ('a -> 'b) -> 'a -> t
  end

  module Mutex : sig
    type t
    val create : unit -> t
    val lock : t -> unit
    val try_lock : t -> bool
    val unlock : t -> unit
  end
end
