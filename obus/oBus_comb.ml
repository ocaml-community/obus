(*
 * oBus_comb.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types
open OBus_wire

let ($) a b = a b
let (|>) a b x = b (a x)

type ('a, +'b, +'c) t = {
  annot : ('b, 'c) OBus_types.annot;
  reader : ('a, 'b, 'c, reader) OBus_wire.t;
  writer : 'a -> (unit, 'b, 'c, writer) OBus_wire.t;
}
type ('a, +'b, +'c) one = ('a, 'b, 'c * 'b) t
type ('a, 'b) basic_p = ('a, unit, 'b) one
constraint 'b = [< abasic ]
type ('a, 'b) single_p = ('a, unit, 'b) one
type ('a, 'b) sequence_p = ('a, unit, 'b) t

let make ~annot ~reader ~writer =
  { annot = annot;
    reader = reader;
    writer = writer }

let annot { annot = x } = x
let reader { reader = x } = x
let writer { writer = x } = x

let wrap comb f g = make (annot comb) (reader comb >>= (f |> return)) (g |> writer comb)

type ('a, 'b, 'c, 'd, 'e) func = {
  (* Input signature of the combinator *)
  signature : (unit, 'd) OBus_types.annot;

  (* Combinator used to read/write the reply of a method call *)
  reply : ('c, unit, 'e) t;

  (* If used to send a message.

     It take as argument an accumulator, which is a writer monad, and
     a continuation. [send] must construct a writer monad from the
     argument passed after and pass it to the continuation. *)
  send : (unit, unit, unit, writer) OBus_wire.t -> ((unit, unit, 'd, writer) OBus_wire.t -> 'b) -> 'a;

  (* If used to receive a message.

     [recv] is a reader which must construct a closure containing the
     value read from the input, which when applied on a function give
     these values to the function.

     i.e., suppose the combinator is of type [int -> string -> 'a],
     and we have a message containing: [1; "toto"], then when run on
     this message [recv] must return:

     [fun f -> f 1 "toto"] *)
  recv : ('a -> 'b, unit, 'd, reader) OBus_wire.t;
}

let reply comb = {
  reply = comb;
  signature = dnil;
  send = (fun acc cont -> cont acc);
  recv = return (fun f -> f);
}

let abstract { annot = annot; reader = reader; writer = writer } func = {
  reply = func.reply;
  signature = annot ++ func.signature;
  send = (fun acc cont x -> func.send (acc >> writer x) cont);
  recv = (perform
            x <-- reader;
            f <-- func.recv;
            return $ fun g -> f (g x))
}

let func_signature f = f.signature
let func_reply f = f.reply
let func_make_reader f = f.recv
let func_make_writer f = f.send (return ())
