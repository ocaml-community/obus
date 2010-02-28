(*
 * syntax_extension.ml
 * -------------------
 * Copyright : (c) 2009-2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* +-----------------------------------------------------------------+
   | Type tests                                                      |
   +-----------------------------------------------------------------+ *)

(* Functionnal type *)
let typ = <:obus_func< string -> uint -> string -> string -> string -> string list -> (string, variant) assoc -> int -> uint >>

(* Alias *)
type t = int with obus

(* Alias with type parameters *)
type ('a, 'b, 'c) t = (int * 'a list) structure * ('c, 'b) balbla with obus

module type M = sig
  (* Alias with type paramters in an interface *)
  type ('a, 'b, 'c) t = (int * 'a list) structure * ('c, 'b) balbla
    with obus(single -> basic -> basic -> container)
end

(* Automatic generation of a record combinator*)
type foo = {
  a : A.B.string;
  b : int list;
  c : (int, string, char) machin;
  d : (int * byte_array * (int, string) dict_entry set) structure * int;
} with obus

(* Tuple *)
let big_tuple =
  <:obus_type< int * string * uint * int32 * byte * char * int list * int * int * string * variant * signature >>

(* Very big tuple *)
let super_big_tuple =
  <:obus_type< x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10 * x11 * x12 * x13 * x14 * x15 * x16 * x17 * x18 * x19 * x20 * x21 * x22 * x23 * x24 * x25 * x26 * x27 * x28 * x29 * x30 * x31 * x32 * x33 * x34 * x35 * x36 * x37 * x38 * x39 * x40 * x41 * x42 >>

(* +-----------------------------------------------------------------+
   | Exceptions                                                      |
   +-----------------------------------------------------------------+ *)

exception Fatal_error of string
  with obus("org.foo.Error.FatalError")

exception Simple_error of string
  with obus(prefix ^ ".SimpleError")

(* +-----------------------------------------------------------------+
   | Proxy code                                                      |
   +-----------------------------------------------------------------+ *)

OP_interface "org.plop"

OP_method Plop : int
OP_method Plop : int -> string
OP_signal HaHaHa : string
OP_property_r Foo : int list

(* +-----------------------------------------------------------------+
   | Proxy code with a custom proxy                                  |
   +-----------------------------------------------------------------+ *)

module Proxy = OBus_proxy.Make
  (struct
     type proxy = t
     let cast x = x.proxy
     let make x = failwith "not implemented"
   end)

OP_interface(Proxy) "org.plop"

OP_method SetCPUFreqGovernor : string
OP_method MethodWithLabels : x : int -> y : int -> str : string -> unit

(* +-----------------------------------------------------------------+
   | Object code                                                     |
   +-----------------------------------------------------------------+ *)

OL_interface "org.foo" as foo

OL_method Test : int -> int
OL_method TestWithDefinition : int -> int = fun x -> x + 1
OL_signal Foo : string * string
OL_property_rw Prop : int = (fun obj -> return obj.x) (fun obj x -> obj.x <- x; return ())

(* +-----------------------------------------------------------------+
   | Tricky things with modules                                      |
   +-----------------------------------------------------------------+ *)

OL_interface "org.foo" as foo
OL_method A : int

module Bar =
struct
  OL_interface "org.bar" as bar
  OL_method B : int
end

OL_method C : int
