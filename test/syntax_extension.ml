
let x = <:obus_func< string -> uint -> string -> string -> string -> string list -> (string, variant) assoc -> int -> uint >>

type t = int with obus

type ('a, 'b, 'c) t = (int * 'a list) structure * ('c, 'b) balbla
  with obus

exception Fatal_error of string
  with obus(prefix ^ ".Error.FatalError")

let big_tuple =
  <:obus_type< int * string * uint * int32 * byte * char * int list * int * int * string * variant * signature >>

let super_big_tuple =
  <:obus_type< x0 * x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10 * x11 * x12 * x13 * x14 * x15 * x16 * x17 * x18 * x19 * x20 * x21 * x22 * x23 * x24 * x25 * x26 * x27 * x28 * x29 * x30 * x31 * x32 * x33 * x34 * x35 * x36 * x37 * x38 * x39 * x40 * x41 * x42 >>

type toto = {
  a : A.B.string;
  b : int list;
  c : (int, string, char) machin;
  d : (int * byte_array * (int, string) dict_entry set) structure * int;
} with obus

OP_method Plop : int -> string
OP_signal HaHaHa : string
OP_property_r Foo : int list

OBUS_name_translator "ocaml"

OP_method SetCPUFreqGovernor : string

OBUS_name_translator "haskell"

OP_method SetCPUFreqGovernor : string

OP_method MethodWithLabels : x : int -> y : int -> string -> unit

OP_method Foo : x : int -> y : int -> int
OL_property_rw Prop : int = (fun obj -> return obj.x) (fun obj x -> obj.x <- x; return ())
