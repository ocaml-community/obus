let x = <:obus_func_type< string -> uint -> string -> string -> string -> string list -> (string, variant) assoc -> int -> uint >>

OBUS_type t = int

OBUS_type ('a, 'b, 'c) t = [int * 'a list] * ('c, 'b) balbla

OBUS_bitwise request_name_flag : uint =
  [ 1 -> `allow_replacement
  | 2 -> `replace_existing
  | 4 -> `do_not_queue ];;

OBUS_flag request_name_result : uint =
  [ 1 -> `primary_owner
  | 2 -> `in_queue
  | 3 -> `exists
  | 4 -> `already_owner ]

OBUS_exn Name_has_owner = "Error.NameHasNoOwner"
OBUS_global_exn Name_has_owner = "org.freedesktop.DBus.Error.NameHasNoOwner"

let big_tuple =
  <:obus_type< int * $let x = OBus_type.tstring in x$ * uint * int32 * byte * char * int list * int * int * string * variant * signature >>
;;

let other_sugars =
  <:obus_type< [ int * string * int ] * {int, string} set >>
;;

OBUS_record toto = {
  a: A.B.string;
  b: int list;
  c: (int, string, char) machin;
  d: [int * byte_array * {int, string} set] * int;
}

OBUS_struct ('a, 'b) coord = { x: 'a; y: 'b }

OBUS_struct 'a x = { x: 'a }
