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

OBUS_exception Error.NameHasNoOwner
OBUS_global_exception org.freedesktop.DBus.Error.NameHasNoOwner

let big_tuple =
  <:obus_type< int * string * uint * int32 * byte * char * int list * int * int * string * variant * signature >>
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

class virtual dbus = OBUS_interface "org.freedesktop.DBus"
  OBUS_method ListNames : string list;
  OBUS_method Truc : [{string, variant} list * int] -> string;
  OBUS_signal NameOwnerChanged : string * string * string;
  OBUS_val_rw mutable x : int;
  OBUS_property_r y : string
end

OBUS_method Plop : int -> string
OBUS_signal HaHaHa : string

OBUS_name_translator upper

OBUS_method SetCPUFreqGovernor : string

OBUS_name_translator lower

OBUS_method SetCPUFreqGovernor : string
