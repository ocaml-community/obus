let x = <:obtyf< string -> uint -> string -> string -> string -> string list -> (string, variant) assoc -> int -> uint >>

OBUS_BITWISE request_name_flag : uint =
  [ 1 -> `allow_replacement
  | 2 -> `replace_existing
  | 4 -> `do_not_queue ];;

OBUS_FLAG request_name_result : uint =
  [ 1 -> `primary_owner
  | 2 -> `in_queue
  | 3 -> `exists
  | 4 -> `already_owner ]

OBUS_EXN Name_has_owner = "Error.NameHasNoOwner"
OBUS_GLOBAL_EXN Name_has_owner = "org.freedesktop.DBus.Error.NameHasNoOwner"

let big_tuple =
  <:obty< int * string * uint * int32 * byte * char * int list * int * int * string * variant * signature >>
