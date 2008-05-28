(*
 * wire_backend_ml.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module LEWriter =
struct
  DEFINE BIT(count, n) = n;
  INCLUDE "obus/wire_writers.ml"
end

module BEWriter =
struct
  DEFINE BIT(count, n) = count/8 - n;
  INCLUDE "obus/wire_writers.ml"
end

module LEReader =
struct
  DEFINE BIT(count, n) = n;
  INCLUDE "obus/wire_readers.ml"
end

module BEReader =
struct
  DEFINE BIT(count, n) = count/8 - n;
  INCLUDE "obus/wire_readers.ml"
end

let native_byte_order () = 0
let string_match buffer i str len =
  let rec aux = function
    | -1 -> true
    | j when String.unsafe_get buffer (i + j) = String.unsafe_get str j ->
        aux (j - 1)
    | _ -> false
  in
    aux (len - 1)
