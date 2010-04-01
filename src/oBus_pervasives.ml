(*
 * oBus_pervasives.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_value
open OBus_private_type
open OBus_type

let obus_byte = Btype {
  b_type = Tbyte;
  b_make = byte;
  b_cast = (fun context -> function
              | Byte x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_byte", signature_mismatch)));
}

let obus_char = obus_byte

let obus_int8 = map obus_byte
  (fun x -> let x = int_of_char x in
   if x >= 128 then x - 256 else x)
  (fun x -> Char.unsafe_chr (x land 0xff))

let obus_uint8 = map obus_byte
  int_of_char
  (fun x -> Char.unsafe_chr (x land 0xff))

let obus_boolean = Btype {
  b_type = Tboolean;
  b_make = boolean;
  b_cast = (fun context -> function
              | Boolean x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_boolean", signature_mismatch)));
}

let obus_bool = obus_boolean

let obus_int16 = Btype {
  b_type = Tint16;
  b_make = int16;
  b_cast = (fun context -> function
              | Int16 x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_int16", signature_mismatch)));
}

let obus_int32 = Btype {
  b_type = Tint32;
  b_make = int32;
  b_cast = (fun context -> function
              | Int32 x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_int32", signature_mismatch)));
}

let obus_int = map obus_int32 Int32.to_int Int32.of_int

let obus_int64 = Btype {
  b_type = Tint64;
  b_make = int64;
  b_cast = (fun context -> function
              | Int64 x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_int64", signature_mismatch)));
}

let obus_uint16 = Btype {
  b_type = Tuint16;
  b_make = uint16;
  b_cast = (fun context -> function
              | Uint16 x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_uint16", signature_mismatch)));
}

let obus_uint32 = Btype {
  b_type = Tuint32;
  b_make = uint32;
  b_cast = (fun context -> function
              | Uint32 x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_uint32", signature_mismatch)));
}

let obus_uint = map obus_uint32 Int32.to_int Int32.of_int

let obus_uint64 = Btype {
  b_type = Tuint64;
  b_make = uint64;
  b_cast = (fun context -> function
              | Uint64 x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_uint64", signature_mismatch)));
}

let obus_double = Btype {
  b_type = Tdouble;
  b_make = double;
  b_cast = (fun context -> function
              | Double x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_double", signature_mismatch)));
}

let obus_float = obus_double

let obus_string = Btype {
  b_type = Tstring;
  b_make = string;
  b_cast = (fun context -> function
              | String x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_string", signature_mismatch)));
}

let obus_signature = Btype {
  b_type = Tsignature;
  b_make = signature;
  b_cast = (fun context -> function
              | Signature x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_signature", signature_mismatch)));
}

let obus_object_path = Btype {
  b_type = Tobject_path;
  b_make = object_path;
  b_cast = (fun context -> function
              | Object_path x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_object_path", signature_mismatch)));
}

let obus_path = obus_object_path

let obus_broken_path = map obus_string OBus_path.of_string OBus_path.to_string

let obus_unix_file_descr = Btype {
  b_type = Tunix_fd;
  b_make = unix_fd;
  b_cast = (fun context -> function
              | Unix_fd fd -> begin
                  try
                    FDMap.find fd context.ctx_fds
                  with Not_found ->
                    let fd' = Unix.dup fd in
                    context.ctx_fds <- FDMap.add fd fd' context.ctx_fds;
                    fd'
                end
              | _ ->
                  raise (Cast_failure("OBus_pervasives.obus_unix_file_descr", signature_mismatch)));
}

let obus_file_descr = map obus_unix_file_descr Lwt_unix.of_unix_file_descr Lwt_unix.unix_file_descr

let obus_uuid = OBus_type.map obus_string OBus_uuid.of_string OBus_uuid.to_string

let obus_list elt =
  let typ = type_single elt in
  Ctype {
    c_type = Tarray typ;
    c_make = (let make = make_single elt in
              fun l -> array typ (List.map make l));
    c_cast = (let cast = _cast_single elt in
              fun context -> function
                | Array(typ', l) when typ' = typ ->
                    List.map (fun x -> cast context x) l
                | Byte_array s when typ = Tbasic Tbyte ->
                    let rec aux acc = function
                      | -1 -> acc
                      | i -> aux (cast context (sbyte (String.unsafe_get s i)) :: acc) (i - 1)
                    in
                    aux [] (String.length s - 1)
                | _ -> raise (Cast_failure("OBus_pervasives.obus_list", signature_mismatch)))
  }

let obus_array elt = map (obus_list elt) Array.of_list Array.to_list

let obus_byte_array = Ctype {
  c_type = Tarray(Tbasic Tbyte);
  c_make = byte_array;
  c_cast = (fun context -> function
              | Byte_array s -> s
              | _ -> raise (Cast_failure("OBus_pervasives.obus_byte_array", signature_mismatch)));
}

let obus_dict tyk tyv =
  let typk = type_basic tyk and typv = type_single tyv in
  Ctype {
    c_type = Tdict(typk, typv);
    c_make = (let makek = make_basic tyk and makev = make_single tyv in
              fun l -> dict typk typv (List.map (fun (k, v) -> (makek k, makev v)) l));
    c_cast = (let castk = _cast_basic tyk and castv = _cast_single tyv in
              fun context -> function
                | Dict(typk', typv', l) when typk' = typk && typv' = typv ->
                    List.map (fun (k, v) -> (castk context k, castv context v)) l
                | _ ->
                    raise (Cast_failure("OBus_pervasives.obus_dict", signature_mismatch)))
  }

let obus_structure ty = Ctype {
  c_type = Tstructure(type_sequence ty);
  c_make = (let make = make_sequence ty in
            fun x -> structure (make x));
  c_cast = (let cast = _cast_sequence ty in
            fun context -> function
              | Structure l -> cast context l
              | _ -> raise (Cast_failure("OBus_pervasives.obus_structure", signature_mismatch)));
}

let obus_variant = Ctype {
  c_type = Tvariant;
  c_make = variant;
  c_cast = (fun context -> function
              | Variant v -> v
              | _ -> raise (Cast_failure("OBus_pervasives.obus_variant", signature_mismatch)));
}

let obus_unit = Stype {
  s_type = Tnil;
  s_make = (fun () -> Tnil);
  s_cast = (fun context l -> ((), l));
}

let obus_context = Stype {
  s_type = Tnil;
  s_make = (fun _ -> Tnil);
  s_cast = (fun context l ->
              match context.ctx_user with
                | Some context -> (context, l)
                | None -> raise (Cast_failure("OBus_pervasives.context", "no context provided")));
}

type byte = char
type boolean = bool
type int8 = int
type uint8 = int
type int16 = int
type uint16 = int
type uint32 = int32
type uint64 = int64
type uint = int
type double = float
type signature = OBus_value.signature
type object_path = OBus_path.t
type path = OBus_path.t
type ('a, 'b) dict = ('a * 'b) list
type 'a structure = 'a
type variant = OBus_value.single
type byte_array = string
type file_descr = Lwt_unix.file_descr
type unix_file_descr = Unix.file_descr
type broken_path = OBus_path.t
type uuid = OBus_uuid.t
