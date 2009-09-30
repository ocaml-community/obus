(*
 * oBus_lowlevel.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus_constant
open OBus_value
open OBus_message

exception Data_error of string
exception Protocol_error of string

type byte_order = Little_endian | Big_endian
let native_byte_order = match OBus_config.native_byte_order with
  | `little_endian -> Little_endian
  | `big_endian -> Big_endian

let padding2 i = i land 1
let padding4 i = (4 - i) land 3
let padding8 i = (8 - i) land 7

let pad2 i = i + padding2 i
let pad4 i = i + padding4 i
let pad8 i = i + padding8 i

let pad8_p = function
  | Tstructure _
  | Tbasic Tint64
  | Tbasic Tuint64
  | Tbasic Tdouble -> true
  | _ -> false

(* Common error message *)
let array_too_big len = sprintf "array size exceed the limit: %d" len
let message_too_big len = sprintf "message size exceed the limit: %d" len
let signature_too_long s len = sprintf "too long signature: '%s', with len %d" (string_of_signature s) len
let invalid_protocol_version ver = sprintf "invalid protocol version: %d (obus implement protocol version %d)" ver OBus_info.protocol_version
let invalid_byte_order ch = sprintf "invalid byte order(%C)" ch

(* +-----------------------------------------------------------------+
   | Raw description of header fields                                |
   +-----------------------------------------------------------------+ *)

type raw_fields = {
  mutable rf_path : OBus_path.t option;
  mutable rf_member : OBus_name.member option;
  mutable rf_interface : OBus_name.interface option;
  mutable rf_error_name : OBus_name.error option;
  mutable rf_reply_serial : serial option;
  mutable rf_destination : OBus_name.bus option;
  mutable rf_sender : OBus_name.bus option;
  mutable rf_signature : signature;
}

let path = ("path", fun x -> x.rf_path)
let member = ("member", fun x -> x.rf_member)
let interface = ("interface", fun x -> x.rf_interface)
let error_name = ("error_name", fun x -> x.rf_error_name)
let reply_serial = ("reply_serial", fun x -> x.rf_reply_serial)

let get_required message_type_name (field_name, get_field) fields =
  match get_field fields with
    | Some v -> v
    | None -> raise (Protocol_error(sprintf "invalid header, field '%s' is required for '%s'"
                                      field_name message_type_name))

let method_call_of_raw fields =
  let req x = get_required "method_call" x in
  Method_call(req path fields,
              fields.rf_interface,
              req member fields)

let method_return_of_raw fields =
  let req x = get_required "method_return" x in
  Method_return(req reply_serial fields)

let error_of_raw fields =
  let req x = get_required "error" x in
  Error(req reply_serial fields,
        req error_name fields)

let signal_of_raw fields =
  let req x = get_required "signal" x in
  Signal(req path fields,
         req interface fields,
         req member fields)

(* +-----------------------------------------------------------------+
   | Error mapping                                                   |
   +-----------------------------------------------------------------+ *)

(* Maps error returned by [OBus_*.*] to [Data_error] or
   [Protocol_error]: *)

let map_exn f = function
  | Failure msg ->
      let i = String.index msg ':' in
      raise (f (String.sub msg (i + 1) (String.length msg - i - 1)))
  | OBus_string.Invalid_string err ->
      raise (f (OBus_string.error_message err))
  | exn ->
      raise exn

let data_error msg = Data_error msg
let protocol_error msg = Protocol_error msg

(* +-----------------------------------------------------------------+
   | Message size calculation                                        |
   +-----------------------------------------------------------------+ *)

module Size =
struct
  let path_length = function
    | [] -> 1
    | l -> List.fold_left (fun acc x -> 1 + String.length x + acc) 0 l

  (* Each function takes an argument [i] which represent an offset, it
     computes the offset after writing what they have to then return
     it. *)

  let rec tsingle i = function
    | Tbasic _ -> i + 1
    | Tarray t -> tsingle (i + 1) t
    | Tdict(tk, tv) -> tsingle (i + 4) tv
    | Tstructure l -> List.fold_left tsingle (i + 2) l
    | Tvariant -> i + 1

  let tsequence i l = List.fold_left tsingle i l

  let rec tsingle_of_single i = function
    | Basic x -> i + 1
    | Array(t, x) -> tsingle (i + 1) t
    | Byte_array _ -> i + 2
    | Dict(tk, tv, x) -> tsingle (i + 4) tv
    | Structure l -> List.fold_left tsingle_of_single (i + 2) l
    | Variant x -> i + 1

  let tsequence_of_sequence i l = List.fold_left tsingle_of_single i l

  let rec basic i = function
    | Byte _ -> i + 1
    | Int16 _
    | Uint16 _ -> pad2 i + 2
    | Boolean _
    | Int32 _
    | Uint32 _ -> pad4 i + 4
    | Int64 _
    | Uint64 _
    | Double _ -> pad8 i + 8
    | String s -> pad4 i + String.length s + 5
    | Signature s -> tsequence (i + 2) s
    | Object_path p -> pad4 i + path_length p + 5

  let rec single i = function
    | Basic x ->
        basic i x
    | Array(t, l) ->
        let i = pad4 i + 4 in
        let i = if pad8_p t then pad8 i else i in
        List.fold_left single i l
    | Byte_array bytes ->
        pad4 i + 4 + String.length bytes
    | Dict(tk, tv, l) ->
        let i = pad4 i + 4 in
        let i = pad8 i in
        List.fold_left (fun i (k, v) -> single (basic (pad8 i) k) v) i l
    | Structure l ->
        List.fold_left single (pad8 i) l
    | Variant x ->
        let i = 1 + tsingle_of_single i x + 1 in
        single i x

  let sequence i l = List.fold_left single i l

  let message msg =
    let i = 16 in
    let i = match msg.typ with
      | Method_call(path, None, member) ->
          (* +9 for:
             - the code (1)
             - the signature of one basic type code (3)
             - the string length (4)
             - the null byte (1) *)
          let i = pad8 i + 9 + path_length path in
          pad8 i + 9 + String.length member
      | Method_call(path, Some interface, member)
      | Signal(path, interface, member) ->
          let i = pad8 i + 9 + path_length path in
          let i = pad8 i + 9 + String.length interface in
          pad8 i + 9 + String.length member
      | Method_return serial ->
          pad8 i + 4
      | Error(serial, name) ->
          let i = pad8 i + 9 + String.length name in
          pad8 i + 4
    in
    let i = match msg.destination with
      | None -> i
      | Some destination -> pad8 i + 9 + String.length destination
    in
    let i = match msg.sender with
      | None -> i
      | Some sender -> pad8 i + 9 + String.length sender
    in
    let i = tsequence_of_sequence (pad8 i + 6) msg.body in
    sequence (pad8 i) msg.body
end

(* +-----------------------------------------------------------------+
   | Unsafe writing of integers                                      |
   +-----------------------------------------------------------------+ *)

let put_char = String.unsafe_set
let put_uint8 buf ofs x = put_char buf ofs (Char.unsafe_chr x)

module type Integer_writers = sig
  val put_int16 : string -> int -> int -> unit
  val put_int32 : string -> int -> int32 -> unit
  val put_int64 : string -> int -> int64 -> unit
  val put_uint16 : string -> int -> int -> unit
  val put_uint32 : string -> int -> int32 -> unit
  val put_uint64 : string -> int -> int64 -> unit

  val put_uint : string -> int -> int -> unit
end

module LE_integer_writers : Integer_writers =
struct
  let put_int16 buf ofs v =
    put_uint8 buf (ofs + 0) v;
    put_uint8 buf (ofs + 1) (v lsr 8)
  let put_uint16 = put_int16

  let put_int32 buf ofs v =
    put_uint8 buf (ofs + 0) (Int32.to_int v);
    put_uint8 buf (ofs + 1) (Int32.to_int (Int32.shift_right v 8));
    put_uint8 buf (ofs + 2) (Int32.to_int (Int32.shift_right v 16));
    put_uint8 buf (ofs + 3) (Int32.to_int (Int32.shift_right v 24))
  let put_uint32 = put_int32

  let put_int64 buf ofs v =
    put_uint8 buf (ofs + 0) (Int64.to_int v);
    put_uint8 buf (ofs + 1) (Int64.to_int (Int64.shift_right v 8));
    put_uint8 buf (ofs + 2) (Int64.to_int (Int64.shift_right v 16));
    put_uint8 buf (ofs + 3) (Int64.to_int (Int64.shift_right v 24));
    put_uint8 buf (ofs + 4) (Int64.to_int (Int64.shift_right v 32));
    put_uint8 buf (ofs + 5) (Int64.to_int (Int64.shift_right v 40));
    put_uint8 buf (ofs + 6) (Int64.to_int (Int64.shift_right v 48));
    put_uint8 buf (ofs + 7) (Int64.to_int (Int64.shift_right v 56))
  let put_uint64 = put_int64

  let put_uint buf ofs v =
    put_uint8 buf (ofs + 0) v;
    put_uint8 buf (ofs + 1) (v lsr 8);
    put_uint8 buf (ofs + 2) (v lsr 16);
    put_uint8 buf (ofs + 3) (v asr 24)
end

module BE_integer_writers : Integer_writers =
struct
  let put_int16 buf ofs v =
    put_uint8 buf (ofs + 0) (v lsr 8);
    put_uint8 buf (ofs + 1) v
  let put_uint16 = put_int16

  let put_int32 buf ofs v =
    put_uint8 buf (ofs + 0) (Int32.to_int (Int32.shift_right v 24));
    put_uint8 buf (ofs + 1) (Int32.to_int (Int32.shift_right v 16));
    put_uint8 buf (ofs + 2) (Int32.to_int (Int32.shift_right v 8));
    put_uint8 buf (ofs + 3) (Int32.to_int v)
  let put_uint32 = put_int32

  let put_int64 buf ofs v =
    put_uint8 buf (ofs + 0) (Int64.to_int (Int64.shift_right v 56));
    put_uint8 buf (ofs + 1) (Int64.to_int (Int64.shift_right v 48));
    put_uint8 buf (ofs + 2) (Int64.to_int (Int64.shift_right v 40));
    put_uint8 buf (ofs + 3) (Int64.to_int (Int64.shift_right v 32));
    put_uint8 buf (ofs + 4) (Int64.to_int (Int64.shift_right v 24));
    put_uint8 buf (ofs + 5) (Int64.to_int (Int64.shift_right v 16));
    put_uint8 buf (ofs + 6) (Int64.to_int (Int64.shift_right v 8));
    put_uint8 buf (ofs + 7) (Int64.to_int v)
  let put_uint64 = put_int64

  let put_uint buf ofs v =
    put_uint8 buf (ofs + 0) (v asr 24);
    put_uint8 buf (ofs + 1) (v lsr 16);
    put_uint8 buf (ofs + 2) (v lsr 8);
    put_uint8 buf (ofs + 3) v
end

(* +-----------------------------------------------------------------+
   | Unsafe reading of integers                                      |
   +-----------------------------------------------------------------+ *)

let get_char = String.unsafe_get
let get_uint8 buf ofs = Char.code (get_char buf ofs)

module type Integer_readers = sig
  val get_int16 : string -> int -> int
  val get_int32 : string -> int -> int32
  val get_int64 : string -> int -> int64
  val get_uint16 : string -> int -> int
  val get_uint32 : string -> int -> int32
  val get_uint64 : string -> int -> int64

  val get_uint : string -> int -> int
end

module LE_integer_readers : Integer_readers =
struct
  let get_int16 buf ofs =
    let v0 = get_uint8 buf (ofs + 0)
    and v1 = get_uint8 buf (ofs + 1) in
    let v = v0 lor (v1 lsl 8) in
    if v land (1 lsl 15) = 0 then
      v
    else
      ((-1 land (lnot 0x7fff)) lor v)

  let get_uint16 buf ofs =
    let v0 = get_uint8 buf (ofs + 0)
    and v1 = get_uint8 buf (ofs + 1) in
    (v0 lor (v1 lsl 8))

  let get_int32 buf ofs =
    let v0 = get_uint8 buf (ofs + 0)
    and v1 = get_uint8 buf (ofs + 1)
    and v2 = get_uint8 buf (ofs + 2)
    and v3 = get_uint8 buf (ofs + 3) in
    (Int32.logor
       (Int32.logor
          (Int32.of_int v0)
          (Int32.shift_left (Int32.of_int v1) 8))
       (Int32.logor
          (Int32.shift_left (Int32.of_int v2) 16)
          (Int32.shift_left (Int32.of_int v3) 24)))
  let get_uint32 = get_int32

  let get_int64 buf ofs =
    let v0 = get_uint8 buf (ofs + 0)
    and v1 = get_uint8 buf (ofs + 1)
    and v2 = get_uint8 buf (ofs + 2)
    and v3 = get_uint8 buf (ofs + 3)
    and v4 = get_uint8 buf (ofs + 4)
    and v5 = get_uint8 buf (ofs + 5)
    and v6 = get_uint8 buf (ofs + 6)
    and v7 = get_uint8 buf (ofs + 7) in
    (Int64.logor
       (Int64.logor
          (Int64.logor
             (Int64.of_int v0)
             (Int64.shift_left (Int64.of_int v1) 8))
          (Int64.logor
             (Int64.shift_left (Int64.of_int v2) 16)
             (Int64.shift_left (Int64.of_int v3) 24)))
       (Int64.logor
          (Int64.logor
             (Int64.shift_left (Int64.of_int v4) 32)
             (Int64.shift_left (Int64.of_int v5) 40))
          (Int64.logor
             (Int64.shift_left (Int64.of_int v6) 48)
             (Int64.shift_left (Int64.of_int v7) 56))))
  let get_uint64 = get_int64

  let get_uint buf ofs =
    let v0 = get_uint8 buf (ofs + 0)
    and v1 = get_uint8 buf (ofs + 1)
    and v2 = get_uint8 buf (ofs + 2)
    and v3 = get_uint8 buf (ofs + 3) in
    (v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24))
end

module BE_integer_readers : Integer_readers =
struct
  let get_int16 buf ofs =
    let v1 = get_uint8 buf (ofs + 0)
    and v0 = get_uint8 buf (ofs + 1) in
    let v = v0 lor (v1 lsl 8) in
    if v land (1 lsl 15) = 0 then
      v
    else
      ((-1 land (lnot 0x7fff)) lor v)

  let get_uint16 buf ofs =
    let v1 = get_uint8 buf (ofs + 0)
    and v0 = get_uint8 buf (ofs + 1) in
    (v0 lor (v1 lsl 8))

  let get_int32 buf ofs =
    let v3 = get_uint8 buf (ofs + 0)
    and v2 = get_uint8 buf (ofs + 1)
    and v1 = get_uint8 buf (ofs + 2)
    and v0 = get_uint8 buf (ofs + 3) in
    (Int32.logor
       (Int32.logor
          (Int32.of_int v0)
          (Int32.shift_left (Int32.of_int v1) 8))
       (Int32.logor
          (Int32.shift_left (Int32.of_int v2) 16)
          (Int32.shift_left (Int32.of_int v3) 24)))
  let get_uint32 = get_int32

  let get_int64 buf ofs =
    let v7 = get_uint8 buf (ofs + 0)
    and v6 = get_uint8 buf (ofs + 1)
    and v5 = get_uint8 buf (ofs + 2)
    and v4 = get_uint8 buf (ofs + 3)
    and v3 = get_uint8 buf (ofs + 4)
    and v2 = get_uint8 buf (ofs + 5)
    and v1 = get_uint8 buf (ofs + 6)
    and v0 = get_uint8 buf (ofs + 7) in
    (Int64.logor
       (Int64.logor
          (Int64.logor
             (Int64.of_int v0)
             (Int64.shift_left (Int64.of_int v1) 8))
          (Int64.logor
             (Int64.shift_left (Int64.of_int v2) 16)
             (Int64.shift_left (Int64.of_int v3) 24)))
       (Int64.logor
          (Int64.logor
             (Int64.shift_left (Int64.of_int v4) 32)
             (Int64.shift_left (Int64.of_int v5) 40))
          (Int64.logor
             (Int64.shift_left (Int64.of_int v6) 48)
             (Int64.shift_left (Int64.of_int v7) 56))))
  let get_uint64 = get_int64

  let get_uint buf ofs =
    let v3 = get_uint8 buf (ofs + 0)
    and v2 = get_uint8 buf (ofs + 1)
    and v1 = get_uint8 buf (ofs + 2)
    and v0 = get_uint8 buf (ofs + 3) in
    (v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24))
end

(* A pointer, used to serialize or unserialize data *)
type pointer = {
  buf : string;
  mutable ofs : int;
  max : int;
}

(* +---------------------------------------------------------------+
   | Common writing functions                                      |
   +---------------------------------------------------------------+ *)

let write_padding2 ptr =
  if pad2 ptr.ofs = 1 then begin
    put_uint8 ptr.buf ptr.ofs 0;
    ptr.ofs <- ptr.ofs + 1
  end

let write_padding4 ptr =
  for k = 1 to padding4 ptr.ofs do
    put_uint8 ptr.buf ptr.ofs 0;
    ptr.ofs <- ptr.ofs + 1
  done

let write_padding8 ptr =
  for k = 1 to padding8 ptr.ofs do
    put_uint8 ptr.buf ptr.ofs 0;
    ptr.ofs <- ptr.ofs + 1
  done

let write1 writer ptr value =
  writer ptr.buf ptr.ofs value;
  ptr.ofs <- ptr.ofs + 1

let write2 writer ptr value =
  write_padding2 ptr;
  writer ptr.buf ptr.ofs value;
  ptr.ofs <- ptr.ofs + 2

let write4 writer ptr value =
  write_padding4 ptr;
  writer ptr.buf ptr.ofs value;
  ptr.ofs <- ptr.ofs + 4

let write8 writer ptr value =
  write_padding8 ptr;
  writer ptr.buf ptr.ofs value;
  ptr.ofs <- ptr.ofs + 8

let write_bytes ptr value =
  let len = String.length value in
  String.unsafe_blit value 0 ptr.buf ptr.ofs len;
  ptr.ofs <- ptr.ofs + len

(* +-----------------------------------------------------------------+
   | Message writing                                                 |
   +-----------------------------------------------------------------+ *)

module Make_writer(Integer_writers : Integer_writers) =
struct
  open Integer_writers

  let write_uint8 ptr value = write1 put_uint8 ptr value
  let write_uint ptr value = write4 put_uint ptr value

  (* Serialize one string, without verifying it *)
  let write_string_no_check ptr string =
    write_uint ptr (String.length string);
    write_bytes ptr string;
    write_uint8 ptr 0

  (* Serialize a signature. *)
  let write_signature ptr signature =
    let string = try OBus_value.string_of_signature signature with exn -> map_exn data_error exn in
    write_uint8 ptr (String.length string);
    write_bytes ptr string;
    write_uint8 ptr 0

  let write_object_path ptr path =
    write_string_no_check ptr (try OBus_path.to_string path with exn -> map_exn data_error exn)

  let write_basic ptr = function
    | Byte x -> write1 put_char ptr x
    | Boolean x -> write4 put_uint ptr (match x with true -> 1 | false -> 0)
    | Int16 x -> write2 put_int16 ptr x
    | Int32 x -> write4 put_int32 ptr x
    | Int64 x -> write8 put_int64 ptr x
    | Uint16 x -> write2 put_uint16 ptr x
    | Uint32 x -> write4 put_uint32 ptr x
    | Uint64 x -> write8 put_uint64 ptr x
    | Double x -> write8 put_uint64 ptr (Int64.bits_of_float x)
    | String x -> begin match OBus_string.validate x with
        | Some error ->
            raise (Data_error(OBus_string.error_message error))
        | None ->
            write_string_no_check ptr x
      end
    | Signature x -> write_signature ptr x
    | Object_path x -> write_object_path ptr x

  let rec write_array ptr padded_on_8 write_element values =
    (* Array are serialized as follow:

       (1) padding to a 4-block alignement (for array size)
       (2) array size
       (3) alignement to array elements padding (even if the array is empty)
       (4) serialized elements

       The array size (2) is the size of serialized elements (4) *)

    (* Write the padding *)
    write_padding4 ptr;
    (* Save the position where to write the length of the array: *)
    let length_ofs = ptr.ofs in
    (* Allocate 4 bytes for the length: *)
    ptr.ofs <- ptr.ofs + 4;
    (* After the size we are always padded on 4, so we only need to
       add padding if elements padding is 8: *)
    if padded_on_8 then write_padding8 ptr;
    (* Save the position of the beginning of the elements of the
       array: *)
    let start_ofs = ptr.ofs in
    List.iter (fun x -> write_element ptr x) values;
    let length = ptr.ofs - start_ofs in
    if length < 0 || length > max_array_size then raise (Data_error(array_too_big length));
    (* Write the array length: *)
    put_uint ptr.buf length_ofs length

  let rec write_dict_entry ptr (k, v) =
    (* Dict-entries are serialized as follow:

       (1) alignement on a 8-block
       (2) serialized key
       (3) serialized value *)
    write_padding8 ptr;
    write_basic ptr k;
    write_single ptr v

  and write_single ptr = function
    | Basic x ->
        write_basic ptr x
    | Array(t, x) ->
        write_array ptr (pad8_p t) write_single x
    | Byte_array s ->
        write_uint ptr (String.length s);
        write_bytes ptr s
    | Dict(tk, tv, x) ->
        write_array ptr true write_dict_entry x
    | Structure x ->
        (* Structure are serialized as follow:

           (1) alignement to an 8-block
           (2) serialized contents *)
        write_padding8 ptr;
        write_sequence ptr x
    | Variant x ->
        (* Variant are serialized as follow:

           (1) marshaled variant signature
           (2) serialized contents *)
        write_signature ptr [OBus_value.type_of_single x];
        write_single ptr x

  and write_sequence ptr = function
    | [] ->
        ()
    | x :: l ->
        write_single ptr x;
        write_sequence ptr l

  (* Header field ptr *)
  let write_field_real ptr code typ writer value =
    (* Each header field is a structure, so we need to be aligned on 8 *)
    write_padding8 ptr;
    write_uint8 ptr code;
    write_signature ptr [OBus_value.Tbasic typ];
    writer ptr value

  (* Write a field if defined *)
  let write_field ptr code typ writer = function
    | None ->
        ()
    | Some value ->
        write_field_real ptr code typ writer value

  (* Validate and write a field if defined *)
  let write_name_field ptr code test field = match field with
    | None ->
        ()
    | Some string ->
        match test string with
          | Some error ->
              raise (Data_error(OBus_string.error_message error))
          | None ->
              write_field_real ptr code Tstring write_string_no_check string

  (* Serialize one complete message *)
  let write_message byte_order_char msg =
    let size = Size.message msg in
    if size > max_message_size then raise (Data_error(message_too_big size));

    let buffer = String.create size in
    let ptr = {
      buf = buffer;
      ofs = 16;
      max = size;
    } in

    (* Compute ``raw'' headers *)
    let code, fields = match msg.typ with
      | Method_call(path, interface, member) ->
          (1,
           { rf_path = Some path;
             rf_interface = interface;
             rf_member = Some member;
             rf_error_name = None;
             rf_reply_serial = None;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = type_of_sequence msg.body })
      | Method_return reply_serial ->
          (2,
           { rf_path = None;
             rf_interface = None;
             rf_member = None;
             rf_error_name = None;
             rf_reply_serial = Some reply_serial;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = type_of_sequence msg.body })
      | Error(reply_serial, error_name) ->
          (3,
           { rf_path = None;
             rf_interface = None;
             rf_member = None;
             rf_error_name = Some error_name;
             rf_reply_serial = Some reply_serial;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = type_of_sequence msg.body })
      | Signal(path, interface, member) ->
          (4,
           { rf_path = Some path;
             rf_interface = Some interface;
             rf_member = Some member;
             rf_error_name = None;
             rf_reply_serial = None;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = type_of_sequence msg.body })
    in

    write_field ptr 1 Tobject_path write_object_path fields.rf_path;
    write_name_field ptr 2 OBus_name.validate_interface fields.rf_interface;
    write_name_field ptr 3 OBus_name.validate_member fields.rf_member;
    write_name_field ptr 4 OBus_name.validate_error fields.rf_error_name;
    write_field ptr 5 Tuint32 (write4 put_uint32) fields.rf_reply_serial;
    write_name_field ptr 6 OBus_name.validate_bus fields.rf_destination;
    write_name_field ptr 7 OBus_name.validate_bus fields.rf_sender;
    write_field_real ptr 8 Tsignature write_signature fields.rf_signature;

    let fields_length = ptr.ofs - 16 in

    if fields_length < 0 || fields_length > max_array_size then
      raise (Data_error(array_too_big fields_length));

    (* The message body start aligned on an 8-boundary after the
       header: *)
    write_padding8 ptr;

    let start_ofs = ptr.ofs in

    (* Write the message body *)
    write_sequence ptr msg.body;

    let body_length = ptr.ofs - start_ofs in

    (* byte #0 : byte-order *)
    put_char buffer 0 byte_order_char;
    (* byte #1 : message type code *)
    put_uint8 buffer 1 code;
    (* byte #2 : message flags *)
    put_uint8 buffer 2
      ((if msg.flags.no_reply_expected then 1 else 0) lor
         (if msg.flags.no_auto_start then 2 else 0));
    (* byte #3 : protocol version *)
    put_uint8 buffer 3 OBus_info.protocol_version;
    (* byte #4-7 : body length *)
    put_uint buffer 4 body_length;
    (* byte #8-11 : serial *)
    put_uint32 buffer 8 msg.serial;
    (* byte #12-15 : fields length *)
    put_uint buffer 12 fields_length;

    ptr.buf
end

module LE_writer = Make_writer(LE_integer_writers)
module BE_writer = Make_writer(BE_integer_writers)

let string_of_message ?(byte_order=native_byte_order) msg =
  match byte_order with
    | Little_endian ->
        LE_writer.write_message 'l' msg
    | Big_endian ->
        BE_writer.write_message 'B' msg

let write_message oc ?byte_order msg =
  Lwt_io.write oc (string_of_message ?byte_order msg)

(* +-----------------------------------------------------------------+
   | Common reading operations                                       |
   +-----------------------------------------------------------------+ *)

let out_of_bounds () = raise (Protocol_error "out of bounds")
let unitialized_padding () = raise (Protocol_error "unitialized padding")

let read_padding ptr count =
  for i = 1 to count do
    if get_uint8 ptr.buf ptr.ofs <> 0 then unitialized_padding ();
    ptr.ofs <- ptr.ofs + 1
  done

let read_padding2 ptr =
  if padding2 ptr.ofs = 1 then begin
    if ptr.ofs + 1 > ptr.max then out_of_bounds ();
    if get_uint8 ptr.buf ptr.ofs <> 0 then unitialized_padding ()
  end

let read_padding4 ptr =
  let padding = padding4 ptr.ofs in
  if ptr.ofs + padding > ptr.max then out_of_bounds ();
  read_padding ptr padding

let read_padding8 ptr =
  let padding = padding8 ptr.ofs in
  if ptr.ofs + padding > ptr.max then out_of_bounds ();
  read_padding ptr padding

let read1 reader ptr =
  if ptr.ofs + 1 > ptr.max then out_of_bounds ();
  let x = reader ptr.buf ptr.ofs in
  ptr.ofs <- ptr.ofs + 1;
  x

let read2 reader ptr =
  let padding = padding2 ptr.ofs in
  if ptr.ofs + padding + 2 > ptr.max then out_of_bounds ();
  read_padding ptr padding;
  let x = reader ptr.buf ptr.ofs in
  ptr.ofs <- ptr.ofs + 2;
  x

let read4 reader ptr =
  let padding = padding4 ptr.ofs in
  if ptr.ofs + padding + 4 > ptr.max then out_of_bounds ();
  read_padding ptr padding;
  let x = reader ptr.buf ptr.ofs in
  ptr.ofs <- ptr.ofs + 4;
  x

let read8 reader ptr =
  let padding = padding8 ptr.ofs in
  if ptr.ofs + padding + 8 > ptr.max then out_of_bounds ();
  read_padding ptr padding;
  let x = reader ptr.buf ptr.ofs in
  ptr.ofs <- ptr.ofs + 8;
  x

let read_bytes ptr len =
  if len < 0 || ptr.ofs + len > ptr.max then out_of_bounds ();
  let s = String.create len in
  String.unsafe_blit ptr.buf ptr.ofs s 0 len;
  ptr.ofs <- ptr.ofs + len;
  s

(* +-----------------------------------------------------------------+
   | Message reading                                                 |
   +-----------------------------------------------------------------+ *)

module Make_reader(Integer_readers : Integer_readers) =
struct
  open Integer_readers

  let read_uint ptr = read4 get_uint ptr
  let read_uint8 ptr = read1 get_uint8 ptr

  let read_string_no_check ptr =
    let len = read_uint ptr in
    let x = read_bytes ptr len in
    if read_uint8 ptr <> 0 then raise (Protocol_error "missing string terminal null byte");
    x

  let read_signature ptr =
    let len = read_uint8 ptr in
    let x = read_bytes ptr len in
    if read_uint8 ptr <> 0 then raise (Protocol_error "missing signature terminating null byte");
    try OBus_value.signature_of_string x with exn -> map_exn protocol_error exn

  let read_object_path ptr =
    let str = read_string_no_check ptr in
    try OBus_path.of_string str with exn -> map_exn protocol_error exn

  let read_vbyte ptr = Byte(read1 get_char ptr)
  let read_vboolean ptr = match read_uint ptr with
    | 0 -> Boolean false
    | 1 -> Boolean true
    | n -> raise (Protocol_error(sprintf "invalid boolean value: %d" n))
  let read_vint16 ptr = Int16(read2 get_int16 ptr)
  let read_vint32 ptr = Int32(read4 get_int32 ptr)
  let read_vint64 ptr = Int64(read8 get_int64 ptr)
  let read_vuint16 ptr = Uint16(read2 get_uint16 ptr)
  let read_vuint32 ptr = Uint32(read4 get_uint32 ptr)
  let read_vuint64 ptr = Uint64(read8 get_uint64 ptr)
  let read_vdouble ptr = Double(Int64.float_of_bits (read8 get_uint64 ptr))
  let read_vstring ptr =
    let str = read_string_no_check ptr in
    match OBus_string.validate str with
      | None -> String str
      | Some error -> raise (Protocol_error(OBus_string.error_message error))
  let read_vsignature ptr = Signature(read_signature ptr)
  let read_vobject_path ptr = Object_path(read_object_path ptr)

  let basic_reader = function
    | Tbyte -> read_vbyte
    | Tboolean -> read_vboolean
    | Tint16 -> read_vint16
    | Tint32 -> read_vint32
    | Tint64 -> read_vint64
    | Tuint16 -> read_vuint16
    | Tuint32 -> read_vuint32
    | Tuint64 -> read_vuint64
    | Tdouble -> read_vdouble
    | Tstring -> read_vstring
    | Tsignature -> read_vsignature
    | Tobject_path -> read_vobject_path

  let read_array padded_on_8 read_element ptr =
    let len = read_uint ptr in
    if len < 0 || len > max_array_size then raise (Protocol_error(array_too_big len));
    if padded_on_8 then read_padding8 ptr;
    let limit = ptr.ofs + len in
    let rec aux () =
      if ptr.ofs >= limit then
        []
      else
        let x = read_element ptr in
        let l = aux () in
        x :: l
    in
    aux ()

  let rec single_reader = function
    | Tbasic t ->
        let reader = basic_reader t in
        (fun ptr -> basic(reader ptr))
    | Tarray t ->
        let reader = single_reader t and padded_on_8 = pad8_p t in
        (fun ptr -> array t (read_array padded_on_8 reader ptr))
    | Tdict(tk, tv) ->
        let kreader = basic_reader tk and vreader = single_reader tv in
        let reader ptr =
          read_padding8 ptr;
          let k = kreader ptr in
          let v = vreader ptr in
          (k, v)
        in
        (fun ptr -> dict tk tv (read_array true reader ptr))
    | Tstructure tl ->
        let reader = sequence_reader tl in
        (fun ptr ->
           read_padding8 ptr;
           structure (reader ptr))
    | Tvariant ->
        read_variant

  and read_variant ptr =
    match read_signature ptr with
      | [t] ->
          variant (single_reader t ptr)
      | s ->
          raise (Protocol_error(Printf.sprintf "variant signature does not contain one single type: %S" (OBus_value.string_of_signature s)))

  and sequence_reader = function
    | [] ->
        (fun ptr -> [])
    | t :: l ->
        let head_reader = single_reader t and tail_reader = sequence_reader l in
        (fun ptr ->
           let x = head_reader ptr in
           let l = tail_reader ptr in
           x :: l)

  let read_field code typ reader ptr =
    match read_signature ptr with
      | [Tbasic t] when t = typ ->
          reader ptr
      | s ->
          raise (Protocol_error(sprintf "invalid header field signature for code %d: %S, should be %S"
                                  code (string_of_signature s) (string_of_signature [Tbasic typ])))

  let read_name_field code test ptr =
    let str = read_field code Tstring read_string_no_check ptr in
    match test str with
      | None ->
          str
      | Some error ->
          raise (Protocol_error(OBus_string.error_message error))

  let read_message buffer get_message =
    (* Check the protocol version first, since we can not do anything
       if it is not the same as our *)
    let protocol_version = get_uint8 buffer 3 in
    if protocol_version <> OBus_info.protocol_version then
      raise (Protocol_error(invalid_protocol_version protocol_version));

    let message_maker = match get_uint8 buffer 1 with
      | 1 -> method_call_of_raw
      | 2 -> method_return_of_raw
      | 3 -> error_of_raw
      | 4 -> signal_of_raw
      | n -> raise (Protocol_error(sprintf "unknown message type: %d" n)) in

    let n = get_uint8 buffer 2 in
    let flags = { no_reply_expected = n land 1 = 1; no_auto_start = n land 2 = 2 } in

    let body_length = get_uint buffer 4
    and serial = get_uint32 buffer 8
    and fields_length = get_uint buffer 12 in

    (* Header fields array start on byte #16 and message start aligned
       on a 8-boundary after it, so we have: *)
    let total_length = 16 + pad8 fields_length + body_length in

    (* Safety checkings *)

    if fields_length < 0 || fields_length > max_array_size then
      raise (Protocol_error(array_too_big fields_length));

    if body_length < 0 || total_length > max_message_size then
      raise (Protocol_error(message_too_big total_length));

    get_message total_length begin fun ptr cont ->
      let fields = {
        rf_path = None;
        rf_member = None;
        rf_interface = None;
        rf_error_name = None;
        rf_reply_serial = None;
        rf_destination = None;
        rf_sender = None;
        rf_signature = [];
      } in
      let limit = ptr.ofs + fields_length in
      (* Reading of fields *)
      while ptr.ofs < limit do
        read_padding8 ptr;
        match read_uint8 ptr with
          | 1 -> fields.rf_path <- Some(read_field 1 Tobject_path read_object_path ptr)
          | 2 -> fields.rf_interface <- Some(read_name_field 2 OBus_name.validate_interface ptr)
          | 3 -> fields.rf_member <- Some(read_name_field 3 OBus_name.validate_member ptr)
          | 4 -> fields.rf_error_name <- Some(read_name_field 4 OBus_name.validate_error ptr)
          | 5 -> fields.rf_reply_serial <- Some(read_field 5 Tuint32 (read4 get_uint32) ptr)
          | 6 -> fields.rf_destination <- Some(read_name_field 6 OBus_name.validate_bus ptr)
          | 7 -> fields.rf_sender <- Some(read_name_field 7 OBus_name.validate_bus ptr)
          | 8 -> fields.rf_signature <- read_field 8 Tsignature read_signature ptr
          | _ -> ignore (read_variant ptr) (* Unsupported header field *)
      done;

      read_padding8 ptr;
      let body = sequence_reader fields.rf_signature ptr in

      if ptr.ofs < ptr.max then raise (Protocol_error "junk bytes after message");
      cont { flags = flags;
             sender = fields.rf_sender;
             destination = fields.rf_destination;
             serial = serial;
             typ = message_maker fields;
             body = body }
    end
end

module LE_reader = Make_reader(LE_integer_readers)
module BE_reader = Make_reader(BE_integer_readers)

let read_message ic =
  Lwt_io.atomic begin fun ic ->
    let buffer = String.create 16 in
    lwt () = Lwt_io.read_into_exactly ic buffer 0 16 in
    (match get_char buffer 0 with
       | 'l' -> LE_reader.read_message
       | 'B' -> BE_reader.read_message
       | ch -> raise (Protocol_error(invalid_byte_order ch)))
      buffer
      (fun length f ->
         let length = length - 16 in
         let buffer = String.create length in
         lwt () = Lwt_io.read_into_exactly ic buffer 0 length in
         f { buf = buffer; ofs = 0; max = length } Lwt.return)
  end ic

let message_of_string buffer =
  if String.length buffer < 16 then invalid_arg "OBus_wire.message_of_string: buffer too small";
  (match get_char buffer 0 with
     | 'l' -> LE_reader.read_message
     | 'B' -> BE_reader.read_message
     | ch -> raise (Protocol_error(invalid_byte_order ch)))
    buffer
    (fun length f ->
       if length <> String.length buffer then raise (Protocol_error "invalid message size");
       f { buf = buffer; ofs = 16; max = length } (fun x -> x))

(* +-----------------------------------------------------------------+
   | Size computation                                                |
   +-----------------------------------------------------------------+ *)

let get_message_size buf ofs =

  let unsafe_get_uint map_ofs i =
    let v0 = String.unsafe_get buf (map_ofs (i + 0))
    and v1 = String.unsafe_get buf (map_ofs (i + 1))
    and v2 = String.unsafe_get buf (map_ofs (i + 2))
    and v3 = String.unsafe_get buf (map_ofs (i + 3)) in
    Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)
  in

  if ofs < 0 || ofs + 16 >= String.length buf then
    raise (Invalid_argument "OBus_wire.get_message_size")

  else
    (* Byte-order *)
    let map_ofs = match String.unsafe_get buf ofs with
      | 'l' -> (fun i -> i)
      | 'B' -> (fun i -> 3 - i)
      | ch -> raise (Protocol_error(invalid_byte_order ch))
    in
    let ver = Char.code (String.unsafe_get buf (ofs + 3)) in
    if ver <> OBus_info.protocol_version then
      raise (Protocol_error(invalid_protocol_version ver));

    let body_length = unsafe_get_uint map_ofs (ofs + 8)
    and fields_length = unsafe_get_uint map_ofs (ofs + 12) in

    let total_length = 16 + fields_length + pad8 fields_length + body_length in

    if fields_length < 0 || fields_length > max_array_size then
      raise (Protocol_error(array_too_big fields_length));

    if body_length < 0 || total_length > max_message_size then
      raise (Protocol_error(message_too_big total_length));

    total_length
