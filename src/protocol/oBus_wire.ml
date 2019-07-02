(*
 * oBus_lowlevel.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(wire)"

open Printf
open OBus_value
open OBus_message
open OBus_protocol

(* +-----------------------------------------------------------------+
   | Errors                                                          |
   +-----------------------------------------------------------------+ *)

exception Data_error of string
exception Protocol_error of string

let () =
  Printexc.register_printer
    (function
       | Data_error msg ->
           Some(sprintf "failed to marshal D-Bus message: %s" msg)
       | Protocol_error msg ->
           Some(sprintf "D-Bus protocol error: %s" msg)
       | _ ->
           None)

(* Common error message *)
let array_too_big len = sprintf "array size exceed the limit: %d" len
let message_too_big len = sprintf "message size exceed the limit: %d" len
let signature_too_long s len = sprintf "too long signature: '%s', with len %d" (string_of_signature s) len
let invalid_protocol_version ver = sprintf "invalid protocol version: %d (obus implement protocol version %d)" ver OBus_info.protocol_version
let invalid_byte_order ch = sprintf "invalid byte order(%C)" ch

(* +-----------------------------------------------------------------+
   | Padding                                                         |
   +-----------------------------------------------------------------+ *)

let padding2 i = i land 1
let padding4 i = (4 - i) land 3
let padding8 i = (8 - i) land 7

let pad2 i = i + padding2 i
let pad4 i = i + padding4 i
let pad8 i = i + padding8 i

let pad8_p = function
  | T.Structure _
  | T.Basic T.Int64
  | T.Basic T.Uint64
  | T.Basic T.Double -> true
  | _ -> false

(* +-----------------------------------------------------------------+
   | Raw description of header fields                                |
   +-----------------------------------------------------------------+ *)

type raw_fields = {
  mutable rf_path : OBus_path.t option;
  mutable rf_member : OBus_name.member;
  mutable rf_interface : OBus_name.interface;
  mutable rf_error_name : OBus_name.error;
  mutable rf_reply_serial : serial option;
  mutable rf_destination : OBus_name.bus;
  mutable rf_sender : OBus_name.bus;
  mutable rf_signature : signature;
  mutable rf_unix_fds : int;
}

let missing_field message_type_name field_name =
  raise (Protocol_error(sprintf "invalid header, field '%s' is required for '%s'"
                          field_name message_type_name))

let get_required_string message_type_name field_name = function
  | "" ->
      missing_field message_type_name field_name
  | string ->
      string

let get_required_option message_type_name field_name = function
  | None ->
      missing_field message_type_name field_name
  | Some value ->
      value

let method_call_of_raw fields =
  Method_call(get_required_option "method-call" "path" fields.rf_path,
              fields.rf_interface,
              get_required_string "method-call" "member" fields.rf_member)

let method_return_of_raw fields =
  Method_return(get_required_option "method-return" "reply-serial" fields.rf_reply_serial)

let error_of_raw fields =
  Error(get_required_option "error" "reply-serial" fields.rf_reply_serial,
        get_required_string "error" "error-name" fields.rf_error_name)

let signal_of_raw fields =
  Signal(get_required_option "signal" "path" fields.rf_path,
         get_required_string "signal" "interface" fields.rf_interface,
         get_required_string "signal" "member" fields.rf_member)

(* +-----------------------------------------------------------------+
   | Error mapping                                                   |
   +-----------------------------------------------------------------+ *)

(* Maps error returned by [OBus_*.*] to [Data_error] or
   [Protocol_error]: *)

let map_exn f = function
  | OBus_string.Invalid_string err ->
      raise (f (OBus_string.error_message err))
  | OBus_value.Invalid_signature(str, msg) ->
      raise (f (Printf.sprintf "invalid signature (%S): %s" str msg))
  | exn ->
      raise exn

let data_error msg = Data_error msg
let protocol_error msg = Protocol_error msg

(* +-----------------------------------------------------------------+
   | Message size calculation                                        |
   +-----------------------------------------------------------------+ *)

module FD_set = Set.Make(struct type t = Unix.file_descr let compare = compare end)

module Count =
struct
  (* The goal of this module is to compute the marshaled size of a
     message, and the number of different file descriptors it
     contains. *)

  type counter = {
    mutable ofs : int;
    (* Simulate an offset *)
    mutable fds : FD_set.t;
    (* Set used to collect all file descriptors *)
  }

  let path_length = function
    | [] -> 1
    | l -> List.fold_left (fun acc x -> 1 + String.length x + acc) 0 l

  let rec iter f c = function
    | [] -> ()
    | x :: l -> f c x; iter f c l

  let rec tsingle c = function
    | T.Basic _ ->
        c.ofs <- c.ofs + 1
    | T.Array t ->
        c.ofs <- c.ofs + 1;
        tsingle c t
    | T.Dict(tk, tv) ->
        c.ofs <- c.ofs + 4;
        tsingle c tv
    | T.Structure l ->
        c.ofs <- c.ofs + 2;
        iter tsingle c l
    | T.Variant ->
        c.ofs <- c.ofs + 1

  let tsequence c l =
    iter tsingle c l

  let rec tsingle_of_single c = function
    | V.Basic x ->
        c.ofs <- c.ofs + 1
    | V.Array(t, x) ->
        c.ofs <- c.ofs + 1;
        tsingle c t
    | V.Byte_array _ ->
        c.ofs <- c.ofs + 2
    | V.Dict(tk, tv, x) ->
        c.ofs <- c.ofs + 4;
        tsingle c tv
    | V.Structure l ->
        c.ofs <- c.ofs + 2;
        iter tsingle_of_single c l
    | V.Variant x ->
        c.ofs <- c.ofs + 1

  let tsequence_of_sequence c l =
    iter tsingle_of_single c l

  let rec basic c = function
    | V.Byte _ ->
        c.ofs <- c.ofs + 1
    | V.Int16 _
    | V.Uint16 _ ->
        c.ofs <- pad2 c.ofs + 2
    | V.Boolean _
    | V.Int32 _
    | V.Uint32 _ ->
        c.ofs <- pad4 c.ofs + 4
    | V.Int64 _
    | V.Uint64 _
    | V.Double _ ->
        c.ofs <- pad8 c.ofs + 8
    | V.String s ->
        c.ofs <- pad4 c.ofs + String.length s + 5
    | V.Signature s ->
        c.ofs <- c.ofs + 2;
        tsequence c s
    | V.Object_path p ->
        c.ofs <- pad4 c.ofs + path_length p + 5
    | V.Unix_fd fd ->
        c.ofs <- pad4 c.ofs + 4;
        c.fds <- FD_set.add fd c.fds

  let rec single c = function
    | V.Basic x ->
        basic c x
    | V.Array(t, l) ->
        c.ofs <- pad4 c.ofs + 4;
        if pad8_p t then c.ofs <- pad8 c.ofs;
        iter single c l
    | V.Byte_array bytes ->
        c.ofs <- pad4 c.ofs + 4 + String.length bytes
    | V.Dict(tk, tv, l) ->
        c.ofs <- pad8 (pad4 c.ofs + 4);
        iter dict_entry c l
    | V.Structure l ->
        c.ofs <- pad8 c.ofs;
        iter single c l
    | V.Variant x ->
        c.ofs <- c.ofs + 2;
        tsingle_of_single c x;
        single c x

  and dict_entry c (k, v) =
    c.ofs <- pad8 c.ofs;
    basic c k;
    single c v

  let sequence c l =
    iter single c l

  let message msg =
    let c = { ofs = 16; fds = FD_set.empty } in
    begin match msg.typ with
      | Method_call(path, "", member) ->
          (* +9 for:
             - the code (1)
             - the signature of one basic type code (3)
             - the string length (4)
             - the null byte (1) *)
          c.ofs <- pad8 c.ofs + 9 + path_length path;
          c.ofs <- pad8 c.ofs + 9 + String.length member
      | Method_call(path, interface, member)
      | Signal(path, interface, member) ->
          c.ofs <- pad8 c.ofs + 9 + path_length path;
          c.ofs <- pad8 c.ofs + 9 + String.length interface;
          c.ofs <- pad8 c.ofs + 9 + String.length member
      | Method_return serial ->
          c.ofs <- pad8 c.ofs + 8
      | Error(serial, name) ->
          c.ofs <- pad8 c.ofs + 9 + String.length name;
          c.ofs <- pad8 c.ofs + 8
    end;
    if msg.destination <> "" then
      c.ofs <- pad8 c.ofs + 9 + String.length msg.destination;
    if msg.sender <> "" then
      c.ofs <- pad8 c.ofs + 9 + String.length msg.sender;
    (* The signature *)
    c.ofs <- pad8 c.ofs + 6;
    tsequence_of_sequence c msg.body;
    (* The number of fds: *)
    c.ofs <- pad8 c.ofs + 8;
    (* The message body: *)
    sequence c msg.body;
    c
end

(* +-----------------------------------------------------------------+
   | Unsafe writing of integers                                      |
   +-----------------------------------------------------------------+ *)

let put_char = Bytes.unsafe_set
let put_uint8 buf ofs x = put_char buf ofs (Char.unsafe_chr x)

module type Integer_writers = sig
  val put_int16 : bytes -> int -> int -> unit
  val put_int32 : bytes -> int -> int32 -> unit
  val put_int64 : bytes -> int -> int64 -> unit
  val put_uint16 : bytes -> int -> int -> unit
  val put_uint32 : bytes -> int -> int32 -> unit
  val put_uint64 : bytes -> int -> int64 -> unit

  val put_uint : bytes -> int -> int -> unit
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

(* +---------------------------------------------------------------+
   | Common writing functions                                      |
   +---------------------------------------------------------------+ *)

module FD_map = Map.Make(struct type t = Unix.file_descr let compare = Pervasives.compare end)

(* A pointer for serializing data *)
type wpointer = {
  buf : bytes;
  mutable ofs : int;
  max : int;
  fds : int FD_map.t;
  (* Maps file descriptros to their index in the resulting fds
     array *)
}

let write_padding2 ptr =
  if ptr.ofs land 1 = 1 then begin
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
    let string = OBus_value.string_of_signature signature in
    write_uint8 ptr (String.length string);
    write_bytes ptr string;
    write_uint8 ptr 0

  let write_object_path ptr path =
    write_string_no_check ptr (OBus_path.to_string path)

  let write_basic ptr = function
    | V.Byte x -> write1 put_char ptr x
    | V.Boolean x -> write4 put_uint ptr (match x with true -> 1 | false -> 0)
    | V.Int16 x -> write2 put_int16 ptr x
    | V.Int32 x -> write4 put_int32 ptr x
    | V.Int64 x -> write8 put_int64 ptr x
    | V.Uint16 x -> write2 put_uint16 ptr x
    | V.Uint32 x -> write4 put_uint32 ptr x
    | V.Uint64 x -> write8 put_uint64 ptr x
    | V.Double x -> write8 put_uint64 ptr (Int64.bits_of_float x)
    | V.String x -> begin match OBus_string.validate x with
        | Some error ->
            raise (Data_error(OBus_string.error_message error))
        | None ->
            write_string_no_check ptr x
      end
    | V.Signature x -> write_signature ptr x
    | V.Object_path x -> write_object_path ptr x
    | V.Unix_fd fd -> write4 put_uint ptr (FD_map.find fd ptr.fds)

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
    | V.Basic x ->
        write_basic ptr x
    | V.Array(t, x) ->
        write_array ptr (pad8_p t) write_single x
    | V.Byte_array s ->
        write_uint ptr (String.length s);
        write_bytes ptr s
    | V.Dict(tk, tv, x) ->
        write_array ptr true write_dict_entry x
    | V.Structure x ->
        (* Structure are serialized as follow:

           (1) alignement to an 8-block
           (2) serialized contents *)
        write_padding8 ptr;
        write_sequence ptr x
    | V.Variant x ->
        (* Variant are serialized as follow:

           (1) marshaled variant signature
           (2) serialized contents *)
        write_signature ptr [OBus_value.V.type_of_single x];
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
    write_signature ptr [T.Basic typ];
    writer ptr value

  (* Write a field if defined *)
  let write_field ptr code typ writer = function
    | None ->
        ()
    | Some value ->
        write_field_real ptr code typ writer value

  (* Validate and write a field if defined *)
  let write_name_field ptr code test = function
    | "" ->
        ()
    | string ->
        match test string with
          | Some error ->
              raise (Data_error(OBus_string.error_message error))
          | None ->
              write_field_real ptr code T.String write_string_no_check string

  (* Serialize one complete message *)
  let write_message byte_order_char msg =
    let { Count.ofs = size; Count.fds = fds } = Count.message msg in
    if size > max_message_size then raise (Data_error(message_too_big size));

    let buffer = Bytes.create size in
    let ptr = {
      buf = buffer;
      ofs = 16;
      max = size;
      fds = snd (FD_set.fold (fun fd (n, map) -> (n + 1, FD_map.add fd n map)) fds (0, FD_map.empty));
    } in

    let fd_count = FD_set.cardinal fds in
    (* Compute ``raw'' headers *)
    let code, fields = match msg.typ with
      | Method_call(path, interface, member) ->
          if member = "" then raise (Data_error "invalid method-call message: field 'member' is empty");
          (1,
           { rf_path = Some path;
             rf_interface = interface;
             rf_member = member;
             rf_error_name = "";
             rf_reply_serial = None;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = V.type_of_sequence msg.body;
             rf_unix_fds = fd_count })
      | Method_return reply_serial ->
          (2,
           { rf_path = None;
             rf_interface = "";
             rf_member = "";
             rf_error_name = "";
             rf_reply_serial = Some reply_serial;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = V.type_of_sequence msg.body;
             rf_unix_fds = fd_count })
      | Error(reply_serial, error_name) ->
          if error_name = "" then raise (Data_error "invalid error message: field 'error-name' is empty");
          (3,
           { rf_path = None;
             rf_interface = "";
             rf_member = "";
             rf_error_name = error_name;
             rf_reply_serial = Some reply_serial;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = V.type_of_sequence msg.body;
             rf_unix_fds = fd_count })
      | Signal(path, interface, member) ->
          if interface = "" then raise (Data_error "invalid signal message, field 'interface' is empty");
          if member = "" then raise (Data_error "invalid signal message, field 'member' is empty");
          (4,
           { rf_path = Some path;
             rf_interface = interface;
             rf_member = member;
             rf_error_name = "";
             rf_reply_serial = None;
             rf_destination = msg.destination;
             rf_sender = msg.sender;
             rf_signature = V.type_of_sequence msg.body;
             rf_unix_fds = fd_count })
    in

    write_field ptr 1 T.Object_path write_object_path fields.rf_path;
    write_name_field ptr 2 OBus_name.validate_interface fields.rf_interface;
    write_name_field ptr 3 OBus_name.validate_member fields.rf_member;
    write_name_field ptr 4 OBus_name.validate_error fields.rf_error_name;
    write_field ptr 5 T.Uint32 (write4 put_uint32) fields.rf_reply_serial;
    write_name_field ptr 6 OBus_name.validate_bus fields.rf_destination;
    write_name_field ptr 7 OBus_name.validate_bus fields.rf_sender;
    write_field_real ptr 8 T.Signature write_signature fields.rf_signature;
    write_field_real ptr 9 T.Uint32 (write4 put_uint) fields.rf_unix_fds;

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

    (* Create the array of file descriptors *)
    let fds = Array.make fd_count Unix.stdin in
    FD_map.iter (fun fd index -> Array.unsafe_set fds index fd) ptr.fds;

    (Bytes.unsafe_to_string ptr.buf, fds)
end

module LE_writer = Make_writer(LE_integer_writers)
module BE_writer = Make_writer(BE_integer_writers)

let string_of_message ?(byte_order=Lwt_io.system_byte_order) msg =
  try
    match byte_order with
      | Lwt_io.Little_endian ->
          LE_writer.write_message 'l' msg
      | Lwt_io.Big_endian ->
          BE_writer.write_message 'B' msg
  with exn ->
    raise (map_exn data_error exn)

let write_message oc ?byte_order msg =
  match string_of_message ?byte_order msg with
    | str, [||] ->
        Lwt_io.write oc str
    | _ ->
        Lwt.fail (Data_error "Cannot send a message with file descriptors on a channel")

type writer = {
  w_channel : Lwt_io.output_channel;
  w_file_descr : Lwt_unix.file_descr;
}

let close_writer writer = Lwt_io.close writer.w_channel

let writer fd = {
  w_channel = Lwt_io.of_fd ~mode:Lwt_io.output ~close:Lwt.return fd;
  w_file_descr = fd;
}

let write_message_with_fds writer ?byte_order msg =
  match string_of_message ?byte_order msg with
    | buf, [||] ->
        (* No file descriptor to send, simply use the channel *)
        Lwt_io.write writer.w_channel buf
    | buf, fds ->
        Lwt_io.atomic begin fun oc ->
          (* Ensures there is nothing left to send: *)
          let%lwt () = Lwt_io.flush oc in
          let len = String.length buf in
          (* Send the file descriptors and the message: *)
          let%lwt n = Lwt_unix.send_msg writer.w_file_descr [Lwt_unix.io_vector buf 0 len] (Array.to_list fds) in
          assert (n >= 0 && n <= len);
          (* Write what is remaining: *)
          Lwt_io.write_from_string_exactly oc buf n (len - n)
        end writer.w_channel

(* +-----------------------------------------------------------------+
   | Common reading operations                                       |
   +-----------------------------------------------------------------+ *)

(* A pointer for unserializing data *)
type rpointer = {
  buf : string;
  mutable ofs : int;
  max : int;
  mutable fds : Unix.file_descr array;
  (* The array of file descriptors received with the message *)
}

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
  let s = Bytes.create len in
  String.unsafe_blit ptr.buf ptr.ofs s 0 len;
  ptr.ofs <- ptr.ofs + len;
  Bytes.unsafe_to_string s

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
    OBus_value.signature_of_string x

  let read_object_path ptr =
    let str = read_string_no_check ptr in
    OBus_path.of_string str

  let read_vbyte ptr = V.Byte(read1 get_char ptr)
  let read_vboolean ptr = match read_uint ptr with
    | 0 -> V.Boolean false
    | 1 -> V.Boolean true
    | n -> raise (Protocol_error(sprintf "invalid boolean value: %d" n))
  let read_vint16 ptr = V.Int16(read2 get_int16 ptr)
  let read_vint32 ptr = V.Int32(read4 get_int32 ptr)
  let read_vint64 ptr = V.Int64(read8 get_int64 ptr)
  let read_vuint16 ptr = V.Uint16(read2 get_uint16 ptr)
  let read_vuint32 ptr = V.Uint32(read4 get_uint32 ptr)
  let read_vuint64 ptr = V.Uint64(read8 get_uint64 ptr)
  let read_vdouble ptr = V.Double(Int64.float_of_bits (read8 get_uint64 ptr))
  let read_vstring ptr =
    let str = read_string_no_check ptr in
    match OBus_string.validate str with
      | None -> V.String str
      | Some error -> raise (Protocol_error(OBus_string.error_message error))
  let read_vsignature ptr = V.Signature(read_signature ptr)
  let read_vobject_path ptr = V.Object_path(read_object_path ptr)
  let read_unix_fd ptr =
    let index = read4 get_uint ptr in
    if index < 0 || index >= Array.length ptr.fds then
      raise (Protocol_error "fd index out of bounds")
    else
      V.Unix_fd(Array.unsafe_get ptr.fds index)

  let basic_reader = function
    | T.Byte -> read_vbyte
    | T.Boolean -> read_vboolean
    | T.Int16 -> read_vint16
    | T.Int32 -> read_vint32
    | T.Int64 -> read_vint64
    | T.Uint16 -> read_vuint16
    | T.Uint32 -> read_vuint32
    | T.Uint64 -> read_vuint64
    | T.Double -> read_vdouble
    | T.String -> read_vstring
    | T.Signature -> read_vsignature
    | T.Object_path -> read_vobject_path
    | T.Unix_fd -> read_unix_fd

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
    | T.Basic t ->
        let reader = basic_reader t in
        (fun ptr -> V.basic(reader ptr))
    | T.Array(T.Basic T.Byte)->
        (fun ptr ->
           let len = read_uint ptr in
           if len < 0 || len > max_array_size then raise (Protocol_error(array_too_big len));
           V.byte_array (read_bytes ptr len))
    | T.Array t ->
        let reader = single_reader t and padded_on_8 = pad8_p t in
        (fun ptr -> V.unsafe_array t (read_array padded_on_8 reader ptr))
    | T.Dict(tk, tv) ->
        let kreader = basic_reader tk and vreader = single_reader tv in
        let reader ptr =
          read_padding8 ptr;
          let k = kreader ptr in
          let v = vreader ptr in
          (k, v)
        in
        (fun ptr -> V.unsafe_dict tk tv (read_array true reader ptr))
    | T.Structure tl ->
        let reader = sequence_reader tl in
        (fun ptr ->
           read_padding8 ptr;
           V.structure (reader ptr))
    | T.Variant ->
        read_variant

  and read_variant ptr =
    match read_signature ptr with
      | [t] ->
          V.variant (single_reader t ptr)
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
      | [T.Basic t] when t = typ ->
          reader ptr
      | s ->
          raise (Protocol_error(sprintf "invalid header field signature for code %d: %S, should be %S"
                                  code (string_of_signature s) (string_of_signature [T.Basic typ])))

  let read_name_field code test ptr =
    let str = read_field code T.String read_string_no_check ptr in
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

    get_message total_length begin fun ptr pending_fds cont ->
      let fields = {
        rf_path = None;
        rf_member = "";
        rf_interface = "";
        rf_error_name = "";
        rf_reply_serial = None;
        rf_destination = "";
        rf_sender = "";
        rf_signature = [];
        rf_unix_fds = 0;
      } in
      let limit = ptr.ofs + fields_length in
      (* Reading of fields *)
      while ptr.ofs < limit do
        read_padding8 ptr;
        match read_uint8 ptr with
          | 1 -> fields.rf_path <- Some(read_field 1 T.Object_path read_object_path ptr)
          | 2 -> fields.rf_interface <- read_name_field 2 OBus_name.validate_interface ptr
          | 3 -> fields.rf_member <- read_name_field 3 OBus_name.validate_member ptr
          | 4 -> fields.rf_error_name <- read_name_field 4 OBus_name.validate_error ptr
          | 5 -> fields.rf_reply_serial <- Some(read_field 5 T.Uint32 (read4 get_uint32) ptr)
          | 6 -> fields.rf_destination <- read_name_field 6 OBus_name.validate_bus ptr
          | 7 -> fields.rf_sender <- read_name_field 7 OBus_name.validate_bus ptr
          | 8 -> fields.rf_signature <- read_field 8 T.Signature read_signature ptr
          | 9 -> fields.rf_unix_fds <- read_field 9 T.Uint32 (read4 get_uint) ptr
          | _ -> ignore (read_variant ptr) (* Unsupported header field *)
      done;

      begin
        match pending_fds with
          | None ->
              if fields.rf_unix_fds <> Array.length ptr.fds then
                raise (Protocol_error(sprintf
                                        "invalid number of file descriptor, %d expected, %d received"
                                        fields.rf_unix_fds
                                        (Array.length ptr.fds)));
          | Some(consumed, queue) ->
              ptr.fds <- Array.init fields.rf_unix_fds
                (fun i ->
                   if Queue.is_empty queue then
                     raise (Protocol_error "file descriptor missing")
                   else begin
                     let fd = Queue.take queue in
                     consumed := fd :: !consumed;
                     fd
                   end)
      end;

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
  try%lwt
    Lwt_io.atomic begin fun ic ->
      let buffer = Bytes.create 16 in
      let%lwt () = Lwt_io.read_into_exactly ic buffer 0 16 in
      let buffer = Bytes.unsafe_to_string buffer in
      (match get_char buffer 0 with
         | 'l' -> LE_reader.read_message
         | 'B' -> BE_reader.read_message
         | ch -> raise (Protocol_error(invalid_byte_order ch)))
        buffer
        (fun length f ->
           let length = length - 16 in
           let buffer = Bytes.create length in
           let%lwt () = Lwt_io.read_into_exactly ic buffer 0 length in
           let buffer = Bytes.unsafe_to_string buffer in
           f { buf = buffer; ofs = 0; max = length; fds = [||] } None Lwt.return)
    end ic
  with exn ->
    raise (map_exn protocol_error exn)

let message_of_string buffer fds =
  if String.length buffer < 16 then invalid_arg "OBus_wire.message_of_string: buffer too small";
  try
    (match get_char buffer 0 with
       | 'l' -> LE_reader.read_message
       | 'B' -> BE_reader.read_message
       | ch -> raise (Protocol_error(invalid_byte_order ch)))
      buffer
      (fun length f ->
         if length <> String.length buffer then raise (Protocol_error "invalid message size");
         f { buf = buffer; ofs = 16; max = length; fds = fds } None (fun x -> x))
  with exn ->
    raise (map_exn protocol_error exn)

type reader = {
  r_channel : Lwt_io.input_channel;
  r_pending_fds : Unix.file_descr Queue.t;
  (* File descriptors received and not yet taken *)
}

let close_reader reader =
  let fds = Queue.fold (fun fds fd -> fd :: fds) [] reader.r_pending_fds in
  Queue.clear reader.r_pending_fds;
  let%lwt () =
    Lwt_list.iter_p
      (fun fd ->
         try
           Lwt_unix.close (Lwt_unix.of_unix_file_descr ~set_flags:false fd)
         with Unix.Unix_error(err, _, _) ->
           Lwt_log.error_f ~section "cannot close file descriptor: %s" (Unix.error_message err))
      fds
  in
  Lwt_io.close reader.r_channel

let reader fd =
  let pending_fds = Queue.create () in
  {
    r_channel = Lwt_io.make ~mode:Lwt_io.input
      (fun buf ofs len ->
        let%lwt n, fds = Lwt_bytes.recv_msg fd [Lwt_bytes.io_vector buf ofs len] in
         List.iter (fun fd ->
                      (try Unix.set_close_on_exec fd with _ -> ());
                      Queue.push fd pending_fds) fds;
         Lwt.return n);
    r_pending_fds = pending_fds;
  }

let read_message_with_fds reader  =
  let consumed_fds = ref [] in
  try%lwt
    Lwt_io.atomic begin fun ic ->
      let buffer = Bytes.create 16 in
      let%lwt () = Lwt_io.read_into_exactly ic buffer 0 16 in
      let buffer = Bytes.unsafe_to_string buffer in
      (match get_char buffer 0 with
         | 'l' -> LE_reader.read_message
         | 'B' -> BE_reader.read_message
         | ch -> raise (Protocol_error(invalid_byte_order ch)))
        buffer
        (fun length f ->
           let length = length - 16 in
           let buffer = Bytes.create length in
           let%lwt () = Lwt_io.read_into_exactly ic buffer 0 length in
           let buffer = Bytes.unsafe_to_string buffer in
           f { buf = buffer; ofs = 0; max = length; fds = [||] } (Some(consumed_fds, reader.r_pending_fds)) Lwt.return)
    end reader.r_channel
  with exn ->
    let%lwt () =
      Lwt_list.iter_p
        (fun fd ->
           try
             Lwt_unix.close (Lwt_unix.of_unix_file_descr ~set_flags:false fd)
           with Unix.Unix_error(err, _, _) ->
             Lwt_log.error_f ~section "cannot close file descriptor: %s" (Unix.error_message err))
        !consumed_fds
    in
    Lwt.fail (map_exn protocol_error exn)

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
