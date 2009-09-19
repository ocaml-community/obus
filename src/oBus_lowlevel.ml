(*
 * oBus_lowlevel.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open Constant
open OBus_value
open OBus_message

exception Data_error of string
exception Protocol_error of string

type byte_order = Little_endian | Big_endian
let native_byte_order = Little_endian

module type Monad = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
end
module type Reader = sig
  include Monad
  val get_char : char t
  val get_string : int -> string t
end
module type Writer = sig
  include Monad
  val put_char : char -> unit t
  val put_string : string -> unit t
end

let pad2 i = i land 1
let pad4 i = (4 - i) land 3
let pad8 i = (8 - i) land 7

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
let invalid_signature s reason = sprintf "invalid signature('%s'): %s" (string_of_signature s) reason
let invalid_protocol_version ver = sprintf "invalid protocol version: %d (obus implement protocol version %d)" ver OBus_info.protocol_version
let invalid_byte_order ch = sprintf "invalid byte order(%C)" ch

(* +----------------------------------+
   | Raw description of header fields |
   +----------------------------------+ *)

type raw_fields = {
  _path : OBus_path.t option;
  _member : OBus_name.member option;
  _interface : OBus_name.interface option;
  _error_name : OBus_name.error option;
  _reply_serial : serial option;
  _destination : OBus_name.bus option;
  _sender : OBus_name.bus option;
  _signature : signature;
}

let empty_fields = {
  _path = None;
  _member = None;
  _interface = None;
  _error_name = None;
  _reply_serial = None;
  _destination = None;
  _sender = None;
  _signature = [];
}

let path = ("path", fun x -> x._path)
let member = ("member", fun x -> x._member)
let interface = ("interface", fun x -> x._interface)
let error_name = ("error_name", fun x -> x._error_name)
let reply_serial = ("reply_serial", fun x -> x._reply_serial)

let get_required message_type_name (field_name, get_field) fields =
  match get_field fields with
    | Some v -> v
    | None -> failwith (sprintf "invalid header, field '%s' is required for '%s'"
                          field_name message_type_name)

let method_call_of_raw fields =
  let req x = get_required "method_call" x in
  Method_call(req path fields,
              fields._interface,
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

module Make_writer(Writer : Writer) =
struct
  open Writer

  let failwith msg = raise (Data_error msg)
  let ( >>= ) = bind

  (* +---------------------+
     | Writing of integers |
     +---------------------+ *)

  module type Int_writers = sig
    val byte_order_char : char

    val put_int16 : int -> unit Writer.t
    val put_int32 : int32 -> unit Writer.t
    val put_int64 : int64 -> unit Writer.t
    val put_uint16 : int -> unit Writer.t
    val put_uint32 : int32 -> unit Writer.t
    val put_uint64 : int64 -> unit Writer.t

    val put_uint : int -> unit Writer.t
      (* Output an int as an uint32 *)
  end

  let put_uint8 x = put_char (Char.unsafe_chr x)

  module LE_int_writers : Int_writers =
  struct
    let byte_order_char = 'l'

    let put_int16 v =
      (perform
         put_char (Char.unsafe_chr v);
         put_char (Char.unsafe_chr (v lsr 8)))
    let put_uint16 = put_int16

    let put_int32 v =
      (perform
         put_char (Char.unsafe_chr (Int32.to_int v));
         put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
         put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
         put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24))))
    let put_uint32 = put_int32

    let put_int64 v =
      (perform
         put_char (Char.unsafe_chr (Int64.to_int v));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56))))
    let put_uint64 = put_int64

    let put_uint v =
      (perform
         put_char (Char.unsafe_chr v);
         put_char (Char.unsafe_chr (v lsr 8));
         put_char (Char.unsafe_chr (v lsr 16));
         put_char (Char.unsafe_chr (v asr 24)))
  end

  module BE_int_writers : Int_writers =
  struct
    let byte_order_char = 'B'

    let put_uint16 v =
      (perform
         put_char (Char.unsafe_chr (v lsr 8));
         put_char (Char.unsafe_chr v))
    let put_int16 v =
      if v < 0 then
        put_uint16 (v lor 0x8000)
      else
        put_uint16 v

    let put_int32 v =
      (perform
         put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)));
         put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
         put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
         put_char (Char.unsafe_chr (Int32.to_int v)))
    let put_uint32 = put_int32

    let put_int64 v =
      (perform
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
         put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
         put_char (Char.unsafe_chr (Int64.to_int v)))
    let put_uint64 = put_int64

    let put_uint v =
      (perform
         put_char (Char.unsafe_chr (v asr 24));
         put_char (Char.unsafe_chr (v lsr 16));
         put_char (Char.unsafe_chr (v lsr 8));
         put_char (Char.unsafe_chr v))
  end

  (* +---------+
     | Padding |
     +---------+ *)

  let put_null = put_char '\000'

  (* Precalculation of padding monads *)
  let put_null0 = return ()
  let put_null1 = put_null
  let put_null2 = put_null >>= fun _ -> put_null1
  let put_null3 = put_null >>= fun _ -> put_null2
  let put_null4 = put_null >>= fun _ -> put_null3
  let put_null5 = put_null >>= fun _ -> put_null4
  let put_null6 = put_null >>= fun _ -> put_null5
  let put_null7 = put_null >>= fun _ -> put_null6

  let wpad2 i = match i land 1 with
    | 0 -> i + 0, put_null0
    | _ -> i + 1, put_null1

  let wpad4 i = match i land 3 with
    | 0 -> i + 0, put_null0
    | 1 -> i + 3, put_null3
    | 2 -> i + 2, put_null2
    | _ -> i + 1, put_null1

  let wpad8 i = match i land 7 with
    | 0 -> i + 0, put_null0
    | 1 -> i + 7, put_null7
    | 2 -> i + 6, put_null6
    | 3 -> i + 5, put_null5
    | 4 -> i + 4, put_null4
    | 5 -> i + 3, put_null3
    | 6 -> i + 2, put_null2
    | _ -> i + 1, put_null1

  (* +-----------------+
     | Message writing |
     +-----------------+ *)

  module Signature_writer = OBus_value.Make_signature_writer(Writer)

  type offset = int

  (* Each writing function act as follow:

     it takes:

     - the current offset, i.e. the size that will take serialized
     values already processed

     - the value to serialize

     then it compute the the length taken by the serialized value, the
     needed padding, add it to the current offset and return the
     result and a writer for the padding and the value. *)

  type 'a serializer = offset -> 'a -> offset * unit Writer.t

  let write2 f i v =
    let i, put_padding = wpad2 i in
    (i + 2, perform put_padding; f v)
  let write4 f i v =
    let i, put_padding = wpad4 i in
    (i + 4, perform put_padding; f v)
  let write8 f i v =
    let i, put_padding = wpad8 i in
    (i + 8, perform put_padding; f v)

  module Make_writer(Int_writers : Int_writers) =
  struct
    open Int_writers

    (* Serialize one string, without verifying it *)
    let wstring i str =
      let i, put_padding = wpad4 i and len = String.length str in
      (i + 4 + len + 1,
       perform
         put_padding;
         put_uint32 (Int32.of_int len);
         put_string str;
         put_null)

    (* Serialize a signature. *)
    let wsignature i s = match OBus_value.validate_signature s with
      | Some reason ->
          failwith (invalid_signature s reason)
      | None ->
          let len, put_signature = Signature_writer.write_signature s in
          if len > 255 then
            failwith (signature_too_long s len)
          else
            (i + 1 + len + 1,
             perform
               put_uint8 len;
               put_signature;
               put_null)

    let wobject_path i = function
      | [] ->
          let i, put_padding = wpad4 i in
          (i + 4 + 1 + 1,
           perform
             put_padding;
             put_uint32 1l;
             put_char '/';
             put_null)
      | path ->
          let i, put_padding = wpad4 i
          and len = List.fold_left (fun acc elt ->
                                      match OBus_path.validate_element elt with
                                        | Some error ->
                                            failwith (OBus_string.error_message error)
                                        | None ->
                                            1 + acc + String.length elt) 0 path in
          (i + 4 + len + 1,
           perform
             put_padding;
             put_uint32 (Int32.of_int len);
             List.fold_left (fun m elt -> perform
                               m;
                               put_char '/';
                               put_string elt) (return ()) path;
             put_null)

    (* The following function serialize DBus values. Actually this
       need a lot a redundant matching. With GADT it would be:

       {[
         let wbasic : 'a type_basic -> 'a serializer = function
           | Tbyte -> (fun i v -> (i + 1, put_char v))
           | Tint16 -> (fun i v -> write 2 put_int16 i v)
           ...

         let wsingle : 'a type_single -> 'a serializer = ...
         let wsequence : 'a type_sequence -> 'a serializer = ...
       ]}
    *)

    let wbasic i = function
      | Byte x -> (i + 1, put_char x)
      | Boolean x -> write4 put_uint i (match x with true -> 1 | false -> 0)
      | Int16 x -> write2 put_int16 i x
      | Int32 x -> write4 put_int32 i x
      | Int64 x -> write8 put_int64 i x
      | Uint16 x -> write2 put_uint16 i x
      | Uint32 x -> write4 put_uint32 i x
      | Uint64 x -> write8 put_uint64 i x
      | Double x -> write8 put_uint64 i (Int64.bits_of_float x)
      | String x -> begin match OBus_string.validate x with
          | Some error ->
              failwith (OBus_string.error_message error)
          | None ->
              wstring i x
        end
      | Signature x -> wsignature i x
      | Object_path x -> wobject_path i x

    let rec warray i padded_on_8 welement x =
      (* Array are serialized as follow:

         (1) padding to a 4-block alignement (for array size)
         (2) array size
         (3) alignement to array elements padding (even if the array is empty)
         (4) serialized elements

         The array size (2) is the size of serialized elements (4) *)

      let i, put_padding = wpad4 i in
      let i = i + 4 in
      (* After the size we are always padded on 4, so we only need to
         add padding if elements padding is 8 *)
      let i, put_initial_padding = if padded_on_8 then wpad8 i else i, return () in
      let j, put_array = List.fold_left (fun (i, f) x ->
                                           let i, g = welement i x in
                                           (i, perform f; g))
        (i, return ()) x in
      let len = j - i in
      if len < 0 || len > max_array_size then
        failwith (array_too_big len)
      else
        (j,
         perform
           put_padding;
           put_uint32 (Int32.of_int len);
           put_initial_padding;
           put_array)

    let rec wdict_entry i (k, v) =
      (* Dict-entries are serialized as follow:

         (1) alignement on a 8-block
         (2) serialized key
         (3) serialized value *)
      let i, put_padding = wpad8 i in
      let i, put_key = wbasic i k in
      let i, put_val = wsingle i v in
      (i, perform
         put_padding;
         put_key;
         put_val)

    and wsingle i = function
      | Basic x -> wbasic i x
      | Array(t, x) ->
          warray i (pad8_p t) wsingle x
      | Dict(tk, tv, x) ->
          warray i true wdict_entry x
      | Structure x ->
          (* Structure are serialized as follow:

             (1) alignement to an 8-block
             (2) serialized contents *)
          let i, put_padding = wpad8 i in
          let i, put_sequence = wsequence i x in
          (i, perform put_padding; put_sequence)
      | Variant x ->
          (* Variant are serialized as follow:

             (1) marshaled variant signature
             (2) serialized contents *)
          let i, put_signature = wsignature i [OBus_value.type_of_single x] in
          let i, put_variant = wsingle i x in
          (i,
           perform
             put_signature;
             put_variant)

    and wsequence i = function
      | [] -> (i, return ())
      | x :: l ->
          let i, put_head = wsingle i x in
          let i, put_tail = wsequence i l in
          (i,
           perform
             put_head;
             put_tail)

    (* Serialize one complete message. The starting offset is always
       0. *)
    let wmessage msg =
      (* Compute ``raw'' headers *)
      let code, fields = match msg.typ with
        | Method_call(path, interface, member) ->
            ('\001',
             { _path = Some path;
               _interface = interface;
               _member = Some member;
               _error_name = None;
               _reply_serial = None;
               _destination = msg.destination;
               _sender = msg.sender;
               _signature = type_of_sequence msg.body })
        | Method_return(reply_serial) ->
            ('\002',
             { _path = None;
               _interface = None;
               _member = None;
               _error_name = None;
               _reply_serial = Some reply_serial;
               _destination = msg.destination;
               _sender = msg.sender;
               _signature = type_of_sequence msg.body })
        | Error(reply_serial, error_name) ->
            ('\003',
             { _path = None;
               _interface = None;
               _member = None;
               _error_name = Some error_name;
               _reply_serial = Some reply_serial;
               _destination = msg.destination;
               _sender = msg.sender;
               _signature = type_of_sequence msg.body })
        | Signal(path, interface, member) ->
            ('\004',
             { _path = Some path;
               _interface = Some interface;
               _member = Some member;
               _error_name = None;
               _reply_serial = None;
               _destination = msg.destination;
               _sender = msg.sender;
               _signature = type_of_sequence msg.body })
      in

      (* Header field serializer *)
      let wfield_real code typ f v (i, wacc) =
        (* Each header field is a structure, so we need to be aligned
           on 8 *)
        let i, put_padding = wpad8 i in
        (* Add 4 to i for:
           - the field code
           - the signature length (=1)
           - the signature (of length 1)
           - the null byte *)
        let i, writer = f (i + 4) v in
        (i,
         perform
           wacc;
           put_padding;
           put_uint8 code;
           put_uint8 1;
           put_char (OBus_value.basic_type_code typ);
           put_null;
           writer) in

      (* Write a field if defined *)
      let wfield code typ f field acc = match field with
        | None -> acc
        | Some v -> wfield_real code typ f v acc in

      (* Validate and write a field if defined *)
      let wfield_test code test field acc = match field with
        | None -> acc
        | Some v -> match test v with
            | Some error ->
                failwith (OBus_string.error_message error)
            | None ->
                wfield_real code Tstring wstring v acc in

      let acc = 0, return () in
      let acc = wfield 1 Tobject_path wobject_path fields._path acc in
      let acc = wfield_test 2 OBus_name.validate_interface fields._interface acc in
      let acc = wfield_test 3 OBus_name.validate_member fields._member acc in
      let acc = wfield_test 4 OBus_name.validate_error fields._error_name acc in
      let acc = wfield 5 Tuint32 (write4 put_uint32) fields._reply_serial acc in
      let acc = wfield_test 6 OBus_name.validate_bus fields._destination acc in
      let acc = wfield_test 7 OBus_name.validate_bus fields._sender acc in
      let fields_length, put_fields = wfield_real 8 Tsignature wsignature fields._signature acc in

      if fields_length > max_array_size then
        failwith (array_too_big fields_length);

      let body_length, put_body = wsequence 0 msg.body in

      (* The message start aligned on an 8-boundary after the header,
         and header fields start at offset #16, so: *)
      let padded_fields_length, put_padding_before_body = wpad8 fields_length in

      (* and the total message size is: *)
      let total_length = 16 + padded_fields_length + body_length in

      if total_length > max_message_size then
        failwith (message_too_big total_length);

      (total_length,
       perform
         (* byte #0 : byte-order *)
         put_char byte_order_char;
         (* byte #1 : message type code *)
         put_char code;
         (* byte #2 : message flags *)
         put_uint8
           ((if msg.flags.no_reply_expected then 1 else 0) lor
              (if msg.flags.no_auto_start then 2 else 0));
         (* byte #3 : protocol version *)
         put_uint8 OBus_info.protocol_version;
         (* byte #4-7 : body length *)
         put_uint body_length;
         (* byte #8-11 : serial *)
         put_uint32 msg.serial;
         (* byte #12-15 : fields length *)
         put_uint fields_length;
         (* header fields *)
         put_fields;
         (* padding between header and body *)
         put_padding_before_body;
         (* message body *)
         put_body)
  end

  module LEWriter = Make_writer(LE_int_writers)
  module BEWriter = Make_writer(BE_int_writers)

  let put_message ?(byte_order=native_byte_order) msg =
    match byte_order with
      | Little_endian ->
          LEWriter.wmessage msg
      | Big_endian ->
          BEWriter.wmessage msg
end

module Make_reader(Reader : Reader) =
struct
  open Reader

  let failwith msg = Reader.fail (Protocol_error msg)
  let out_of_bounds () = failwith "out of bounds"
  let ( >>= ) = bind

  (* +---------------------+
     | Reading of integers |
     +---------------------+ *)

  module type Int_readers = sig
    val get_int16 : int Reader.t
    val get_int32 : int32 Reader.t
    val get_int64 : int64 Reader.t
    val get_uint16 : int Reader.t
    val get_uint32 : int32 Reader.t
    val get_uint64 : int64 Reader.t

    val get_uint : int Reader.t
      (* Input an uint32 as an int *)
  end

  let get_uint8 = get_char >>= (fun n -> return (Char.code n))

  module LE_int_readers : Int_readers =
  struct
    let get_int16 =
      (perform
         v0 <-- get_char;
         v1 <-- get_char;
         let v = (Char.code v0) lor (Char.code v1 lsl 8) in
         if v land (1 lsl 15) = 0 then
           return v
         else
           return ((-1 land (lnot 0x7fff)) lor v))

    let get_uint16 =
      (perform
         v0 <-- get_char;
         v1 <-- get_char;
         return (Char.code v0 lor (Char.code v1 lsl 8)))

    let get_int32 =
      (perform
         v0 <-- get_char;
         v1 <-- get_char;
         v2 <-- get_char;
         v3 <-- get_char;
         return (Int32.logor
                   (Int32.logor
                      (Int32.of_int (Char.code v0))
                      (Int32.shift_left (Int32.of_int (Char.code v1)) 8))
                   (Int32.logor
                      (Int32.shift_left (Int32.of_int (Char.code v2)) 16)
                      (Int32.shift_left (Int32.of_int (Char.code v3)) 24))))
    let get_uint32 = get_int32

    let get_int64 =
      (perform
         v0 <-- get_char;
         v1 <-- get_char;
         v2 <-- get_char;
         v3 <-- get_char;
         v4 <-- get_char;
         v5 <-- get_char;
         v6 <-- get_char;
         v7 <-- get_char;
         return (Int64.logor
                   (Int64.logor
                      (Int64.logor
                         (Int64.of_int (Char.code v0))
                         (Int64.shift_left (Int64.of_int (Char.code v1)) 8))
                      (Int64.logor
                         (Int64.shift_left (Int64.of_int (Char.code v2)) 16)
                         (Int64.shift_left (Int64.of_int (Char.code v3)) 24)))
                   (Int64.logor
                      (Int64.logor
                         (Int64.shift_left (Int64.of_int (Char.code v4)) 32)
                         (Int64.shift_left (Int64.of_int (Char.code v5)) 40))
                      (Int64.logor
                         (Int64.shift_left (Int64.of_int (Char.code v6)) 48)
                         (Int64.shift_left (Int64.of_int (Char.code v7)) 56)))))
    let get_uint64 = get_int64

    let get_uint =
      (perform
         v0 <-- get_char;
         v1 <-- get_char;
         v2 <-- get_char;
         v3 <-- get_char;
         return (Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)))
  end

  module BE_int_readers : Int_readers =
  struct
    let get_int16 =
      (perform
         v1 <-- get_char;
         v0 <-- get_char;
         let v = Char.code v0 lor (Char.code v1 lsl 8) in
         if v land (1 lsl 15) = 0 then
           return v
         else
           return ((-1 land (lnot 0x7fff)) lor v))

    let get_uint16 =
      (perform
         v1 <-- get_char;
         v0 <-- get_char;
         return (Char.code v0 lor (Char.code v1 lsl 8)))

    let get_int32 =
      (perform
         v3 <-- get_char;
         v2 <-- get_char;
         v1 <-- get_char;
         v0 <-- get_char;
         return (Int32.logor
                   (Int32.logor
                      (Int32.of_int (Char.code v0))
                      (Int32.shift_left (Int32.of_int (Char.code v1)) 8))
                   (Int32.logor
                      (Int32.shift_left (Int32.of_int (Char.code v2)) 16)
                      (Int32.shift_left (Int32.of_int (Char.code v3)) 24))))
    let get_uint32 = get_int32

    let get_int64 =
      (perform
         v7 <-- get_char;
         v6 <-- get_char;
         v5 <-- get_char;
         v4 <-- get_char;
         v3 <-- get_char;
         v2 <-- get_char;
         v1 <-- get_char;
         v0 <-- get_char;
         return (Int64.logor
                   (Int64.logor
                      (Int64.logor
                         (Int64.of_int (Char.code v0))
                         (Int64.shift_left (Int64.of_int (Char.code v1)) 8))
                      (Int64.logor
                         (Int64.shift_left (Int64.of_int (Char.code v2)) 16)
                         (Int64.shift_left (Int64.of_int (Char.code v3)) 24)))
                   (Int64.logor
                      (Int64.logor
                         (Int64.shift_left (Int64.of_int (Char.code v4)) 32)
                         (Int64.shift_left (Int64.of_int (Char.code v5)) 40))
                      (Int64.logor
                         (Int64.shift_left (Int64.of_int (Char.code v6)) 48)
                         (Int64.shift_left (Int64.of_int (Char.code v7)) 56)))))
    let get_uint64 = get_int64

    let get_uint =
      (perform
         v3 <-- get_char;
         v2 <-- get_char;
         v1 <-- get_char;
         v0 <-- get_char;
         return (Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)))
  end

  (* +---------+
     | Padding |
     +---------+ *)

  let get_null cont =
    get_char >>= function
      | '\000' -> cont
      | _ -> failwith "uninitialized padding"

  let get_null0 = return ()
  let get_null1 = get_null get_null0
  let get_null2 = get_null get_null1
  let get_null3 = get_null get_null2
  let get_null4 = get_null get_null3
  let get_null5 = get_null get_null4
  let get_null6 = get_null get_null5
  let get_null7 = get_null get_null6

  let rpad2 i = match i land 1 with
    | 0 -> i + 0, get_null0
    | _ -> i + 1, get_null1

  let rpad4 i = match i land 3 with
    | 0 -> i + 0, get_null0
    | 1 -> i + 3, get_null3
    | 2 -> i + 2, get_null2
    | _ -> i + 1, get_null1

  let rpad8 i = match i land 7 with
    | 0 -> i + 0, get_null0
    | 1 -> i + 7, get_null7
    | 2 -> i + 6, get_null6
    | 3 -> i + 5, get_null5
    | 4 -> i + 4, get_null4
    | 5 -> i + 3, get_null3
    | 6 -> i + 2, get_null2
    | _ -> i + 1, get_null1

  (* +-----------------+
     | Message reading |
     +-----------------+ *)

  module Signature_reader = OBus_value.Make_signature_reader
    (struct
       type 'a t = int ref -> 'a Reader.t
       let bind m f remaining = Reader.bind (m remaining) (fun x -> f x remaining)
       let return x _ = Reader.return x
       let failwith msg _ = Reader.fail (Protocol_error msg)
       let get_char remaining = match !remaining with
         | 0 -> Reader.fail (Protocol_error "premature end of signature")
         | n -> remaining := n - 1; Reader.get_char
       let eof remaining = Reader.return (!remaining = 0)
     end)

  (* Each reading function act as follow:

     it takes the current offset, the limit and return a monad which
     read a value and return the new offset with the value *)

  let read1 f = (); fun i limit ->
    let i = i + 1 in
    if i > limit then
      out_of_bounds ()
    else
      f >>= fun x -> return (i, x)

  let read2 f = (); fun i limit ->
    let i, get_padding = rpad2 i in
    let i = i + 2 in
    if i > limit then
      out_of_bounds ()
    else
      (perform
         get_padding;
         v <-- f;
         return (i, v))

  let read4 f = (); fun i limit ->
    let i, get_padding = rpad4 i in
    let i = i + 4 in
    if i > limit then
      out_of_bounds ()
    else
      (perform
         get_padding;
         v <-- f;
         return (i, v))

  let read8 f = (); fun i limit ->
    let i, get_padding = rpad8 i in
    let i = i + 8 in
    if i > limit then
      out_of_bounds ()
    else
      (perform
         get_padding;
         v <-- f;
         return (i, v))

  module Make_reader(Int_readers : Int_readers) =
  struct
    open Int_readers

    let ruint8 = read1 get_uint8
    let ruint = read4 get_uint
    let ruint32 = read4 get_uint32
    let rstring i size =
      (perform
         (i, len) <-- ruint i size;
         let i = i + len + 1 in
         if len < 0 || i > size then
           out_of_bounds ()
         else
           perform
             str <-- get_string len;
             get_char >>= function
               | '\000' -> return (i, str)
               | _ -> failwith "string terminal null byte missing")
    let rsignature i size =
      (perform
         (i, len) <-- ruint8 i size;
         let i = i + len + 1 in
         if len < 0 || i > size then
           out_of_bounds ()
         else
           Signature_reader.read_signature (ref len) >>= fun s ->
             match OBus_value.validate_signature s with
               | Some reason ->
                   failwith (invalid_signature s reason)
               | None ->
                   get_char >>= function
                     | '\000' -> return (i, s)
                     | _ -> failwith "signature terminal null byte missing")
    let rtype i size =
      rsignature i size >>= fun (i, s) -> match s with
        | [t] -> return (i, t)
        | [] -> failwith "empty variant signature"
        | _ -> failwith (sprintf
                           "variant signature contains more than one single type: %s"
                           (string_of_signature s))
    let robject_path i size =
      rstring i size >>= fun (i, str) ->
        try
          return (i, OBus_path.of_string str)
        with
            OBus_string.Invalid_string error -> failwith (OBus_string.error_message error)

    (* Get a fixed-size value and box it, without padding and size
       checking *)
    let get_vbyte = get_char >>= fun ch -> return (Byte ch)
    let get_vboolean = get_uint >>= function
      | 0 -> return (Boolean false)
      | 1 -> return (Boolean true)
      | n -> failwith (sprintf "invalid boolean value: %d" n)
    let get_vint16 = get_int16 >>= fun x -> return (Int16 x)
    let get_vint32 = get_int32 >>= fun x -> return (Int32 x)
    let get_vint64 = get_int64 >>= fun x -> return (Int64 x)
    let get_vuint16 = get_uint16 >>= fun x -> return (Uint16 x)
    let get_vuint32 = get_uint32 >>= fun x -> return (Uint32 x)
    let get_vuint64 = get_uint64 >>= fun x -> return (Uint64 x)
    let get_vdouble = get_uint64 >>= fun x -> return (Double(Int64.float_of_bits x))

    let rbasic = function
      | Tbyte -> read1 get_vbyte
      | Tboolean -> read4 get_vboolean
      | Tint16 -> read2 get_vint16
      | Tint32 -> read4 get_vint32
      | Tint64 -> read8 get_vint64
      | Tuint16 -> read2 get_vuint16
      | Tuint32 -> read4 get_vuint32
      | Tuint64 -> read8 get_vuint64
      | Tdouble -> read8 get_vdouble
      | Tstring ->
          (fun i size ->
             rstring i size >>= fun (i, str) -> match OBus_string.validate str with
               | Some error ->
                   failwith (OBus_string.error_message error)
               | None ->
                   return (i, String str));
      | Tsignature ->
          (fun i size -> perform
             (i, s) <-- rsignature i size;
             return (i, Signature s))
      | Tobject_path -> (fun i size -> robject_path i size >>= fun (i, v) -> return (i, Object_path v))

    (* Read elememts of an array *)
    let rec rarray_elements i limit reader =
      if i = limit then
        return []
      else
        (perform
           (i, x) <-- reader i limit;
           l <-- rarray_elements i limit reader;
           return (x :: l))

    let rarray padded_on_8 relement i size =
      (perform
         (i, len) <-- ruint i size;
         let i, get_padding = if padded_on_8 then rpad8 i else (i, return ()) in
         let limit = i + len in
         if len < 0 || len > max_array_size then
           failwith (array_too_big len)
         else if limit > size then
           out_of_bounds ()
         else perform
           get_padding;
           l <-- rarray_elements i limit relement;
           return (limit, l))

    let rec rsingle = function
      | Tbasic t ->
          let reader = rbasic t in
          (fun i size -> reader i size >>= fun (i, x) -> return (i, basic x))
      | Tarray t ->
          let reader = rsingle t in
          (fun i size ->
             perform
               (i, l) <-- rarray (pad8_p t) reader i size;
               return (i, array t l))
      | Tdict(tk, tv) ->
          let kreader = rbasic tk
          and vreader = rsingle tv in
          (fun i size -> perform
             (i, l) <-- rarray true
               (fun i size ->
                  let i, get_padding = rpad8 i in
                  if i > size then
                    out_of_bounds ()
                  else perform
                    get_padding;
                    (i, k) <-- kreader i size;
                    (i, v) <-- vreader i size;
                    return (i, (k, v))) i size;
             return (i, dict tk tv l))
      | Tstructure tl ->
          let reader = rsequence tl in
          (fun i size ->
             let i, get_padding = rpad8 i in
             if i > size then
               out_of_bounds ()
             else perform
               get_padding;
               (i, l) <-- reader i size;
               return (i, structure l))
      | Tvariant ->
          (fun i size -> perform
             (i, t) <-- rtype i size;
             (i, v) <-- rsingle t i size;
             return (i, variant v))

    and rsequence = function
      | [] -> (fun i size -> return (i, []))
      | t :: tl ->
          let head_reader = rsingle t
          and tail_reader = rsequence tl in
          (fun i size -> perform
             (i, x) <-- head_reader i size;
             (i, l) <-- tail_reader i size;
             return (i, x :: l))

    let rfields limit =
      let rfield code typ reader i =
        rtype i limit >>= fun (i, t) -> match t with
          | Tbasic t' when t' = typ ->
              reader i limit;
          | _ ->
              failwith (sprintf "invalid header field signature for code %d: %S, should be %S"
                          code (string_of_signature [t]) (string_of_signature [Tbasic typ]))
      in
      let rfield_test code test i =
        rfield code Tstring rstring i >>= fun ((_, name) as x) ->
          match test name with
            | Some error ->
                failwith (OBus_string.error_message error)
            | None ->
                return x
      in

      let rec aux (i, acc) =
        if i = limit then
          return acc
        else
          let i, get_padding = rpad8 i in
          let i = i + 1 in
          (perform
             get_padding;
             get_uint8 >>=
               (function
                  | 1 -> rfield 1 Tobject_path robject_path i >>= (fun (i, x) -> aux (i, { acc with _path = Some x }))
                  | 2 -> rfield_test 2 OBus_name.validate_interface i >>= (fun (i, x) -> aux (i, { acc with _interface = Some x }))
                  | 3 -> rfield_test 3 OBus_name.validate_member i >>= (fun (i, x) -> aux (i, { acc with _member = Some x }))
                  | 4 -> rfield_test 4 OBus_name.validate_error i >>= (fun (i, x) -> aux (i, { acc with _error_name = Some x }))
                  | 5 -> rfield 5 Tuint32 ruint32 i >>= (fun (i, x) -> aux (i, { acc with _reply_serial = Some x }))
                  | 6 -> rfield_test 6 OBus_name.validate_bus i >>= (fun (i, x) -> aux (i, { acc with _destination = Some x }))
                  | 7 -> rfield_test 7 OBus_name.validate_bus i >>= (fun (i, x) -> aux (i, { acc with _sender = Some x }))
                  | 8 -> rfield 8 Tsignature rsignature i >>= (fun (i, x) -> aux (i, { acc with _signature = x }))
                  | n ->
                      (perform
                         (i, t) <-- rtype i limit;
                         (i, _) <-- rsingle t i limit;
                         aux (i, acc))))
      in
      aux (0, empty_fields)

    let rmessage =
      (perform
         message_maker <-- get_char >>=
           (function
              | '\001' -> return method_call_of_raw
              | '\002' -> return method_return_of_raw
              | '\003' -> return error_of_raw
              | '\004' -> return signal_of_raw
              | c -> failwith (sprintf "unknown message type: %d" (Char.code c)));

         n <-- get_uint8;
         let flags = { no_reply_expected = n land 1 = 1;
                       no_auto_start = n land 2 = 2 } in

         protocol_version <-- get_uint8;

         (* Check the protocol version first, since we can not do
            anything if it is not the same as our *)
         if protocol_version <> protocol_version then
           failwith (invalid_protocol_version protocol_version)

         else perform
           body_length <-- get_uint;
           serial <-- get_uint32;
           fields_length <-- get_uint;

           (* Header fields array start on byte #16 and message start
              aligned on a 8-boundary after it, so we have: *)
           let padded_fields_length, get_padding_before_body = rpad8 fields_length in
           let total_length = 16 + padded_fields_length + body_length in

           (* Safety checking *)
           if fields_length < 0 || fields_length > max_array_size then
             failwith (array_too_big fields_length)

           else if body_length < 0 || total_length > max_message_size then
             failwith (message_too_big total_length)

           else perform
             fields <-- rfields fields_length;
             get_padding_before_body;
             (i, body) <-- rsequence fields._signature 0 body_length;
             if i = body_length then
               try
                 return { flags = flags;
                          sender = fields._sender;
                          destination = fields._destination;
                          serial = serial;
                          typ = message_maker fields;
                          body = body }
               with
                   (* If fields are invalid *)
                   Failure msg -> failwith msg
             else
               failwith "junk bytes after message")
  end

  module LEReader = Make_reader(LE_int_readers)
  module BEReader = Make_reader(BE_int_readers)

  let get_message =
    get_char >>= function
      | 'l' -> LEReader.rmessage
      | 'B' -> BEReader.rmessage
      | ch -> failwith (invalid_byte_order ch)
end

(* +------------------+
   | Size computation |
   +------------------+ *)

let get_message_size buf ofs =

  let unsafe_get_uint map_ofs i =
    let v0 = String.unsafe_get buf (map_ofs (i + 0))
    and v1 = String.unsafe_get buf (map_ofs (i + 1))
    and v2 = String.unsafe_get buf (map_ofs (i + 2))
    and v3 = String.unsafe_get buf (map_ofs (i + 3)) in
    Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)
  in

  if ofs < 0 || ofs + 16 >= String.length buf then
    raise (Invalid_argument "OBus_lowlevel.get_message_size")

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

(* +-------------------------------------------------+
   | Serialization/deserialization over lwt channels |
   +-------------------------------------------------+ *)

open Unix
open Lwt

module Lwt_writer = Make_writer
  (struct
     type 'a t = Lwt_chan.out_channel -> 'a Lwt.t
     let bind m f oc = Lwt.bind (m oc) (fun x -> f x oc)
     let return x  _ = Lwt.return x
     let fail exn _ = Lwt.fail exn
     let put_char ch oc = Lwt_chan.output_char oc ch
     let put_string str oc = Lwt_chan.output_string oc str
   end)

module Lwt_reader = Make_reader
  (struct
     type 'a t = Lwt_chan.in_channel -> 'a Lwt.t
     let bind m f ic = Lwt.bind (m ic) (fun x -> f x ic)
     let return x _ = Lwt.return x
     let fail exn _ = Lwt.fail exn
     let get_char ic = Lwt_chan.input_char ic
     let get_string len ic =
       let str = String.create len in
       Lwt.bind
         (Lwt_chan.really_input ic str 0 len)
         (fun _ -> Lwt.return str)
   end)

let lwt_chan_put_message ?byte_order oc msg = snd (Lwt_writer.put_message ?byte_order msg) oc
let lwt_chan_get_message ic = Lwt_reader.get_message ic

(* +--------------------------------------------+
   | Serialization/deserialization over strings |
   +--------------------------------------------+ *)

type ptr = { buffer : string; mutable offset : int }

module Ptr =
struct
  type 'a t = ptr -> 'a
  let bind m f ptr = f (m ptr) ptr
  let return x _ = x
  let fail exn ptr = raise exn
end

module String_reader = Make_reader
  (struct
     include Ptr
     let get_char ptr =
       let n = ptr.offset in
       ptr.offset <- n + 1;
       ptr.buffer.[n]
     let get_string len ptr =
       let n = ptr.offset in
       ptr.offset <- n + len;
       String.sub ptr.buffer n len
   end)

module String_writer = Make_writer
  (struct
     include Ptr
     let put_char ch ptr =
       let n = ptr.offset in
       ptr.buffer.[n] <- ch;
       ptr.offset <- n + 1
     let put_string str ptr =
       let n = ptr.offset and len = String.length str in
       String.blit str 0 ptr.buffer n len;
       ptr.offset <- n + len
   end)

let string_put_message ?byte_order msg =
  let size, writer = String_writer.put_message ?byte_order msg in
  let buf = String.create size in
  writer { buffer = buf; offset = 0 };
  buf

let string_get_message buf =
  String_reader.get_message { buffer = buf; offset = 0 }

(* +----------------------------------------------------+
   | Serialization/deserialization over custom channels |
   +----------------------------------------------------+ *)

let default_buffer_size = ref(
  let default = 16 * 1024 in
  try
    let x = int_of_string(Sys.getenv "OBUS_BUFFER_SIZE") in
    if x <= 0 then begin
      Log.error "OBUS_BUFFER_SIZE contains a negative or null size (%d)" x;
      default
    end else if x > Sys.max_string_length then begin
      Log.error "OBUS_BUFFER_SIZE contains a too big size (%d)" x;
      default
    end else begin
      Log.debug "using a default buffer size of %d" x;
      x
    end
  with
      _ ->
        default
)

let get_default_buffer_size = !default_buffer_size
let set_default_buffer_size sz =
  if sz < 1 then
    invalid_arg "OBus_lowlevel.set_default_buffer_size: size must be >= 1"
  else if sz > Sys.max_string_length then
    invalid_arg "OBus_lowlevel.set_default_buffer_size: size must be < Sys.max_string_length"
  else
    default_buffer_size := sz

type ochan = {
  oc_buffer : string;
  mutable oc_ptr : int;
  oc_perform_io : string -> int -> int -> int Lwt.t;
  mutable oc_writers : int;
  mutable oc_serializer : unit Lwt.t;
}

type ichan = {
  ic_buffer : string;
  mutable ic_ptr : int;
  mutable ic_max : int;
  ic_perform_io : string -> int -> int -> int Lwt.t;
  mutable ic_serializer : unit Lwt.t;
}

(* Important note: all operations assumes that:

   - things are serialized and do not need locks
   - index are always valid

   since these functions are not available from the outside the only
   thing to check is that the serialization code is correct *)

let make_ochan ?buffer_size perform_io = {
  oc_buffer = String.create (match buffer_size with
                               | Some s -> s
                               | None -> !default_buffer_size);
  oc_ptr = 0;
  oc_perform_io = perform_io;
  oc_writers = 0;
  oc_serializer = Lwt.return ();
}

let ochan_enqueue oc f =
  let new_w = Lwt.wait () in
  let old_w = oc.oc_serializer in
  oc.oc_serializer <- new_w;
  old_w >>= fun _ ->
    Lwt.finalize f
      (fun _ ->
         Lwt.wakeup new_w ();
         Lwt.return ())

let flush_partial oc =
  oc.oc_perform_io oc.oc_buffer 0 oc.oc_ptr >>= fun n ->
    if n < oc.oc_ptr then
      String.unsafe_blit oc.oc_buffer n oc.oc_buffer 0 (oc.oc_ptr - n);
    oc.oc_ptr <- oc.oc_ptr - n;
    return ()

let rec flush_total oc =
  flush_partial oc >>= fun _ ->
    if oc.oc_ptr > 0 then
      flush_total oc
    else
      return ()

let yield_flush oc =
  Lwt_unix.yield () >>= fun _ ->
    if oc.oc_writers > 0 then
      (* At least one other thread is currently writing to the buffer,
         let it flush the buffer when it will finish *)
      return ()
    else
      ochan_enqueue oc (fun _ ->
                          if oc.oc_writers > 0 then
                            return ()
                          else
                            flush_total oc)

let ochan_flush oc = ochan_enqueue oc (fun _ -> flush_total oc)

let rec put_char ch oc =
  let ptr = oc.oc_ptr in
  if ptr < String.length oc.oc_buffer then begin
    oc.oc_ptr <- ptr + 1;
    String.unsafe_set oc.oc_buffer ptr ch;
    return ()
  end else
    flush_partial oc >>= fun _ -> put_char ch oc

let rec put_string_rec oc str ofs len =
  if len = 0 then
    return ()
  else begin
    let buf_size = String.length oc.oc_buffer in
    let avail = buf_size - oc.oc_ptr in
    if avail >= len then begin
      String.unsafe_blit str ofs oc.oc_buffer oc.oc_ptr len;
      oc.oc_ptr <- oc.oc_ptr + len;
      return ()
    end else begin
      String.unsafe_blit str ofs oc.oc_buffer oc.oc_ptr avail;
      oc.oc_ptr <- buf_size;
      flush_partial oc >>= fun _ -> put_string_rec oc str (ofs + avail) (len - avail)
    end
  end

let put_string str oc = put_string_rec oc str 0 (String.length str)

let make_ichan ?buffer_size perform_io = {
  ic_buffer = String.create (match buffer_size with
                               | Some s -> s
                               | None -> !default_buffer_size);
  ic_ptr = 0;
  ic_max = 0;
  ic_perform_io = perform_io;
  ic_serializer = Lwt.return ();
}

let ichan_enqueue ic f =
  let new_w = Lwt.wait () in
  let old_w = ic.ic_serializer in
  ic.ic_serializer <- new_w;
  old_w >>= fun _ ->
    Lwt.finalize f
      (fun _ ->
         Lwt.wakeup new_w ();
         Lwt.return ())

let rec get_char ic =
  let ptr = ic.ic_ptr in
  if ptr = ic.ic_max then begin
    ic.ic_perform_io ic.ic_buffer 0 (String.length ic.ic_buffer) >>= function
      | 0 ->
          fail End_of_file
      | n ->
          ic.ic_ptr <- 1;
          ic.ic_max <- n;
          return (String.unsafe_get ic.ic_buffer 0)
  end else begin
    ic.ic_ptr <- ptr + 1;
    return (String.unsafe_get ic.ic_buffer ptr)
  end

let rec get_string_rec ic str ofs len =
  if len = 0 then
    return str
  else
    let avail = ic.ic_max - ic.ic_ptr in
    if avail > 0 then begin
      let len' = min len avail in
      String.unsafe_blit ic.ic_buffer ic.ic_ptr str ofs len';
      ic.ic_ptr <- ic.ic_ptr + len';
      get_string_rec ic str (ofs + len') (len - len')
    end else begin
      ic.ic_perform_io ic.ic_buffer 0 (String.length ic.ic_buffer) >>= function
        | 0 ->
            fail End_of_file
        | n ->
            let len' = min len n in
            String.unsafe_blit ic.ic_buffer 0 str ofs len';
            ic.ic_ptr <- len';
            ic.ic_max <- n;
            get_string_rec ic str (ofs + len') (len - len')
    end

let get_string len ic =
  let str = String.create len in
  get_string_rec ic str 0 len

module Chan_writer = Make_writer
  (struct
     type 'a t = ochan -> 'a Lwt.t
     let bind m f oc = Lwt.bind (m oc) (fun x -> f x oc)
     let return x  _ = Lwt.return x
     let fail exn _ = Lwt.fail exn
     let put_char = put_char
     let put_string = put_string
   end)

module Chan_reader = Make_reader
  (struct
     type 'a t = ichan -> 'a Lwt.t
     let bind m f ic = Lwt.bind (m ic) (fun x -> f x ic)
     let return x _ = Lwt.return x
     let fail exn _ = Lwt.fail exn
     let get_char = get_char
     let get_string = get_string
   end)

let chan_put_message ?byte_order oc msg =
  ochan_enqueue oc (fun _ ->
                      oc.oc_writers <- oc.oc_writers + 1;
                      Lwt.finalize
                        (fun _ ->
                           snd (Chan_writer.put_message ?byte_order msg) oc)
                        (fun _ ->
                           oc.oc_writers <- oc.oc_writers - 1;
                           if oc.oc_writers = 0 then
                             ignore (yield_flush oc);
                           return ()))

let chan_get_message ic =
  ichan_enqueue ic (fun _ -> Chan_reader.get_message ic)

let auth_stream_of_channels (ic, oc) =
  (* Serialize everything just in case the user do silly things (and
     we absolutly do not care about performance during
     authentification) *)
  OBus_auth.make_stream
    ~get_char:(fun _ -> ichan_enqueue ic (fun _ -> get_char ic))
    ~put_char:(fun c -> ochan_enqueue oc (fun _ -> put_char c oc))
    ~flush:(fun _ -> ochan_flush oc)

(* +-----------+
   | Transport |
   +-----------+ *)

open OBus_address

module Log = Log.Make(struct let section = "transport" end)

type transport = {
  recv : unit -> OBus_message.t Lwt.t;
  send : OBus_message.t -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
}

let make_transport ~recv ~send ~shutdown = { recv = recv; send = send; shutdown = shutdown }

let recv { recv = recv } = recv ()
let send { send = send } message = send message
let shutdown { shutdown = shutdown } = shutdown ()

let chans_of_fd fd = (make_ichan (fun str ofs len -> Lwt_unix.read fd str ofs len),
                      make_ochan (fun str ofs len -> Lwt_unix.write fd str ofs len))

let transport_of_channels (ic, oc) =
  { recv = (fun _ -> chan_get_message ic);
    send = (fun msg -> chan_put_message oc msg);
    shutdown = (fun _ -> ochan_flush oc) }

let socket fd chans =
  let tr = transport_of_channels chans in
  { tr with
      shutdown = fun _ ->
        Lwt.finalize tr.shutdown
          (fun _ ->
             Lwt_unix.shutdown fd SHUTDOWN_ALL;
             Lwt_unix.close fd;
             Lwt.return ()) }

let loopback _ =
  let queue = MQueue.create () in
  { recv = (fun _ -> MQueue.get queue);
    send = (fun m -> MQueue.put m queue; return ());
    shutdown = (fun _ ->
                  Queue.iter (fun w -> wakeup_exn w (Failure "transport closed")) queue.MQueue.waiters;
                  Queue.clear queue.MQueue.waiters;
                  Queue.clear queue.MQueue.queued;
                  return ()) }

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  catch
    (fun _ -> perform
       Lwt_unix.connect fd addr;
       return fd)
    (fun exn -> Lwt_unix.close fd; fail exn)

let transport_of_addresses ?mechanisms addresses =
  let rec try_one domain typ addr fallback x =
    catch
      (fun _ -> perform
         fd <-- make_socket domain typ addr;
         let chans = chans_of_fd fd in
         guid <-- OBus_auth.client_authenticate ?mechanisms (auth_stream_of_channels chans);
         return (guid, socket fd chans))
      (fun exn ->
         Log.log "transport creation failed for address: domain=%s typ=%s addr=%s: %s"
           (match domain with
              | PF_UNIX -> "unix"
              | PF_INET -> "inet"
              | PF_INET6 -> "inet6")
           (match typ with
              | SOCK_STREAM -> "stream"
              | SOCK_DGRAM -> "dgram"
              | SOCK_RAW -> "raw"
              | SOCK_SEQPACKET -> "seqpacket")
           (match addr with
              | ADDR_UNIX path -> sprintf "unix(%s)" path
              | ADDR_INET(addr, port) -> sprintf "inet(%s,%d)" (string_of_inet_addr addr) port)
           (Util.string_of_exn exn);
         fallback x)
  in
  let rec aux = function
    | [] -> failwith "no working DBus address found"
    | (desc, _) :: rest ->
        match desc with
          | Unix_path path ->
              try_one PF_UNIX SOCK_STREAM (ADDR_UNIX(path))
                aux rest

          | Unix_abstract path ->
              try_one PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ path))
                aux rest

          | Unix_tmpdir _ ->
              Log.error "unix tmpdir can only be used as a listening address";
              aux rest

          | Tcp { tcp_host = host; tcp_port = port; tcp_family = family } ->
              let opts = [AI_SOCKTYPE SOCK_STREAM] in
              let opts = match family with
                | Some `Ipv4 -> AI_FAMILY PF_INET :: opts
                | Some `Ipv6 -> AI_FAMILY PF_INET6 :: opts
                | None -> opts in
              let rec try_all = function
                | [] -> aux rest
                | ai :: ais ->
                    try_one ai.ai_family ai.ai_socktype ai.ai_addr
                      try_all ais
              in
              try_all (getaddrinfo host port opts)

          | Autolaunch ->
              (perform
                 addresses <-- catch
                   (fun _ -> perform
                      uuid <-- Lazy.force OBus_info.machine_uuid;
                      line <-- catch
                        (fun _ -> Util.with_process_in "dbus-launch"
                           [|"dbus-launch"; "--autolaunch"; OBus_uuid.to_string uuid; "--binary-syntax"|]
                           Lwt_chan.input_line)
                        (fun exn ->
                           Log.log "autolaunch failed: %s" (Util.string_of_exn exn);
                           fail exn);
                      let line =
                        try
                          String.sub line 0 (String.index line '\000')
                        with _ -> line
                      in
                      return (OBus_address.of_string line))
                   (fun exn -> return []);
                 aux (addresses @ rest))

          | _ -> aux rest
  in
  aux addresses
