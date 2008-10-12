(*
 * oBus_lowlevel.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Unix
open Printf
open Lwt
open Lwt_chan
open OBus_value
open OBus_info
open OBus_message
open OBus_address

let (&) a b = a b

exception Protocol_error of string

let pad2 i = i land 1
let pad4 i = (4 - i) land 3
let pad8 i = (8 - i) land 7

let pad8_p = function
  | Tdict_entry _ -> true
  | Tsingle t -> match t with
      | Tstruct _
      | Tbasic Tint64
      | Tbasic Tuint64
      | Tbasic Tdouble -> true
      | _ -> false

(****** Raw description of header fields ******)

type raw_fields = {
  _path : OBus_path.t option;
  _member : OBus_name.Member.t option;
  _interface : OBus_name.Interface.t option;
  _error_name : OBus_name.Error.t option;
  _reply_serial : serial option;
  _destination : OBus_name.Connection.t option;
  _sender : OBus_name.Connection_unique.t option;
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
    `Method_call(req path fields,
                 fields._interface,
                 req member fields)

let method_return_of_raw fields =
  let req x = get_required "method_return" x in
    `Method_return(req reply_serial fields)

let error_of_raw fields =
  let req x = get_required "error" x in
    `Error(req reply_serial fields,
           req error_name fields)

let signal_of_raw fields =
  let req x = get_required "signal" x in
    `Signal(req path fields,
            req interface fields,
            req member fields)

(***** Writing of integers *****)

module type Int_writers = sig
  val output_int16 : out_channel -> int -> unit Lwt.t
  val output_int32 : out_channel -> int32 -> unit Lwt.t
  val output_int64 : out_channel -> int64 -> unit Lwt.t
  val output_uint16 : out_channel -> int -> unit Lwt.t
  val output_uint32 : out_channel -> int32 -> unit Lwt.t
  val output_uint64 : out_channel -> int64 -> unit Lwt.t

  val output_uint : out_channel -> int -> unit Lwt.t
    (* Output an int as an uint32 *)
end

let output_uint8 oc x = output_char oc (Char.unsafe_chr x)

module Little_endian_writers : Int_writers =
struct
  let output_int16 oc v =
    (perform
       output_char oc (Char.unsafe_chr v);
       output_char oc (Char.unsafe_chr (v lsr 8)))
  let output_uint16 = output_int16

  let output_int32 oc v =
    (perform
       output_char oc (Char.unsafe_chr (Int32.to_int v));
       output_char oc (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
       output_char oc (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
       output_char oc (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24))))
  let output_uint32 = output_int32

  let output_int64 oc v =
    (perform
       output_char oc (Char.unsafe_chr (Int64.to_int v));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56))))
  let output_uint64 = output_int64

  let output_uint oc v =
    (perform
       output_char oc (Char.unsafe_chr v);
       output_char oc (Char.unsafe_chr (v lsr 8));
       output_char oc (Char.unsafe_chr (v lsr 16));
       output_char oc (Char.unsafe_chr (v asr 24)))
end

module Big_endian_writers : Int_writers =
struct
  let output_int16 oc v =
    (perform
       output_char oc (Char.unsafe_chr (v lsr 8));
       output_char oc (Char.unsafe_chr v))
  let output_uint16 = output_int16

  let output_int32 oc v =
    (perform
       output_char oc (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)));
       output_char oc (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
       output_char oc (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
       output_char oc (Char.unsafe_chr (Int32.to_int v)))
  let output_uint32 = output_int32

  let output_int64 oc v =
    (perform
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
       output_char oc (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
       output_char oc (Char.unsafe_chr (Int64.to_int v)))
  let output_uint64 = output_int64

  let output_uint oc v =
    (perform
       output_char oc (Char.unsafe_chr (v asr 24));
       output_char oc (Char.unsafe_chr (v lsr 16));
       output_char oc (Char.unsafe_chr (v lsr 8));
       output_char oc (Char.unsafe_chr v))
end

(***** Reading of integers *****)

module type Int_readers = sig
  val input_int16 : in_channel -> int Lwt.t
  val input_int32 : in_channel -> int32 Lwt.t
  val input_int64 : in_channel -> int64 Lwt.t
  val input_uint16 : in_channel -> int Lwt.t
  val input_uint32 : in_channel -> int32 Lwt.t
  val input_uint64 : in_channel -> int64 Lwt.t

  val input_uint : in_channel -> int Lwt.t
    (* Input an uint32 as an int *)
end

let input_uint8 ic = input_char ic >>= (fun n -> return (Char.code n))

module Little_endian_readers : Int_readers =
struct
  let input_int16 ic =
    (perform
       v0 <-- input_char ic;
       v1 <-- input_char ic;
       let v = (Char.code v0) land (Char.code v1 lsl 8) in
       if v land (1 lsl 15) = 0 then
         return v
       else
         return ((-1 land (lnot 0x7fff)) lor v))

  let input_uint16 ic =
    (perform
       v0 <-- input_char ic;
       v1 <-- input_char ic;
       return (Char.code v0 land (Char.code v1 lsl 8)))

  let input_int32 ic =
    (perform
       v0 <-- input_char ic;
       v1 <-- input_char ic;
       v2 <-- input_char ic;
       v3 <-- input_char ic;
       return (Int32.logor
                 (Int32.logor
                    (Int32.of_int (Char.code v0))
                    (Int32.shift_left (Int32.of_int (Char.code v1)) 8))
                 (Int32.logor
                    (Int32.shift_left (Int32.of_int (Char.code v2)) 16)
                    (Int32.shift_left (Int32.of_int (Char.code v3)) 24))))
  let input_uint32 = input_int32

  let input_int64 ic =
    (perform
       v0 <-- input_char ic;
       v1 <-- input_char ic;
       v2 <-- input_char ic;
       v3 <-- input_char ic;
       v4 <-- input_char ic;
       v5 <-- input_char ic;
       v6 <-- input_char ic;
       v7 <-- input_char ic;
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
  let input_uint64 = input_int64

  let input_uint ic =
    (perform
       v0 <-- input_char ic;
       v1 <-- input_char ic;
       v2 <-- input_char ic;
       v3 <-- input_char ic;
       return (Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)))
end

module Big_endian_readers : Int_readers =
struct
  let input_int16 ic =
    (perform
       v1 <-- input_char ic;
       v0 <-- input_char ic;
       let v = Char.code v0 land (Char.code v1 lsl 8) in
       if v land (1 lsl 15) = 0 then
         return v
       else
         return ((-1 land (lnot 0x7fff)) lor v))

  let input_uint16 ic =
    (perform
       v1 <-- input_char ic;
       v0 <-- input_char ic;
       return (Char.code v0 land (Char.code v1 lsl 8)))

  let input_int32 ic =
    (perform
       v3 <-- input_char ic;
       v2 <-- input_char ic;
       v1 <-- input_char ic;
       v0 <-- input_char ic;
       return (Int32.logor
                 (Int32.logor
                    (Int32.of_int (Char.code v0))
                    (Int32.shift_left (Int32.of_int (Char.code v1)) 8))
                 (Int32.logor
                    (Int32.shift_left (Int32.of_int (Char.code v2)) 16)
                    (Int32.shift_left (Int32.of_int (Char.code v3)) 24))))
  let input_uint32 = input_int32

  let input_int64 ic =
    (perform
       v7 <-- input_char ic;
       v6 <-- input_char ic;
       v5 <-- input_char ic;
       v4 <-- input_char ic;
       v3 <-- input_char ic;
       v2 <-- input_char ic;
       v1 <-- input_char ic;
       v0 <-- input_char ic;
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
  let input_uint64 = input_int64

  let input_uint ic =
    (perform
       v3 <-- input_char ic;
       v2 <-- input_char ic;
       v1 <-- input_char ic;
       v0 <-- input_char ic;
       return (Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)))
end

(***** String validation (Unicode + no '\000') *****)

let failwith fmt = ksprintf (fun msg -> raise (Failure msg)) fmt

let validate_string s =
  let null_byte i = failwith "invalid string %S, at position %d: null byte" s i
  and malformed_code i = failwith "invalid string %S, at position %d: malformed utf8 code" s i in
  let rec trail c i a =
    if c = 0 then a else
      if i >= String.length s then malformed_code i else
        let n = Char.code (String.unsafe_get s i) in
        if n = 0 then null_byte i else
          if n < 0x80 || n >= 0xc0 then malformed_code i else
            trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main i =
    if i >= String.length s then () else
      let n = Char.code (String.unsafe_get s i) in
      if n = 0 then null_byte i else
        if n < 0x80 then main (i + 1) else
          if n < 0xc2 then malformed_code i else
            if n <= 0xdf then
              if trail 1 (i + 1) (n - 0xc0) < 0x80 then malformed_code i else
                main (i + 2)
            else if n <= 0xef then
              if trail 2 (i + 1) (n - 0xe0) < 0x800 then malformed_code i else
                main (i + 3)
            else if n <= 0xf7 then
              if trail 3 (i + 1) (n - 0xf0) < 0x10000 then malformed_code i else
                main (i + 4)
            else if n <= 0xfb then
              if trail 4 (i + 1) (n - 0xf8) < 0x200000 then malformed_code i else
                main (i + 5)
            else if n <= 0xfd then
              let n = trail 5 (i + 1) (n - 0xfc) in
              if n lsr 16 < 0x400 then malformed_code i else
                main (i + 6)
            else malformed_code i in
  main 0

(***** Message writing *****)

type 'output message_marshaler =
  | Marshaler_failure of string
  | Marshaler_success of ('output -> unit Lwt.t)

module Types_writer = Types_rw.Make_writer(OBus_value)
  (struct
     include Lwt
     type output = out_channel
     let put oc ch = output_char oc ch
   end)

let rec output_padding oc = function
  | 0 -> return ()
  | n -> (perform output_char oc '\000'; output_padding oc (n - 1))

let write size i f v =
  let padding = (size - i) land (size - 1) in
  (i + padding + size,
   (fun oc -> perform
      output_padding oc padding;
      f oc v))

module Make_writer(Int_writers : Int_writers) =
struct
  open Int_writers

  let wstring i str =
    let padding = pad4 i and len = String.length str in
    (i + padding + 4 + len + 1,
     fun oc -> perform
       output_padding oc padding;
       output_uint32 oc (Int32.of_int len);
       output_string oc str;
       output_char oc '\000')
  let wsignature i s =
    let len = Types_writer.signature_size s in
    (i + 1 + len + 1,
     fun oc -> perform
       output_uint8 oc len;
       Types_writer.write_sequence oc s;
       output_char oc '\000')
  let wobject_path i = function
    | [] ->
        let padding = pad4 i in
        (i + padding + 4 + 1 + 1,
         fun oc -> perform
           output_padding oc padding;
           output_uint32 oc 1l;
           output_char oc '/';
           output_char oc '\000')
    | path ->
        let padding = pad4 i
        and len = List.fold_left (fun acc elt ->
                                    OBus_path.validate_element elt;
                                    1 + acc + String.length elt) 0 path in
        (i + padding + 4 + len + 1,
         fun oc -> perform
           output_padding oc padding;
           output_uint32 oc (Int32.of_int len);
           Lwt_util.iter_serial (fun elt -> perform
                                   output_char oc '/';
                                   output_string oc elt) path;
           output_char oc '\000')

  let wbasic i = function
    | Byte x -> (i + 1, fun oc -> output_char oc x)
    | Boolean x -> write 4 i output_uint (match x with true -> 1 | false -> 0)
    | Int16 x -> write 2 i output_int16 x
    | Int32 x -> write 4 i output_int32 x
    | Int64 x -> write 8 i output_int64 x
    | Uint16 x -> write 2 i output_uint16 x
    | Uint32 x -> write 4 i output_uint32 x
    | Uint64 x -> write 8 i output_uint64 x
    | Double x -> write 8 i output_uint64 (Int64.of_float x)
    | String x -> validate_string x; wstring i x
    | Signature x -> wsignature i x
    | Object_path x -> wobject_path i x

  let rec wsingle i = function
    | Basic x -> wbasic i x
    | Array(t, x) ->
        let padding = pad4 i in
        let i = i + padding + 4 in
        let initial_padding = if pad8_p t then pad8 i else 0 in
        let i = i + initial_padding in
        let j, output_array = List.fold_left (fun (i, f) x ->
                                                let i, g = welement i x in
                                                (i, fun oc -> perform f oc; g oc))
          (i, fun oc -> return ()) x in
        let len = j - i in
        if len < 0 || len > max_array_size then
          failwith "array size exceed the limit: %d" len
        else
          (j,
           fun oc -> perform
             output_padding oc padding;
             output_uint32 oc (Int32.of_int len);
             output_padding oc initial_padding;
             output_array oc)
    | Struct x -> wsequence i x
    | Variant x ->
        let t = OBus_value.type_of_single x in
        let len = Types_writer.single_signature_size t in
        let i = i + 1 + len + 1 in
        let i, output_variant = wsingle i x in
        (i,
         fun oc -> perform
           output_uint8 oc len;
           Types_writer.write_single oc t;
           output_char oc '\000';
           output_variant oc)

  and welement i = function
    | Dict_entry(k, v) ->
        let i, output_key = wbasic i k in
        let i, output_val = wsingle i v in
        (i, fun oc -> perform output_key oc; output_val oc)
    | Single x -> wsingle i x

  and wsequence i = function
    | [] -> (i, fun oc -> return ())
    | x :: l ->
        let i, output_head = wsingle i x in
        let i, output_tail = wsequence i l in
        (i, fun oc -> perform output_head oc; output_tail oc)

  let wmessage msg =
    let code, fields = match msg.typ with
      | `Method_call(path, interface, member) ->
          ('\001',
           { _path = Some path;
             _interface = interface;
             _member = Some member;
             _error_name = None;
             _reply_serial = None;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
      | `Method_return(reply_serial) ->
          ('\002',
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = None;
             _reply_serial = Some reply_serial;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
      | `Error(reply_serial, error_name) ->
          ('\003',
           { _path = None;
             _interface = None;
             _member = None;
             _error_name = Some error_name;
             _reply_serial = Some reply_serial;
             _destination = msg.destination;
             _sender = msg.sender;
             _signature = type_of_sequence msg.body })
      | `Signal(path, interface, member) ->
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
    let _wfield code typ f v (i, wacc) =
      let padding = pad8 i in
      let i = i + padding + 4 in
      let i, writer = f i v in
      (i,
       fun oc -> perform
         wacc oc;
         output_padding oc padding;
         output_uint8 oc code;
         output_uint8 oc 1;
         output_char oc (Types_writer.char_of_basic typ);
         output_char oc '\000';
         writer oc) in
    let wfield code typ f field acc = match field with
      | None -> acc
      | Some v -> _wfield code typ f v acc in
    let wfield2  code validator field acc = match field with
      | None -> acc
      | Some v -> validator v; _wfield code Tstring wstring v acc in

    let acc = 0, (fun oc -> return ()) in
    let acc = wfield 1 Tobject_path wobject_path fields._path acc in
    let acc = wfield2 2 OBus_name.Interface.validate fields._interface acc in
    let acc = wfield2 3 OBus_name.Member.validate fields._member acc in
    let acc = wfield2 4 OBus_name.Error.validate fields._error_name acc in
    let acc = wfield 5 Tuint32 (fun i x -> write 4 i output_uint32 x) fields._reply_serial acc in
    let acc = wfield2 6 OBus_name.Connection.validate fields._destination acc in
    let acc = wfield2 7 OBus_name.Connection_unique.validate fields._sender acc in
    let fields_length, fields_writer = _wfield 8 Tsignature wsignature fields._signature acc in

    if fields_length > max_array_size then
      failwith "array size exceed the limit: %d" fields_length;

    let length, message_writer = wsequence 0 msg.body in
    if length > max_message_size then
      failwith "message size exceed the limit: %d" length;

    (fun oc -> perform
       output_char oc code;
       output_uint8 oc
         ((if msg.flags.no_reply_expected then 1 else 0) lor
            (if msg.flags.no_auto_start then 2 else 0));
       output_uint8 oc protocol_version;
       output_uint oc length;
       output_uint32 oc msg.serial;
       output_uint oc fields_length;
       fields_writer oc;
       output_padding oc (pad8 fields_length);
       message_writer oc;
       flush oc)
end

module LEWriter = Make_writer(Little_endian_writers)
module BEWriter = Make_writer(Big_endian_writers)

let put_message ?(byte_order=native_byte_order) msg =
  try
    let bo_char, writer = match byte_order with
      | Little_endian ->
          'l', LEWriter.wmessage msg
      | Big_endian ->
          'B', BEWriter.wmessage msg
    in
    Marshaler_success(fun oc -> perform output_char oc bo_char; writer oc)
  with
    | Failure msg ->
        Marshaler_failure msg
    | OBus_path.Invalid_element(elt, msg) ->
        Marshaler_failure(sprintf "invalid path element(%S): %s" elt msg)
    | exn ->
        assert false

(***** Message reading *****)

let failwith fmt = ksprintf (fun msg -> fail (Protocol_error msg)) fmt

module Types_reader = Types_rw.Make_reader(OBus_value)
  (struct
     include Lwt
     let failwith fmt = failwith ("invalid signature " ^^ fmt)
     type input = in_channel * int ref
     let get (ic, remaining) = match !remaining with
       | 0 -> failwith "unterminated signature"
       | n -> remaining := n - 1; input_char ic
     let get_opt (ic, remaining) = match !remaining with
       | 0 -> return None
       | n ->
           remaining := n - 1;
           input_char ic >>= fun ch -> return (Some ch)
   end)

let rec input_padding ic = function
  | 0 -> return ()
  | n -> input_char ic >>= function
      | '\000' -> input_padding ic (n - 1)
      | _ -> failwith "uninitialized padding"

let out_of_bounds () = failwith "out of bounds"

let read size f g = (); fun ic i limit ->
  let padding = (size - i) land (size - 1) in
  let i = i + padding + size in
  if i > limit then
    out_of_bounds ()
  else
    (perform
       input_padding ic padding;
       v <-- f ic;
       return (i, g v))

module Make_reader(Int_readers : Int_readers) =
struct
  open Int_readers

  let ruint8 ic i size =
    if i + 1 > size then
      out_of_bounds ()
    else
      (perform
         x <-- input_char ic;
         return (i + 1, Char.code x))
  let ruint ic i size =
    let padding = pad4 i in
    let i = i + padding + 4 in
    if i > size then
      out_of_bounds ()
    else
      (perform
         input_padding ic padding;
         x <-- input_uint ic;
         return (i, x))
  let ruint32 ic i size =
    let padding = pad4 i in
    let i = i + padding + 4 in
    if i > size then
      out_of_bounds ()
    else
      (perform
         input_padding ic padding;
         x <-- input_uint32 ic;
         return (i, x))
  let rstring ic i size =
    (perform
       (i, len) <-- ruint ic i size;
       let i = i + len + 1 in
       if len < 0 || i > size then
         out_of_bounds ()
       else let str = String.create len in perform
         really_input ic str 0 len;
         input_char ic >>= function
           | '\000' -> return (i, str)
           | _ -> failwith "string terminal null byte missing")
  let rsignature ic i size =
    (perform
       (i, len) <-- ruint8 ic i size;
       let i = i + len + 1 in
       if i > size then
         out_of_bounds ()
       else perform
         s <-- Types_reader.read_sequence (ic, ref len);
         input_char ic >>= function
           | '\000' -> return (i, s)
           | _ -> failwith "signature terminal null byte missing")
  let rtype ic i size =
    (perform
       (i, s) <-- rsignature ic i size;
       match s with
         | [t] -> return (i, t)
         | [] -> failwith "empty variant signature"
         | _ -> failwith "variant signature contain more than one single type")
  let robject_path ic i size =
    rstring ic i size >>= fun (i, str) ->
      try
        return (i, OBus_path.of_string str)
      with
          exn -> fail exn

  let rbasic  = function
    | Tbyte ->
        (fun ic i size ->
           if i + 1 > size then
             out_of_bounds ()
           else perform
             x <-- input_char ic;
             return (i + 1, Byte x))
    | Tboolean ->
        (fun ic i size ->
           perform
             (i, x) <-- ruint ic i size;
             match x with
               | 0 -> return (i, Boolean false)
               | 1 -> return (i, Boolean true)
               | n -> failwith "invalid boolean value: %d" n)
    | Tint16 -> read 2 input_int16 vint16
    | Tint32 -> read 4 input_int32 vint32
    | Tint64 -> read 8 input_int64 vint64
    | Tuint16 -> read 2 input_uint16 vuint16
    | Tuint32 -> read 4 input_uint32 vuint32
    | Tuint64 -> read 8 input_uint64 vuint64
    | Tdouble -> read 8 input_uint64 (fun x -> Double(Int64.to_float x))
    | Tstring ->
        (fun ic i size ->
           rstring ic i size >>= fun (i, str) ->
             try
               validate_string str;
               return (i, String str)
             with
                 exn -> fail exn)
    | Tsignature ->
        (fun ic i size -> perform
           (i, s) <-- rsignature ic i size;
           return (i, Signature s))
    | Tobject_path ->
        (fun ic i size ->
           rstring ic i size >>= fun (i, str) ->
             try
               return (i, Object_path (OBus_path.of_string str))
             with
                 exn -> fail exn)

  let rec rsingle = function
    | Tbasic t ->
        let reader = rbasic t in
        (fun ic i size -> reader ic i size >>= fun (i, x) -> return (i, vbasic x))
    | Tarray t ->
        let reader = relement t in
        (fun ic i size -> perform
           (i, len) <-- ruint ic i size;
           let padding = if pad8_p t then pad8 i else 0 in
           let i = i + padding in
           let limit = i + len in
           if len < 0 || len > max_array_size then
             failwith "array size exceed the limit: %d" len
           else
             let rec aux i =
               if i < limit then
                 (perform
                    (i, x) <-- reader ic i size;
                    l <-- aux i;
                    return (x :: l))
               else if i > limit then
                 failwith "invalid array size"
               else
                 return []
             in perform
               input_padding ic padding;
               l <-- aux i;
               return (limit, varray t l))
    | Tstruct tl ->
        let reader = rsequence tl in
        (fun ic i size -> perform
           (i, l) <-- reader ic i size;
           return (i, vstruct l))
    | Tvariant ->
        (fun ic i size -> perform
           (i, t) <-- rtype ic i size;
           (i, v) <-- rsingle t ic i size;
           return (i, vvariant v))

  and relement = function
    | Tdict_entry(tk, tv) ->
        let kreader = rbasic tk
        and vreader = rsingle tv in
        (fun ic i size ->
           let padding = pad8 i in
           let i = i + padding in
           if i > size then
             out_of_bounds ()
           else perform
             input_padding ic padding;
             (i, k) <-- kreader ic i size;
             (i, v) <-- vreader ic i size;
             return (i, Dict_entry(k, v)))
    | Tsingle t ->
        let reader = rsingle t in
        (fun ic i size -> perform
           (i, x) <-- reader ic i size;
           return (i, Single x))

  and rsequence = function
    | [] -> (fun ic i size -> return (i, []))
    | t :: tl ->
        let head_reader = rsingle t
        and tail_reader = rsequence tl in
        (fun ic i size -> perform
           (i, x) <-- head_reader ic i size;
           (i, l) <-- tail_reader ic i size;
           return (i, x :: l))

  let rfields ic limit =
    let rfield code typ reader i =
      (perform
         (i, t) <-- rtype ic i limit;
         match t with
           | Tbasic t' when t' = typ ->
               reader ic i limit;
           | _ -> failwith "invalid header field signature for code %d: %S, should be %S"
               code (string_of_signature [t]) (string_of_signature [Tbasic typ]))
    in

    let rfield2 code validator i =
      (rfield code Tstring rstring i >>= fun (i, v) ->
         try
           validator v;
           return (i, v)
         with
             exn -> fail exn) in

    let rec aux (i, acc) =
      if i = limit then
        return acc
      else
        let padding = pad8 i in
        let i = i + padding + 1 in
        (perform
           input_padding ic padding;
           input_uint8 ic >>=
             (function
                | 1 -> rfield 1 Tobject_path robject_path i >>= (fun (i, x) -> aux (i, { acc with _path = Some x }))
                | 2 -> rfield2 2 OBus_name.Interface.validate i >>= (fun (i, x) -> aux (i, { acc with _interface = Some x }))
                | 3 -> rfield2 3 OBus_name.Member.validate i >>= (fun (i, x) -> aux (i, { acc with _member = Some x }))
                | 4 -> rfield2 4 OBus_name.Error.validate i >>= (fun (i, x) -> aux (i, { acc with _error_name = Some x }))
                | 5 -> rfield 5 Tuint32 ruint32 i >>= (fun (i, x) -> aux (i, { acc with _reply_serial = Some x }))
                | 6 -> rfield2 6 OBus_name.Connection.validate i >>= (fun (i, x) -> aux (i, { acc with _destination = Some x }))
                | 7 -> rfield2 7 OBus_name.Connection.validate i >>= (fun (i, x) -> aux (i, { acc with _sender = Some x }))
                | 8 -> rfield 8 Tsignature rsignature i >>= (fun (i, x) -> aux (i, { acc with _signature = x }))
                | n ->
                    (perform
                       (i, t) <-- rtype ic i limit;
                       (i, _) <-- rsingle t ic i limit;
                       aux (i, acc))))
    in
    catch
      (fun _ -> aux (0, empty_fields))
      (function
         | OBus_name.Invalid_name(typ, name, msg) ->
             failwith "invalid %s name %S: %s" typ name msg
         | OBus_path.Invalid_path(path, msg) ->
             failwith "invalid path %S: %s" path msg
         | exn -> fail exn)

  let rmessage ic =
    (perform
       message_maker <-- input_char ic >>=
         (function
            | '\001' -> return method_call_of_raw
            | '\002' -> return method_return_of_raw
            | '\003' -> return error_of_raw
            | '\004' -> return signal_of_raw
            | c -> failwith "unknown message type: %d" (Char.code c));

       n <-- input_uint8 ic;
       let flags = { no_reply_expected = n land 1 = 1;
                     no_auto_start = n land 2 = 2 } in

       protocol_version <-- input_uint8 ic;

       (* Check the protocol version first, since we can not do
          anything if it is not the same as our *)
       if protocol_version <> protocol_version then
         failwith "invalid protocol version: %d" protocol_version
       else perform
         length <-- input_uint ic;
         serial <-- input_uint32 ic;
         fields_length <-- input_uint ic;

         (* Header fields array start on byte #16 and message start
            aligned on a 8-boundary after it, so we have: *)
         let total_length = 16 + fields_length + (pad8 fields_length) + length in

         (* Safety checking *)
         if fields_length < 0 || fields_length > max_array_size then
           failwith "array size exceed the limit: %d" fields_length
         else if length < 0 || total_length > max_message_size then
           failwith "message size exceed the limit: %d" total_length
         else perform
           fields <-- rfields ic fields_length;
           input_padding ic (pad8 fields_length);
           (i, body) <-- rsequence fields._signature ic 0 length;
           if i = length then
             try
               return { flags = flags;
                        sender = fields._sender;
                        destination = fields._destination;
                        serial = serial;
                        typ = message_maker fields;
                        body = body }
             with
                 exn -> fail exn
           else
             failwith "junk after message")
end

module LEReader = Make_reader(Little_endian_readers)
module BEReader = Make_reader(Big_endian_readers)

let get_message ic =
  input_char ic >>= function
    | 'l' -> LEReader.rmessage ic
    | 'B' -> BEReader.rmessage ic
    | ch -> failwith "invalid byte order(%C)" ch

(***** Transport *****)

let failwith fmt = ksprintf (fun msg -> fail (Failure msg)) fmt

class type transport = object
  method get_message : OBus_message.any Lwt.t
  method put_message : 'a. 'a OBus_message.t -> unit message_marshaler
  method shutdown : unit
  method abort : exn -> unit
  method authenticate : OBus_address.guid option Lwt.t Lazy.t
end

class socket_transport fd =
  let ic = Lwt_chan.in_channel_of_descr fd
  and oc = Lwt_chan.out_channel_of_descr fd in
object
  method get_message = get_message ic
  method put_message : 'a. 'a OBus_message.t -> unit message_marshaler =
    fun msg -> match put_message msg with
      | Marshaler_success f -> Marshaler_success(fun () -> f oc)
      | Marshaler_failure msg -> Marshaler_failure msg
  method shutdown =
    Lwt_unix.shutdown fd SHUTDOWN_ALL;
    Lwt_unix.close fd
  method abort exn =
    Lwt_unix.abort fd exn
  method authenticate = lazy(OBus_auth.authenticate (ic, oc))
end

let loopback = object(self)
  val mutable queue = Queue.create ()
  val mutable waiters = Queue.create ()
  val mutable aborted = None

  method get_message = match aborted with
    | Some exn -> fail exn
    | None -> match Queue.is_empty queue with
        | true ->
            let w = Lwt.wait () in
            Queue.push w waiters;
            w
        | false ->
            return (Queue.pop queue)

  method put_message : 'a. 'a OBus_message.t -> unit message_marshaler =
    fun msg ->
      (* TODO: figure out why the typer refuse the following
         cohercion (without Obj.magic): *)
      let msg = (Obj.magic (msg : [< OBus_message.any_type ] OBus_message.t) : OBus_message.any) in
      Marshaler_success(fun () -> match aborted with
                          | Some exn -> fail exn
                          | None ->
                              begin match Queue.is_empty waiters with
                                | true -> Queue.push msg queue
                                | false -> Lwt.wakeup (Queue.pop waiters) msg
                              end;
                              return ())

  method shutdown = self#abort (Failure "transport closed")

  method abort exn =
    aborted <- Some exn;
    Queue.iter (fun w -> Lwt.wakeup_exn w exn) waiters;
    Queue.clear waiters;
    Queue.clear queue

  method authenticate = lazy(return (Some OBus_uuid.loopback))
end

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  try_bind
    (fun _ -> Lwt_unix.connect fd addr)
    (fun _ -> return (new socket_transport fd))
    (fun exn -> Lwt_unix.close fd; fail exn)

let rec try_one t fallback x =
  catch (fun _ -> t)
    (fun exn -> fallback x)

let rec transport_of_addresses = function
  | [] -> failwith "no working DBus address found"
  | (desc, _) :: rest -> match desc with
      | Unix_path path ->
          try_one (make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX(path)))
            transport_of_addresses rest

      | Unix_abstract path ->
          try_one (make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ path)))
            transport_of_addresses rest

      | Unix_tmpdir _ ->
          ERROR("unix tmpdir can only be used as a listening address");
          transport_of_addresses rest

      | Tcp(host, service, family) ->
          let opts = [AI_SOCKTYPE SOCK_STREAM] in
          let opts = match family with
            | Some Ipv4 -> AI_FAMILY PF_INET :: opts
            | Some Ipv6 -> AI_FAMILY PF_INET6 :: opts
            | None -> opts in
          let rec try_all = function
            | [] -> transport_of_addresses rest
            | ai :: ais ->
                try_one (make_socket ai.ai_family ai.ai_socktype ai.ai_addr)
                  try_all ais
          in
          try_all (getaddrinfo host service opts)

      | Autolaunch ->
          transport_of_addresses
            ((try
                let line = Util.with_process_in (sprintf "dbus-launch --autolaunch %s --binary-syntax"
                                                   (Lazy.force OBus_info.machine_uuid)) Pervasives.input_line in
                let line =
                  try
                    String.sub line 0 (String.index line '\000')
                  with _ -> line
                in
                OBus_address.of_string line
              with
                  exn ->
                    LOG("autolaunch failed: %s" (Printexc.to_string exn));
                    []) @ rest)

      | _ -> transport_of_addresses rest

(***** Listener *****)

class type listener = object
  method accept : transport Lwt.t
  method shutdown : unit
end

class socket_listener listen_fd = object
  method accept =
    (perform
       (fd, addr) <-- Lwt_unix.accept listen_fd;
       return (new socket_transport fd))
  method shutdown =
    Lwt_unix.shutdown listen_fd SHUTDOWN_ALL;
    Lwt_unix.close listen_fd
end
