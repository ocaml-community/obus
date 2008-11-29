(*
 * oBus_lowlevel.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus_value
open OBus_info
open OBus_message

exception Data_error of string
exception Protocol_error of string

module type Wire = sig
  type 'a monad
  type input = {
    get_char : unit -> char monad;
    get_string : int -> string monad;
  }
  type output = {
    put_char : char -> unit monad;
    put_string : string -> unit monad;
  }
  val get_message : input -> OBus_message.any monad
  val put_message : ?byte_order:OBus_info.byte_order -> OBus_message.any -> int * (output -> unit monad)
end

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

(* Common error message *)
let array_too_big len = sprintf "array size exceed the limit: %d" len
let message_too_big len = sprintf "message size exceed the limit: %d" len
let signature_too_long s len = sprintf "too long signature: %S, with len %d" (string_of_signature s) len
let variant_signature_too_long s len = sprintf "too long variant signature: %S, with len %d" (string_of_signature [s]) len
let invalid_protocol_version ver = sprintf "invalid protocol version: %d (obus implement protocol version %d)" ver OBus_info.protocol_version
let invalid_byte_order ch = sprintf "invalid byte order(%C)" ch

(****** Raw description of header fields ******)

type raw_fields = {
  _path : OBus_path.t option;
  _member : OBus_name.member option;
  _interface : OBus_name.interface option;
  _error_name : OBus_name.error option;
  _reply_serial : serial option;
  _destination : OBus_name.connection option;
  _sender : OBus_name.unique option;
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

module Make_wire(Monad : OBus_monad.S) : Wire with type 'a monad = 'a Monad.t =
struct
  open Monad

  type 'a monad = 'a Monad.t
  let ( >>= ) = bind

  module T = Types_rw.Make(OBus_value)(Monad)

  type input = {
    get_char : unit -> char monad;
    get_string : int -> string monad;
  }

  type output = {
    put_char : char -> unit monad;
    put_string : string -> unit monad;
  }

  (***** Writing of integers *****)

  module type Int_writers = sig
    val output_int16 : output -> int -> unit monad
    val output_int32 : output -> int32 -> unit monad
    val output_int64 : output -> int64 -> unit monad
    val output_uint16 : output -> int -> unit monad
    val output_uint32 : output -> int32 -> unit monad
    val output_uint64 : output -> int64 -> unit monad

    val output_uint : output -> int -> unit monad
      (* Output an int as an uint32 *)
  end

  let output_uint8 oc x = oc.put_char (Char.unsafe_chr x)

  module LE_int_writers : Int_writers =
  struct
    let output_int16 oc v =
      (perform
         oc.put_char (Char.unsafe_chr v);
         oc.put_char (Char.unsafe_chr (v lsr 8)))
    let output_uint16 = output_int16

    let output_int32 oc v =
      (perform
         oc.put_char (Char.unsafe_chr (Int32.to_int v));
         oc.put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
         oc.put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
         oc.put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24))))
    let output_uint32 = output_int32

    let output_int64 oc v =
      (perform
         oc.put_char (Char.unsafe_chr (Int64.to_int v));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56))))
    let output_uint64 = output_int64

    let output_uint oc v =
      (perform
         oc.put_char (Char.unsafe_chr v);
         oc.put_char (Char.unsafe_chr (v lsr 8));
         oc.put_char (Char.unsafe_chr (v lsr 16));
         oc.put_char (Char.unsafe_chr (v asr 24)))
  end

  module BE_int_writers : Int_writers =
  struct
    let output_int16 oc v =
      (perform
         oc.put_char (Char.unsafe_chr (v lsr 8));
         oc.put_char (Char.unsafe_chr v))
    let output_uint16 = output_int16

    let output_int32 oc v =
      (perform
         oc.put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)));
         oc.put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
         oc.put_char (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
         oc.put_char (Char.unsafe_chr (Int32.to_int v)))
    let output_uint32 = output_int32

    let output_int64 oc v =
      (perform
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 56)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 48)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 40)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 32)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 24)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 16)));
         oc.put_char (Char.unsafe_chr (Int64.to_int (Int64.shift_right v 8)));
         oc.put_char (Char.unsafe_chr (Int64.to_int v)))
    let output_uint64 = output_int64

    let output_uint oc v =
      (perform
         oc.put_char (Char.unsafe_chr (v asr 24));
         oc.put_char (Char.unsafe_chr (v lsr 16));
         oc.put_char (Char.unsafe_chr (v lsr 8));
         oc.put_char (Char.unsafe_chr v))
  end

  (***** Reading of integers *****)

  module type Int_readers = sig
    val input_int16 : input -> int monad
    val input_int32 : input -> int32 monad
    val input_int64 : input -> int64 monad
    val input_uint16 : input -> int monad
    val input_uint32 : input -> int32 monad
    val input_uint64 : input -> int64 monad

    val input_uint : input -> int monad
      (* Input an uint32 as an int *)
  end

  let input_uint8 ic = ic.get_char () >>= (fun n -> return (Char.code n))

  module LE_int_readers : Int_readers =
  struct
    let input_int16 ic =
      (perform
         v0 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         let v = (Char.code v0) land (Char.code v1 lsl 8) in
         if v land (1 lsl 15) = 0 then
           return v
         else
           return ((-1 land (lnot 0x7fff)) lor v))

    let input_uint16 ic =
      (perform
         v0 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         return (Char.code v0 land (Char.code v1 lsl 8)))

    let input_int32 ic =
      (perform
         v0 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         v2 <-- ic.get_char ();
         v3 <-- ic.get_char ();
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
         v0 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         v2 <-- ic.get_char ();
         v3 <-- ic.get_char ();
         v4 <-- ic.get_char ();
         v5 <-- ic.get_char ();
         v6 <-- ic.get_char ();
         v7 <-- ic.get_char ();
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
         v0 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         v2 <-- ic.get_char ();
         v3 <-- ic.get_char ();
         return (Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)))
  end

  module BE_int_readers : Int_readers =
  struct
    let input_int16 ic =
      (perform
         v1 <-- ic.get_char ();
         v0 <-- ic.get_char ();
         let v = Char.code v0 land (Char.code v1 lsl 8) in
         if v land (1 lsl 15) = 0 then
           return v
         else
           return ((-1 land (lnot 0x7fff)) lor v))

    let input_uint16 ic =
      (perform
         v1 <-- ic.get_char ();
         v0 <-- ic.get_char ();
         return (Char.code v0 land (Char.code v1 lsl 8)))

    let input_int32 ic =
      (perform
         v3 <-- ic.get_char ();
         v2 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         v0 <-- ic.get_char ();
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
         v7 <-- ic.get_char ();
         v6 <-- ic.get_char ();
         v5 <-- ic.get_char ();
         v4 <-- ic.get_char ();
         v3 <-- ic.get_char ();
         v2 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         v0 <-- ic.get_char ();
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
         v3 <-- ic.get_char ();
         v2 <-- ic.get_char ();
         v1 <-- ic.get_char ();
         v0 <-- ic.get_char ();
         return (Char.code v0 lor (Char.code v1 lsl 8) lor (Char.code v2 lsl 16) lor (Char.code v3 lsl 24)))
  end

  (***** Message writing *****)

  let failwith msg = raise (Data_error msg)

  type offset = int

  (* Each writing function act as follow:

     it takes:

     - the current offset, i.e. the size that will take serialized
     values already processed

     - the value to serialize

     then it compute the the length taken by the serialized value, the
     needed padding, add it to the current offset and return the
     result and a function which should be used to effectively
     serialize the value with maybe some padding before.

     So, basically, here is the type of a function which serialize
     values of type ['a]: *)

  type 'a serializer = offset -> 'a -> offset * (output -> unit monad)

  let rec output_padding oc = function
    | 0 -> return ()
    | n -> (perform oc.put_char '\000'; output_padding oc (n - 1))

  (* write an integer with the needed padding *)
  let write
      (size : int)
      (* size of the integer *)

      (f : output -> 'a -> unit monad)
      (* output function *)

      : 'a serializer =

    fun i v ->
      (* Compuitation of the needed padding: integers are always
         aligned to a block of their size *)
      let padding = (size - i) land (size - 1) in

      (* Offset after the output of the padding and the integer
         itself: *)
      (i + padding + size,

       (* Effective serialization function: *)
       (fun oc -> perform
          output_padding oc padding;
          f oc v))

  module Make_writer(Int_writers : Int_writers) =
  struct
    open Int_writers

    (* Serialize one string, without verifying it *)
    let wstring : string serializer = fun i str ->
      let padding = pad4 i and len = String.length str in
      (i + padding + 4 + len + 1,
       fun oc -> perform
         output_padding oc padding;
         output_uint32 oc (Int32.of_int len);
         oc.put_string str;
         oc.put_char '\000')

    (* Serialize a signature.

       TODO: verify deepth limit of the signature *)
    let wsignature : OBus_value.signature serializer = fun i s ->
      let len = T.sequence_size s in
      if len > 255 then
        failwith (signature_too_long s len)
      else
        (i + 1 + len + 1,
         fun oc -> perform
           output_uint8 oc len;
           T.write_sequence oc.put_char s;
           oc.put_char '\000')

    let wobject_path : OBus_path.t serializer = fun i -> function
      | [] ->
          let padding = pad4 i in
          (i + padding + 4 + 1 + 1,
           fun oc -> perform
             output_padding oc padding;
             output_uint32 oc 1l;
             oc.put_char '/';
             oc.put_char '\000')
      | path ->
          let padding = pad4 i
          and len = List.fold_left (fun acc elt ->
                                      match OBus_path.test_element elt with
                                        | Some error ->
                                            failwith (OBus_string.error_message error)
                                        | None ->
                                            1 + acc + String.length elt) 0 path in
          (i + padding + 4 + len + 1,
           fun oc -> perform
             output_padding oc padding;
             output_uint32 oc (Int32.of_int len);
             List.fold_left (fun m elt -> perform
                               m;
                               oc.put_char '/';
                               oc.put_string elt) (return ()) path;
             oc.put_char '\000')

    (* The following function serialize DBus values. Actually this
       need a lot a redundant matching. With GADT it would be:

       {[
         let wbasic : 'a type_basic -> 'a serializer = function
           | Tbyte -> (fun i v -> (i + 1, oc.put_char v))
           | Tint16 -> (fun i v -> write 2 output_int16 i v)
           ...

         let wsingle : 'a type_single -> 'a serializer = ...
         let wsequence : 'a type_sequence -> 'a serializer = ...
       ]}
    *)

    let wbasic : OBus_value.basic serializer = fun i ->function
      | Byte x -> (i + 1, fun oc -> oc.put_char x)
      | Boolean x -> write 4 output_uint i (match x with true -> 1 | false -> 0)
      | Int16 x -> write 2 output_int16 i x
      | Int32 x -> write 4 output_int32 i x
      | Int64 x -> write 8 output_int64 i x
      | Uint16 x -> write 2 output_uint16 i x
      | Uint32 x -> write 4 output_uint32 i x
      | Uint64 x -> write 8 output_uint64 i x
      | Double x -> write 8 output_uint64 i (Int64.bits_of_float x)
      | String x -> begin match OBus_string.test x with
          | Some error ->
              failwith (OBus_string.error_message error)
          | None ->
              wstring i x
        end
      | Signature x -> wsignature i x
      | Object_path x -> wobject_path i x

    let rec wsingle i = function
      | Basic x -> wbasic i x
      | Array(t, x) ->
          (* Array are serialized as follow:

             (1) padding to a 4-block alignement (for array size)
             (2) array size
             (3) alignement to array elements padding (even if the array is empty)
             (4) serialized elements

             The array size (2) is the size of serialized elements (4) *)

          let padding = pad4 i in
          let i = i + padding + 4 in
          (* After the size we are always padded on 4, so we only need
             to add padding if elements padding is 8 *)
          let initial_padding = if pad8_p t then pad8 i else 0 in
          let i = i + initial_padding in
          let j, output_array = List.fold_left (fun (i, f) x ->
                                                  let i, g = welement i x in
                                                  (i, fun oc ->
                                                     perform
                                                       f oc;
                                                       g oc))
            (i, fun oc -> return ()) x in
          let len = j - i in
          if len < 0 || len > max_array_size then
            failwith (array_too_big len)
          else
            (j,
             fun oc -> perform
               output_padding oc padding;
               output_uint32 oc (Int32.of_int len);
               output_padding oc initial_padding;
               output_array oc)
      | Struct x ->
          (* Structure are serialized as follow:

             (1) alignement to an 8-block
             (2) serialized contents *)
          let padding = pad8 i in
          let i, output_sequence = wsequence (i + padding) x in
          (i, fun oc -> perform
             output_padding oc padding;
             output_sequence oc)
      | Variant x ->
          (* Variant are serialized as follow:

             (1) marshaled variant signature
             (2) serialized contents *)
          let t = OBus_value.type_of_single x in
          let len = T.single_size t in
          if len > 255 then
            failwith (variant_signature_too_long t len)
          else
            let i = i + 1 + len + 1 in
            let i, output_variant = wsingle i x in
            (i,
             fun oc -> perform
               output_uint8 oc len;
               T.write_single oc.put_char t;
               oc.put_char '\000';
               output_variant oc)

    and welement i = function
      | Dict_entry(k, v) ->
          (* Dict-entry are serialized as follow:

             (1) alignement on a 8-block
             (2) serialized key
             (3) serialized value *)
          let padding = pad8 i in
          let i, output_key = wbasic (i + padding) k in
          let i, output_val = wsingle i v in
          (i, fun oc -> perform
             output_padding oc padding;
             output_key oc;
             output_val oc)
      | Single x -> wsingle i x

    and wsequence i = function
      | [] -> (i, fun oc -> return ())
      | x :: l ->
          let i, output_head = wsingle i x in
          let i, output_tail = wsequence i l in
          (i, fun oc ->
             perform
               output_head oc;
               output_tail oc)

    (* Serialize one complete message. The starting offset is always
       0. *)
    let wmessage msg =
      (* Compute ``raw'' headers *)
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

      (* Header field serializer *)
      let wfield_real code typ f v (i, wacc) =
        (* Each header field is a structure, so we need to be aligned
           on 8 *)
        let padding = pad8 i in
        let i = i + padding + 4 in
        let i, writer = f i v in
        (i,
         fun oc -> perform
           wacc oc;
           output_padding oc padding;
           output_uint8 oc code;
           output_uint8 oc 1;
           oc.put_char (T.char_of_basic typ);
           oc.put_char '\000';
           writer oc) in

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

      let acc = 0, (fun oc -> return ()) in
      let acc = wfield 1 Tobject_path wobject_path fields._path acc in
      let acc = wfield_test 2 OBus_name.test_interface fields._interface acc in
      let acc = wfield_test 3 OBus_name.test_member fields._member acc in
      let acc = wfield_test 4 OBus_name.test_error fields._error_name acc in
      let acc = wfield 5 Tuint32 (write 4 output_uint32) fields._reply_serial acc in
      let acc = wfield_test 6 OBus_name.test_connection fields._destination acc in
      let acc = wfield_test 7 OBus_name.test_unique fields._sender acc in
      let fields_length, fields_writer = wfield_real 8 Tsignature wsignature fields._signature acc in

      if fields_length > max_array_size then
        failwith (array_too_big fields_length);

      let body_length, body_writer = wsequence 0 msg.body in

      (* The message start aligned on an 8-boundary after the header,
         and header fields start at offset #16, so: *)
      let padding_before_message = pad8 fields_length in

      (* and the total message size is: *)
      let total_length = 16 + fields_length + padding_before_message + body_length in

      if total_length > max_message_size then
        failwith (message_too_big total_length);

      (total_length,
       fun oc -> perform
         (* byte #0 : byte-order, written by [put_message] *)
         (* byte #1 : message type code *)
         oc.put_char code;
         (* byte #2 : message flags *)
         output_uint8 oc
           ((if msg.flags.no_reply_expected then 1 else 0) lor
              (if msg.flags.no_auto_start then 2 else 0));
         (* byte #3 : protocol version *)
         output_uint8 oc protocol_version;
         (* byte #4-7 : body length *)
         output_uint oc body_length;
         (* byte #8-11 : serial *)
         output_uint32 oc msg.serial;
         (* byte #12-15 : fields length *)
         output_uint oc fields_length;
         (* header fields *)
         fields_writer oc;
         (* padding between header and body *)
         output_padding oc padding_before_message;
         (* message body *)
         body_writer oc)
  end

  module LEWriter = Make_writer(LE_int_writers)
  module BEWriter = Make_writer(BE_int_writers)

  let put_message ?(byte_order=native_byte_order) (msg : OBus_message.any) =
    let bo_char, (size, writer) = match byte_order with
      | Little_endian ->
          'l', LEWriter.wmessage msg
      | Big_endian ->
          'B', BEWriter.wmessage msg
    in
    (size,
     fun oc ->
       perform
         (* byte #0 : byte order *)
         oc.put_char bo_char;
         writer oc)

  (***** Message reading *****)

  let failwith msg = Monad.fail (Protocol_error msg)

  (* Each reading function act as follow:

     it takes an input, the current offset, the limit and return a
     monad which read a value and return the new offset with the
     value *)

  let rec input_padding ic = function
    | 0 -> return ()
    | n -> ic.get_char () >>= function
        | '\000' -> input_padding ic (n - 1)
        | _ -> failwith "uninitialized padding"

  let out_of_bounds () = failwith "out of bounds"

  (* Read an integer with the needed padding *)
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
           x <-- ic.get_char ();
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
         else
         perform
           str <-- ic.get_string len;
           ic.get_char () >>= function
             | '\000' -> return (i, str)
             | _ -> failwith "string terminal null byte missing")
    let rsignature ic i size =
      (perform
         (i, len) <-- ruint8 ic i size;
         let i = i + len + 1 in
         if i > size then
           out_of_bounds ()
         else let remaining = ref len in
         perform
           s <-- T.read_sequence (fun _ -> match !remaining with
                                    | 0 -> return None
                                    | n -> remaining := n - 1;
                                        ic.get_char () >>= fun ch -> return (Some ch));
           ic.get_char () >>= function
             | '\000' -> return (i, s)
             | _ -> failwith "signature terminal null byte missing")
    let rtype ic i size =
      (perform
         (i, s) <-- rsignature ic i size;
         match s with
           | [t] -> return (i, t)
           | [] -> failwith "empty variant signature"
           | _ -> failwith (sprintf
                          "variant signature contains more than one single type: %s"
                          (string_of_signature s)))
    let robject_path ic i size =
      rstring ic i size >>= fun (i, str) -> return (i, OBus_path.of_string str)

    let rbasic = function
      | Tbyte ->
          (fun ic i size ->
             if i + 1 > size then
               out_of_bounds ()
             else perform
               x <-- ic.get_char ();
               return (i + 1, Byte x))
      | Tboolean ->
          (fun ic i size ->
             perform
               (i, x) <-- ruint ic i size;
               match x with
                 | 0 -> return (i, Boolean false)
                 | 1 -> return (i, Boolean true)
                 | n -> failwith (sprintf "invalid boolean value: %d" n))
      | Tint16 -> read 2 input_int16 vint16
      | Tint32 -> read 4 input_int32 vint32
      | Tint64 -> read 8 input_int64 vint64
      | Tuint16 -> read 2 input_uint16 vuint16
      | Tuint32 -> read 4 input_uint32 vuint32
      | Tuint64 -> read 8 input_uint64 vuint64
      | Tdouble -> read 8 input_uint64 (fun x -> Double(Int64.float_of_bits x))
      | Tstring ->
          (fun ic i size ->
             rstring ic i size >>= fun (i, str) -> match OBus_string.test str with
               | Some error ->
                   failwith (OBus_string.error_message error)
               | None ->
                   return (i, String str));
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
                   OBus_string.Invalid_string error -> failwith (OBus_string.error_message error))

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
               failwith (array_too_big len)
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
          (fun ic i size ->
             let padding = pad8 i in
             let i = i + padding in
             if i > size then
               out_of_bounds ()
             else perform
               input_padding ic padding;
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
        rtype ic i limit >>= fun (i, t) -> match t with
          | Tbasic t' when t' = typ ->
              reader ic i limit;
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
          let padding = pad8 i in
          let i = i + padding + 1 in
          (perform
             input_padding ic padding;
             input_uint8 ic >>=
               (function
                  | 1 -> rfield 1 Tobject_path robject_path i >>= (fun (i, x) -> aux (i, { acc with _path = Some x }))
                  | 2 -> rfield_test 2 OBus_name.test_interface i >>= (fun (i, x) -> aux (i, { acc with _interface = Some x }))
                  | 3 -> rfield_test 3 OBus_name.test_member i >>= (fun (i, x) -> aux (i, { acc with _member = Some x }))
                  | 4 -> rfield_test 4 OBus_name.test_error i >>= (fun (i, x) -> aux (i, { acc with _error_name = Some x }))
                  | 5 -> rfield 5 Tuint32 ruint32 i >>= (fun (i, x) -> aux (i, { acc with _reply_serial = Some x }))
                  | 6 -> rfield_test 6 OBus_name.test_connection i >>= (fun (i, x) -> aux (i, { acc with _destination = Some x }))
                  | 7 -> rfield_test 7 OBus_name.test_unique i >>= (fun (i, x) -> aux (i, { acc with _sender = Some x }))
                  | 8 -> rfield 8 Tsignature rsignature i >>= (fun (i, x) -> aux (i, { acc with _signature = x }))
                  | n ->
                      (perform
                         (i, t) <-- rtype ic i limit;
                         (i, _) <-- rsingle t ic i limit;
                         aux (i, acc))))
      in
      aux (0, empty_fields)

    let rmessage ic =
      (perform
         message_maker <-- ic.get_char () >>=
           (function
              | '\001' -> return method_call_of_raw
              | '\002' -> return method_return_of_raw
              | '\003' -> return error_of_raw
              | '\004' -> return signal_of_raw
              | c -> failwith (sprintf "unknown message type: %d" (Char.code c)));

         n <-- input_uint8 ic;
         let flags = { no_reply_expected = n land 1 = 1;
                       no_auto_start = n land 2 = 2 } in

         protocol_version <-- input_uint8 ic;

         (* Check the protocol version first, since we can not do
            anything if it is not the same as our *)
         if protocol_version <> protocol_version then
           failwith (invalid_protocol_version protocol_version)
         else perform
           body_length <-- input_uint ic;
           serial <-- input_uint32 ic;
           fields_length <-- input_uint ic;

           (* Header fields array start on byte #16 and message start
              aligned on a 8-boundary after it, so we have: *)
           let padding_before_message = pad8 fields_length in
           let total_length = 16 + fields_length + padding_before_message + body_length in

           (* Safety checking *)
           if fields_length < 0 || fields_length > max_array_size then
             failwith (array_too_big fields_length)
           else if body_length < 0 || total_length > max_message_size then
             failwith (message_too_big total_length)
           else perform
             fields <-- rfields ic fields_length;
             input_padding ic padding_before_message;
             (i, body) <-- rsequence fields._signature ic 0 body_length;
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
               failwith "junk after message")
  end

  module LEReader = Make_reader(LE_int_readers)
  module BEReader = Make_reader(BE_int_readers)

  let get_message ic : OBus_message.any monad =
    ic.get_char () >>= function
      | 'l' -> LEReader.rmessage ic
      | 'B' -> BEReader.rmessage ic
      | ch -> failwith (invalid_byte_order ch)
end

module Lwt_wire = Make_wire(Lwt)

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

(***** Transport *****)

open Unix
open OBus_address
open Lwt

module Log = Log.Make(struct let section = "transport" end)

type transport = {
  recv : unit -> OBus_message.any Lwt.t;
  send : OBus_message.any -> unit Lwt.t;
  shutdown : unit -> unit;
}

let make_transport ~recv ~send ~shutdown = { recv = recv; send = send; shutdown = shutdown }

let recv { recv = recv } = recv ()
let send { send = send } message = send message
let shutdown { shutdown = shutdown } = shutdown ()

let chans_of_fd fd = (Lwt_chan.in_channel_of_descr fd,
                       Lwt_chan.out_channel_of_descr fd)

let lwt_input_of_channel ic = { Lwt_wire.get_char = (fun _ -> Lwt_chan.input_char ic);
                                Lwt_wire.get_string = (fun len ->
                                                         let str = String.create len in
                                                         perform
                                                           Lwt_chan.really_input ic str 0 len;
                                                           return str) }
let lwt_output_of_channel oc = { Lwt_wire.put_char = (fun ch -> Lwt_chan.output_char oc ch);
                                 Lwt_wire.put_string = (fun str -> Lwt_chan.output_string oc str) }

let socket fd (ic, oc) =
  { recv = (fun _ -> Lwt_wire.get_message (lwt_input_of_channel ic));
    send = (fun msg ->
              perform
                snd (Lwt_wire.put_message msg) (lwt_output_of_channel oc);
                Lwt_chan.flush oc);
    shutdown = (fun _ ->
                  Lwt_unix.shutdown fd SHUTDOWN_ALL;
                  Lwt_unix.close fd) }

let loopback _ =
  let queue = MQueue.create () in
  { recv = (fun _ -> MQueue.get queue);
    send = (fun m -> MQueue.put m queue; return ());
    shutdown = (fun _ ->
                  Queue.iter (fun w -> wakeup_exn w (Failure "transport closed")) queue.MQueue.waiters;
                  Queue.clear queue.MQueue.waiters;
                  Queue.clear queue.MQueue.queued) }

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
         guid <-- OBus_auth.client_authenticate ?mechanisms chans;
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
