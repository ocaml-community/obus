(*
 * bus.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let obus_protocol_version = 1

type cookie = int32

type t = {
  buffer : string;
  (* Sending/Reception buffer *)
  fd : Unix.file_descr;
  (* Transport *)
  mutable waiters : (cookie * Thr.id) list;
  (* List of thread waiting for a reply *)
  lock : Thr.lock;
}

let create fd =
  { buffer = String.create 65536;
    buffer_lock = Thr.create_lock ();
    fd = fd }

exception Connection_failure of string
exception Communication_error
exception Data_error = Marshaler.Read_error

let connect = function
  | [] -> raise Connection_failure "no working transport found"
  | address :: addresses ->
      try
        match address with
          | Unix addr ->
              let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
                Unix.connect fd (Unix.ADDR_UNIX(addr));
                match Auth.do_auth fd with
                  | Auth.Success -> create fd;
                  | Auth.Failure _ -> raise Exit
      with
        | _ -> connect addresses

module H = Header

module ReadHeader(R : Marshaler.BasicWriter) =
struct
  let match_signature signature i =
    let len = int_of_char R.buffer.[i] in
    let str = String.sub R.buffer i (i + len) in
      if str <> signature then
        raise (Data_error (Printf.sprintf "invalid signature: %s expected, got %s" signature str))

  let read bus byte_order =
    (* header fields array length *)
    let protocol_version = int_of_char R.buffer.[3] in
      if protocol_version <> obus_protocol_version then
        raise (Data_error (Printf.sprinf
                             "invalid protocol version : %d (obus support only %d)"
                             protocol_version
                             obus_protocol_version));

      let message_type = match R.buffer.[1] with
        | '\x00' -> Invalid
        | '\x01' -> Method_call
        | '\x02' -> Method_return
        | '\x03' -> Error
        | '\x04' -> Signal
        | _ -> raise (Data_error "unknown message type")
      and flags =
        let flags = int_of_char R.buffer.[2] in
        let l = if flags land 1 = 1 then [No_reply_expected] else [] in
          if flags land 2 = 2 then No_auto_start :: l else l
      and length = R.int_uint32 4
      and serial = R.int32_uint32 8
      and fields_length = R.int_uint32 12 in

        if fields_length > Constant.max_array_size then
          raise (Data_error "array too big");

        (* Header fields array start on byte #16 and message start aligned
           on a 8-boundary after it, so we have: *)
        let total_length = (R.pad8 fields_length) + length in

          if total_length > max_message_size then
            raise (Data_error "message too big");

          if total_length > String.length bus.buffer then
            bus.buffer = String.create total_length;

          let count = Unix.read bus.fd bus.buffer 0 total_length in
            if count <> total_length then raise Communication_error;

            let i, fields = Marshaler.read_until begin fun i acc ->
              let i = B.pad8 i in
                match buffer.[i] with
                  | '\x01' -> match_signature "o" i; let i, x = R.string_string (i + 4) in  (i, { acc with H.path = x })
                  | '\x02' -> match_signature "s" i; let i, x = R.string_string (i + 4) in (i, { acc with H.interface = x })
                  | '\x03' -> match_signature "s" i; let i, x = R.string_string (i + 4) in (i, { acc with H.member = x })
                  | '\x04' -> match_signature "s" i; let i, x = R.string_string (i + 4) in (i, { acc with H.error_name = x })
                  | '\x05' -> match_signature "u" i; let i, x = R.int32_uint32 (i + 4) in (i, { acc with H.reply_serial = x })
                  | '\x06' -> match_signature "s" i; let i, x = R.string_string (i + 4) in (i, { acc with H.destination = x })
                  | '\x07' -> match_signature "s" i; let i, x = R.string_string (i + 4) in (i, { acc with H.sender = x })
                  | '\x08' -> match_signature "g" i; let i, x = R.string_signature (i + 4) in (i, { acc with H.signature = x })
                  | code when code < '\x01' || code > '\x08' -> failwith "unknown header field code"
                  | _ -> raise (Data_error (Printf.sprintf "malformed header field(code = %d)" (int_of_char code)))
            end H.empty_fields fields_length 0 in
              (i, { byte_order = byte_order;
                    message_type = message_type;
                    flags = flags;
                    protocol_version = protocol_version;
                    length = length;
                    serial = serial;
                    fields = fields })
end

let read_header bus =
  (* Read the minimum for knowing the total size of the message *)
  let len = Unix.read bus.fd bus.buffer 0 16 in
    if len <> 16 then raise Communication_error;
    (* We immediatly look for the byte order, then read the header *)
    match bus.buffer.[0] with
      | 'l' -> let module R = ReadHeader(Marshaler.LEWriter(struct let buffer = bus.buffer end))
        in R.read_message bus H.LittleEndian
      | 'B' -> let module R = ReadHeader(Marshaler.BEWriter(struct let buffer = bus.buffer end))
        in R.read_message bus H.BigEndian

let wait_reply bus cookie =
  
