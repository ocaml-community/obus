(*
 * message.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type path = string
type destination = string
type member = string
type body = Values.values
type flags =
  | No_reply_expected
  | No_auto_start

open Header

let hflags_of_flags l =
  { no_reply_expected = List.mem No_reply_expected l;
    no_auto_start = List.mem No_auto_start l }

let flags_of_hflags f =
  let aux hf f l = match hf with
    | true -> f :: l
    | false -> l
  in
    aux f.no_reply_expected No_reply_expected
      (aux f.no_auto_start No_auto_start [])

let method_call flags destination path interface member body =
  ({
     byte_order = Little_endian;
     message_type = Method_call;
     flags = hflags_of_flags flags;
     serial = ();
     length = ();
     fields = { empty_fields with
                  destination = Some(destination);
                  path = Some(path);
                  interface = Some(interface);
                  member = Some(member);
                  signature = Some(Values.signature_of_dtypes (Values.dtype_of_values body)) }
   },
   body)

let method_reply header body =
  ({
     byte_order = Little_endian;
     message_type = Method_return;
     flags = { default_flags with no_reply_expected = true };
     serial = ();
     length = ();
     fields = { empty_fields with
                  reply_serial = Some(header.serial);
                  destination = header.fields.sender;
                  path = header.fields.path;
                  interface = header.fields.interface;
                  member = header.fields.member;
                  signature = Some(Values.signature_of_dtypes (Values.dtype_of_values body)) }
   },
   body)

let error header error_name error_message =
  let body = match error_message with
    | None -> []
    | Some(msg) -> [Values.make_value Values.string msg]
  in
    ({
       byte_order = Little_endian;
       message_type = Error;
       flags = { default_flags with no_reply_expected = true };
       serial = ();
       length = ();
       fields = { empty_fields with
                    reply_serial = Some(header.serial);
                    destination = header.fields.sender;
                    path = header.fields.path;
                    interface = header.fields.interface;
                    member = header.fields.member;
                    error_name = Some(error_name);
                    signature = Some(Values.signature_of_dtypes (Values.dtype_of_values body)) }
     },
     body)

let signal path interface member body =
  ({
     byte_order = Little_endian;
     message_type = Signal;
     flags = { default_flags with no_reply_expected = true };
     serial = ();
     length = ();
     fields = { empty_fields with
                  path = Some(path);
                  interface = Some(interface);
                  member = Some(member);
                  signature = Some(Values.signature_of_dtypes (Values.dtype_of_values body)) }
   },
   body)
