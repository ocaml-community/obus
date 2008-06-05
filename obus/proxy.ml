(*
 * proxy.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


type name = string
type path = string

type 'a t = {
  connection : Connection.t;
  interface : 'a Interface.t;
  sender : name option;
  name : name option;
  path : path;
}

let make connection interface ?sender ?destination path =
  { connection = connection;
    interface = interface;
    name = destination;
    path = path;
    sender = sender }

let path { path = x } = x
let name { name = x } = x
let sender { sender = x } = x
let connection { connection = x } = x

type ('a, 'b, 'c) method_call_desc = {
  method_interface : 'a Interface.t;
  method_member : string;
  method_in_sig : string;
  method_out_sig : string;
  method_le_reader : 'b -> Wire.buffer -> Wire.ptr -> 'c;
  method_be_reader : 'b -> Wire.buffer -> Wire.ptr -> 'c;
}

open Header

let handle_reply desc cont header buffer i = match header.message_type with
  | Error ->
      Error.raise_error header buffer i
  | _ ->
      let out_sig = match header.fields.signature with
        | Some s -> s
        | None -> ""
      in
        if out_sig = desc.method_out_sig
        then match header.Header.byte_order with
          | Wire.Little_endian -> desc.method_le_reader cont buffer i
          | Wire.Big_endian -> desc.method_be_reader cont buffer i
        else raise (Wire.Content_error
                      (Printf.sprintf
                         "unexpected_signature for method %s.%s, expected: %s, got: %s"
                         (Interface.name desc.method_interface)
                         desc.method_member
                         desc.method_out_sig
                         out_sig))

let proxy_call func desc writer cont proxy =
  func (connection proxy)
    { byte_order = Info.native_byte_order;
      message_type = Method_call;
      flags = default_flags;
      length = ();
      serial = ();
      fields =
        { path = Some (path proxy);
          member = Some desc.method_member;
          interface = Some (Interface.name desc.method_interface);
          error_name = None;
          destination = name proxy;
          reply_serial = None;
          sender = sender proxy;
          signature = Some desc.method_in_sig } }
    writer (handle_reply desc cont)
