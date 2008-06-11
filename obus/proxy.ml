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
  name : name option;
  path : path;
}

let make connection interface ?destination path =
  { connection = connection;
    interface = interface;
    name = destination;
    path = path }

let path { path = x } = x
let name { name = x } = x
let connection { connection = x } = x

open Wire

type ('a, 'b) intern_method_call_desc = {
  intern_mcd_interface : 'a Interface.t;
  intern_mcd_member : string;
  intern_mcd_input_signature : string;
  intern_mcd_output_signature : string;
  intern_mcd_reader : 'b body_reader;
}

open Header
open Connection

let fail desc header =
  failwith
    (Printf.sprintf
       "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
       desc.intern_mcd_member
       (Interface.name desc.intern_mcd_interface)
       desc.intern_mcd_output_signature
       header.signature)

let handle_reply desc header byte_order buffer ptr =
  if header.signature = desc.intern_mcd_output_signature
  then desc.intern_mcd_reader byte_order buffer ptr
  else fail desc header

let handle_reply_async desc f header byte_order buffer ptr =
  if header.signature = desc.intern_mcd_output_signature
  then let x = desc.intern_mcd_reader byte_order buffer ptr in
    (fun () -> f x)
  else fail desc header

let make_header desc ?flags proxy =
  method_call
    ?flags
    ?destination:(name proxy)
    ~path:(path proxy)
    ~member:desc.intern_mcd_member
    ~interface:(Interface.name desc.intern_mcd_interface)
    ~signature:desc.intern_mcd_input_signature ()

let intern_proxy_call_sync proxy desc writer =
  intern_send_message_sync (connection proxy) (make_header desc proxy) writer (handle_reply desc)
let intern_proxy_call_async proxy desc writer ?on_error f =
  intern_send_message_async (connection proxy) (make_header desc proxy) writer ?on_error (handle_reply_async desc f)
let intern_proxy_call_cookie proxy desc writer =
  intern_send_message_cookie (connection proxy) (make_header desc proxy) writer (handle_reply desc)
let intern_proxy_call_no_reply proxy desc writer =
  intern_send_message (connection proxy)
    (make_header desc ~flags:{ no_reply_expected = true;
                               no_auto_start = true }
       proxy) writer
