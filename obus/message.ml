(*
 * header.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type serial = int32
type path = string
type interface = string
type member = string
type error_name = string
type reply_serial = serial
type destination = string
type sender = string
type signature = string

type flags = {
  no_reply_expected : bool;
  no_auto_start : bool;
}

let no_reply_expected flags = flags.no_reply_expected
let no_auto_start flags = flags.no_auto_start

let  default_flags = {
  no_reply_expected = false;
  no_auto_start = false;
}

type method_call_type =
    [ `Method_call of path * interface option * member ]
type method_return_type =
    [ `Method_return of reply_serial ]
type error_type =
    [ `Error of reply_serial * error_name ]
type signal_type =
    [ `Signal of path * interface * member ]

type any_type =
    [ method_call_type
    | method_return_type
    | error_type
    | signal_type ]

type ('typ, 'body) message = {
  flags : flags;
  serial : serial;
  typ : 'typ;
  destination : destination option;
  sender : sender option;
  signature : signature;
  body : 'body;
}

let flags message = message.flags
let serial message = message.serial
let typ message = message.typ
let destination message = message.destination
let sender message = message.sender
let signature message = message.signature
let body message = message.body

type body = Values.values
type t = (any_type, body) message
type method_call = (method_call_type, body) message
type method_return = (method_return_type, body) message
type signal = (signal_type, body) message
type error = (error_type, body) message

let make ?(flags=default_flags) ?sender ?destination ~typ ~body () =
  { flags = flags;
    serial = 0l;
    typ = typ;
    destination = destination;
    sender = sender;
    signature =
      Values.signature_of_dtypes
        (Values.dtypes_of_values body);
    body = body }

let method_call ?flags ?sender ?destination ~path ?interface ~member ~body () =
  make ?flags ?sender ?destination ~typ:(`Method_call(path, interface, member)) ~body:body ()

let method_return ?flags ?sender ?destination ~reply_serial ~body () =
  make ?flags ?sender ?destination ~typ:(`Method_return(reply_serial)) ~body:body ()

let error ?flags ?sender ?destination ~reply_serial ~error_name ~body:body () =
  make ?flags ?sender ?destination ~typ:(`Error(reply_serial, error_name)) ~body:body ()

let signal ?flags ?sender ?destination ~path ~interface ~member ~body () =
  make ?flags ?sender ?destination ~typ:(`Signal(path, interface, member)) ~body:body ()

open Wire
type send_body = body_writer
type recv_body = byte_order * buffer * ptr
type 'a intern_send = ('a, send_body) message
type 'a intern_recv = ('a, recv_body) message
let intern_make_send ?(flags=default_flags) ?sender ?destination ~signature ~typ ~body () =
  { flags = flags;
    serial = 0l;
    typ = typ;
    destination = destination;
    sender = sender;
    signature = signature;
    body = body }
let intern_make_recv ?(flags=default_flags) ?sender ?destination ~serial ~signature ~typ ~body () =
  { flags = flags;
    serial = serial;
    typ = typ;
    destination = destination;
    sender = sender;
    signature = signature;
    body = body }

let intern_user_to_send message =
  { message
    with body = fun byte_order buffer ptr -> match byte_order with
      | Little_endian -> Values.LEWriter.write_values buffer ptr (body message)
      | Big_endian -> Values.BEWriter.write_values buffer ptr (body message) }

let intern_recv_to_user message =
  { message
    with body =
      let (byte_order, buffer, ptr) = body message in
      let ts = Values.dtypes_of_signature (signature message) in
        match byte_order with
          | Little_endian -> snd (Values.LEReader.read_values ts buffer ptr)
          | Big_endian -> snd (Values.BEReader.read_values ts buffer ptr) }


