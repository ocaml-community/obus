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
type body = Values.t list

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

type ('a, 'b) _message = {
  flags : flags;
  serial : serial;
  typ : 'a;
  destination : destination option;
  sender : sender option;
  body : 'b;
}
type 'a message = ('a, Values.t list) _message

let flags message = message.flags
let serial message = message.serial
let typ message = message.typ
let destination message = message.destination
let sender message = message.sender
let body message = message.body

let signature message = Types.to_signature (List.map Values.typ (body message))

type t = any_type message
type method_call = method_call_type message
type method_return = method_return_type message
type signal = signal_type message
type error = error_type message

let make ?(flags=default_flags) ?sender ?destination ~typ ~body () =
  { flags = flags;
    serial = 0l;
    typ = typ;
    destination = destination;
    sender = sender;
    body = body }

let method_call ?flags ?sender ?destination ~path ?interface ~member ~body () =
  make ?flags ?sender ?destination ~typ:(`Method_call(path, interface, member)) ~body:body ()

let method_return ?flags ?sender ?destination ~reply_serial ~body () =
  make ?flags ?sender ?destination ~typ:(`Method_return(reply_serial)) ~body:body ()

let error ?flags ?sender ?destination ~reply_serial ~error_name ~body:body () =
  make ?flags ?sender ?destination ~typ:(`Error(reply_serial, error_name)) ~body:body ()

let signal ?flags ?sender ?destination ~path ~interface ~member ~body () =
  make ?flags ?sender ?destination ~typ:(`Signal(path, interface, member)) ~body:body ()
