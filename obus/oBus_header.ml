(*
 * oBus_header.ml
 * --------------
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

let make_flags ?(no_reply_expected=false) ?(no_auto_start=false) () = {
  no_reply_expected = no_reply_expected;
  no_auto_start = no_auto_start;
}

type method_call_type =
    [ `Method_call of path * interface option * member ]
type method_return_type =
    [ `Method_return of reply_serial ]
type error_type =
    [ `Error of reply_serial * error_name ]
type signal_type =
    [ `Signal of path * interface * member ]

type message_type =
    [ method_call_type
    | method_return_type
    | error_type
    | signal_type ]

type 'typ t = {
  flags : flags;
  serial : serial;
  typ : 'typ;
  destination : string option;
  sender : string option;
}
constraint 'typ = [< message_type ]

let flags message = message.flags
let serial message = message.serial
let typ message = message.typ
let destination message = message.destination
let sender message = message.sender

type method_call = method_call_type t
type method_return = method_return_type t
type signal = signal_type t
type error = error_type t
type any = message_type t

let make ?(flags=default_flags) ?(serial=0l) ?sender ?destination ~typ () =
  { flags = flags;
    serial = serial;
    typ = typ;
    destination = destination;
    sender = sender }

let method_call ?flags ?serial ?sender ?destination ~path ?interface ~member () =
  make ?flags ?serial ?sender ?destination ~typ:(`Method_call(path, interface, member)) ()

let method_return ?flags ?serial ?sender ?destination ~reply_serial () =
  make ?flags ?serial ?sender ?destination ~typ:(`Method_return(reply_serial)) ()

let error ?flags ?serial ?sender ?destination ~reply_serial ~error_name () =
  make ?flags ?serial ?sender ?destination ~typ:(`Error(reply_serial, error_name)) ()

let signal ?flags ?serial ?sender ?destination ~path ~interface ~member () =
  make ?flags ?serial ?sender ?destination ~typ:(`Signal(path, interface, member)) ()
