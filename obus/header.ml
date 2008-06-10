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

let  default_flags = {
  no_reply_expected = false;
  no_auto_start = false;
}

type method_call_typ =
    [ `Method_call of path * interface option * member ]
type method_return_typ =
    [ `Method_return of reply_serial ]
type error_typ =
    [ `Error of reply_serial * error_name ]
type signal_typ =
    [ `Signal of path * interface * member ]

type any_typ =
    [ method_call_typ
    | method_return_typ
    | error_typ
    | signal_typ ]

type 'a header = {
  flags : flags;
  serial : serial;
  message_type : 'a;
  destination : destination option;
  sender : sender option;
  signature : signature;
}

type t = any_typ header

type method_call = method_call_typ header
type method_return = method_return_typ header
type signal = signal_typ header
type error = error_typ header

let method_call ?(flags=default_flags) ?sender ?destination ?(signature="") ~path ?interface ~member () =
  { flags = flags;
    serial = 0l;
    message_type = `Method_call(path, interface, member);
    destination = destination;
    sender = sender;
    signature = signature }

let method_return ?(flags=default_flags) ?sender ?destination ?(signature="") ~reply_serial () =
  { flags = flags;
    serial = 0l;
    message_type = `Method_return(reply_serial);
    destination = destination;
    sender = sender;
    signature = signature }

let error ?(flags=default_flags) ?sender ?destination ?(signature="") ~reply_serial ~error_name () =
  { flags = flags;
    serial = 0l;
    message_type = `Error(reply_serial, error_name);
    destination = destination;
    sender = sender;
    signature = signature }

let signal ?(flags=default_flags) ?sender ?destination  ?(signature="") ~path ~interface ~member () =
  { flags = flags;
    serial = 0l;
    message_type = `Signal(path, interface, member);
    destination = destination;
    sender = sender;
    signature = signature }
