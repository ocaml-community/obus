(*
 * oBus_message.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type serial = int32
type body = OBus_value.sequence

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
    [ `Method_call of OBus_path.t * OBus_name.Interface.t option * OBus_name.Member.t ]
type method_return_type =
    [ `Method_return of serial ]
type error_type =
    [ `Error of serial * OBus_name.Error.t ]
type signal_type =
    [ `Signal of OBus_path.t * OBus_name.Interface.t * OBus_name.Member.t ]

type any_type =
    [ method_call_type
    | method_return_type
    | error_type
    | signal_type ]

type 'typ t = {
  flags : flags;
  serial : serial;
  typ : 'typ;
  destination : OBus_name.Connection.t option;
  sender : OBus_name.Connection.t option;
  body : body;
}

let body message = message.body
let flags message = message.flags
let serial message = message.serial
let typ message = message.typ
let destination message = message.destination
let sender message = message.sender
let path h = match h.typ with
  | `Method_call(path, interface, member) -> path
  | `Signal(path, interface, member) -> path
let interface h = match h.typ with
  | `Method_call(path, interface, member) -> interface
  | `Signal(path, interface, member) -> Some(interface)
let signal_interface { typ = `Signal(path, interface, member) } = interface
let member h = match h.typ with
  | `Method_call(path, interface, member) -> member
  | `Signal(path, interface, member) -> member
let reply_serial h = match h.typ with
  | `Method_return serial -> serial
  | `Error(serial, name) -> serial
let error_name { typ = `Error(serial, name) } = name

type method_call = method_call_type t
type method_return = method_return_type t
type signal = signal_type t
type error = error_type t
type any = any_type t

let make ?(flags=default_flags) ?(serial=0l) ?sender ?destination ~typ body =
  { flags = flags;
    serial = serial;
    typ = typ;
    destination = destination;
    sender = sender;
    body = body }

let method_call ?flags ?serial ?sender ?destination ~path ?interface ~member body =
  make ?flags ?serial ?sender ?destination ~typ:(`Method_call(path, interface, member)) body

let method_return ?flags ?serial ?sender ?destination ~reply_serial body =
  make ?flags ?serial ?sender ?destination ~typ:(`Method_return(reply_serial)) body

let error ?flags ?serial ?sender ?destination ~reply_serial ~error_name body =
  make ?flags ?serial ?sender ?destination ~typ:(`Error(reply_serial, error_name)) body

let signal ?flags ?serial ?sender ?destination ~path ~interface ~member body =
  make ?flags ?serial ?sender ?destination ~typ:(`Signal(path, interface, member)) body

open Format
open OBus_value

let print pp message =
  let opt pp = function
    | Some s -> fprintf pp "Some %S" s
    | None -> pp_print_string pp "None"
  in
  fprintf pp "\
no_reply_expected = %B@
no_auto_start = %B@
serial = %ld@
message_type = %a@
destination = %a@
sender = %a@
signature = %S@
body_type = %s@
body = %s@
" message.flags.no_reply_expected message.flags.no_auto_start message.serial
    (fun pp -> function
       | `Method_call(path, interface, member) ->
           fprintf pp "method_call@
path = %S@
interface = %a@
member = %S" (OBus_path.to_string path) opt interface member
       | `Method_return reply_serial ->
           fprintf pp "method_return@
reply_serial = %ld" reply_serial
       | `Error(reply_serial, error_name) ->
           fprintf pp "error@
reply_serial = %ld@
error_name = %S" reply_serial error_name
       | `Signal(path, interface, member) ->
           fprintf pp "signal@
path = %S@
interface = %S@
member = %S" (OBus_path.to_string path) interface member) message.typ
    opt message.destination
    opt message.sender
    (string_of_signature (type_of_sequence message.body))
    (string_of_tsequence  (type_of_sequence message.body))
    (string_of_sequence message.body)
