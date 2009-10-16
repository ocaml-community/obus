(*
 * oBus_message.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
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

type typ =
  | Method_call of OBus_path.t * OBus_name.interface option * OBus_name.member
  | Method_return of serial
  | Error of serial * OBus_name.error
  | Signal of OBus_path.t * OBus_name.interface * OBus_name.member

type t = {
  flags : flags;
  serial : serial;
  typ : typ;
  destination : OBus_name.bus option;
  sender : OBus_name.bus option;
  body : body;
} with projection

let make ?(flags=default_flags) ?(serial=0l) ?sender ?destination ~typ body =
  { flags = flags;
    serial = serial;
    typ = typ;
    destination = destination;
    sender = sender;
    body = body }

let method_call ?flags ?serial ?sender ?destination ~path ?interface ~member body =
  make ?flags ?serial ?sender ?destination ~typ:(Method_call(path, interface, member)) body

let method_return ?flags ?serial ?sender ?destination ~reply_serial body =
  make ?flags ?serial ?sender ?destination ~typ:(Method_return(reply_serial)) body

let error ?flags ?serial ?sender ?destination ~reply_serial ~error_name body =
  make ?flags ?serial ?sender ?destination ~typ:(Error(reply_serial, error_name)) body

let signal ?flags ?serial ?sender ?destination ~path ~interface ~member body =
  make ?flags ?serial ?sender ?destination ~typ:(Signal(path, interface, member)) body

open Format
open OBus_value

let print pp message =
  let opt pp = function
    | Some x -> fprintf pp "%S" x
    | None -> pp_print_string pp "-"
  in
  fprintf pp "\
no_reply_expected = %B@
no_auto_start = %B@
serial = %ld@
message_type = %a@
sender = %a@
destination = %a@
signature = %S@
body_type = %a@
body = %a@
" message.flags.no_reply_expected message.flags.no_auto_start message.serial
    (fun pp -> function
       | Method_call(path, interface, member) ->
           fprintf pp "method_call@
path = %S@
interface = %a@
member = %S" (OBus_path.to_string path) opt interface member
       | Method_return reply_serial ->
           fprintf pp "method_return@
reply_serial = %ld" reply_serial
       | Error(reply_serial, error_name) ->
           fprintf pp "error@
reply_serial = %ld@
error_name = %S" reply_serial error_name
       | Signal(path, interface, member) ->
           fprintf pp "signal@
path = %S@
interface = %S@
member = %S" (OBus_path.to_string path) interface member) message.typ
    opt message.sender
    opt message.destination
    (string_of_signature (type_of_sequence message.body))
    print_tsequence (type_of_sequence message.body)
    print_sequence message.body
