(*
 * oBus_message.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type serial = int32
type body = OBus_value.V.sequence

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
  | Method_call of OBus_path.t * OBus_name.interface * OBus_name.member
  | Method_return of serial
  | Error of serial * OBus_name.error
  | Signal of OBus_path.t * OBus_name.interface * OBus_name.member

type t = {
  flags : flags;
  serial : serial;
  typ : typ;
  destination : OBus_name.bus;
  sender : OBus_name.bus;
  body : body;
}

let flags m = m.flags
let serial m = m.serial
let typ m = m.typ
let destination m = m.destination
let sender m = m.sender
let body m = m.body

let make ?(flags=default_flags) ?(serial=0l) ?(sender="") ?(destination="") ~typ body =
  { flags = flags;
    serial = serial;
    typ = typ;
    destination = destination;
    sender = sender;
    body = body }

let method_call ?flags ?serial ?sender ?destination ~path ?(interface="") ~member body =
  make ?flags  ?serial ?sender ?destination ~typ:(Method_call(path, interface, member)) body

let method_return ?flags ?serial ?sender ?destination ~reply_serial body =
  make ?flags ?serial ?sender ?destination ~typ:(Method_return(reply_serial)) body

let error ?flags ?serial ?sender ?destination ~reply_serial ~error_name body =
  make ?flags ?serial ?sender ?destination ~typ:(Error(reply_serial, error_name)) body

let signal ?flags ?serial ?sender ?destination ~path ~interface ~member body =
  make ?flags ?serial ?sender ?destination ~typ:(Signal(path, interface, member)) body

exception Invalid_reply of string

let invalid_reply ~method_call ~expected_signature ~method_return =
  match method_call, method_return with
    | { typ = Method_call(path, interface, member) }, { typ = Method_return _; body } ->
        Invalid_reply
          (Printf.sprintf
             "unexpected signature for the reply to the method %S on interface %S, expected: %S, got: %S"
             member
             interface
             (OBus_value.string_of_signature expected_signature)
             (OBus_value.string_of_signature (OBus_value.V.type_of_sequence body)))
    | _ ->
        invalid_arg "OBus_message.invalid_reply"

open Format
open OBus_value

let print pp message =
  fprintf pp
    "no_reply_expected = %B@\n\
     no_auto_start = %B@\n\
     serial = %ld@\n\
     message_type = %a@\n\
     sender = %S@\n\
     destination = %S@\n\
     signature = %S@\n\
     body_type = %a@\n\
     body = %a@\n"
    message.flags.no_reply_expected
    message.flags.no_auto_start
    message.serial
    (fun pp -> function
       | Method_call(path, interface, member) ->
           fprintf pp
             "method_call@\n\
              path = %S@\n\
              interface = %S@\n\
              member = %S"
             (OBus_path.to_string path) interface member
       | Method_return reply_serial ->
           fprintf pp
             "method_return@\n\
              reply_serial = %ld"
             reply_serial
       | Error(reply_serial, error_name) ->
           fprintf pp
             "error@\n\
              reply_serial = %ld@\n\
              error_name = %S"
             reply_serial error_name
       | Signal(path, interface, member) ->
           fprintf pp
             "signal@\n\
              path = %S@\n\
              interface = %S@\n\
              member = %S"
             (OBus_path.to_string path) interface member)
    message.typ
    message.sender
    message.destination
    (string_of_signature (V.type_of_sequence message.body))
    T.print_sequence (V.type_of_sequence message.body)
    V.print_sequence message.body
