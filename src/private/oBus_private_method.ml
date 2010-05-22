(*
 * oBus_private_method.ml
 * ----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_message

exception Invalid_reply of string

let call_with_context ~connection ~path ?destination ~interface ~member ~i_args ~o_args args =
  lwt msg =
    OBus_connection.send_message_with_reply
      connection
      (method_call
         ?destination
         ~path
         ~interface
         ~member
         (OBus_value.C.make_sequence i_args args))
  in
  match msg with
    | { typ = Method_return _ } -> begin
        let context = OBus_private_connection.make_context connection msg in
        try
          return (context, OBus_value.C.cast_sequence o_args (body msg))
        with OBus_value.C.Signature_mismatch ->
          fail
            (Invalid_reply
               (Printf.sprintf
                  OBus_constant.invalid_reply
                  member
                  interface
                  (OBus_value.string_of_signature (OBus_value.C.type_sequence o_args))
                  (OBus_value.string_of_signature (OBus_value.V.type_of_sequence msg.body))))
      end
    | { typ = Error(_, error_name); body = OBus_value.V.Basic(OBus_value.V.String message) :: _  } ->
        fail (OBus_error.make_by_name error_name message)
    | { typ = Error(_, error_name) } ->
        fail (OBus_error.make_by_name error_name "")
    | _ ->
        assert false

let call ~connection ~path ?destination ~interface ~member ~i_args ~o_args args =
  call_with_context ~connection ~path ?destination ~interface ~member ~i_args ~o_args args >|= snd

let call_no_reply ~connection ~path ?destination ~interface ~member ~i_args args =
  OBus_connection.send_message
    connection
    (method_call
       ~flags:{ default_flags with no_reply_expected = true }
       ?destination
       ~path
       ~interface
       ~member
       (OBus_value.C.make_sequence i_args args))
