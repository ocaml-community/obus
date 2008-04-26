(*
 * hello.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This sample is the most simple, it just open a dbus connection to
   the message bus and then send a "Hello". *)

open OBus
open Message
open Values

let _ =
  (* Open a connection with the message bus *)
  let connection = Connection.of_addresses (Address.session ()) false in
    (* Now send a "Hello", this is required before sending any other
       messages. The message bus send us a unique connection name that
       is valid for the lifetime of the message bus. *)
    (* This is the creation of the message *)
  let hello_message = method_call (* function for creating methoc_call message *)
    (* no flags: *)
    []
    (* the destination, this is the message bus: *)
    "org.freedesktop.DBus"
    (* the object path on the message bus: *)
    "/org/freedesktop/DBus"
    (* the interface we want to use: *)
    "org.freedesktop.DBus"
    (* the member of the interface we want to call: *)
    "Hello"
    (* finally the body of the message, which is empty: *)
    []
  in
    (* Now we send it, synchrounously so we wait for the reply *)
  let reply = Connection.send_message_sync connection hello_message in
    (* [reply] is composed of the header and the body of the reply
       message *)
  let (_, body) = reply in
    (* [body] is composed of a sequence of dbus values, in a
       "marshaled" representation.  We now use [get_values] to
       unmarshal the values into a simple ocaml value. If the body
       does not contain what we are claiming (here it is a sequence of
       just one string), this is an error, and [get_values] will raise
       an exception *)
    try
      let (name, ()) = get_values (cons string nil) body in
        (* The content of the reply to "Hello" is just one string: the
           unique connection name: *)
        Printf.printf "My unique connection name is: %s\n" name
    with
        Failure _ ->
          Printf.printf "invalid reply to hello: %s\n" (string_of_values body)
