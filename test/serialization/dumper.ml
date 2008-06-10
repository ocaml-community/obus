(*
 * dumper.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus

let make prefix t =
  Transport.make
    ~backend:(Transport.backend t)
    ~recv:begin let n = ref 0 in
      fun buf pos count ->
        let len = Transport.recv t buf pos count in
        let f = open_out (Printf.sprintf "%s%03d.recv" prefix !n) in
          incr n;
          output f buf pos len;
          close_out f;
          len
    end
    ~send:begin let n = ref 0 in
      fun buf pos count ->
        let len = Transport.send t buf pos count in
        let f = open_out (Printf.sprintf "%s%03d.send" prefix !n) in
          incr n;
          output f buf pos len;
          close_out f;
          len
    end
    ~close:(Transport.close t)
    ()

open Values

let notify connection title msg =
  let (_, body) =
    Connection.send_message_sync connection
      (Header.method_call
         ~destination:"org.freedesktop.Notifications"
         ~path:"/org/freedesktop/Notifications"
         ~interface:"org.freedesktop.Notifications"
         ~member:"Notify" ())
      (make_values
         (ccons cstring
            (ccons cuint32
               (ccons cstring
                  (ccons cstring
                     (ccons cstring
                        (ccons (carray cstring)
                           (ccons (cdict cstring cvariant)
                              (ccons cint32 cnil))))))))
         ((Filename.basename Sys.argv.(0)) (* app_name *)
            ,(0l (* id *)
                , (  "info" (* icon *)
                       , (  title (* summary *)
                              , (  msg (* body *)
                                     , (  [] (* actions *)
                                            , (  [] (* hints *)
                                                   ,    (  5000l (* timeout *), ())))))))))
  in
  let (return_id, ()) = get_values (ccons cuint32 cnil) body in
    return_id

let _ =
  let transport = make "/tmp/obus-dump" (Transport.of_addresses (Address.session ())) in
  let connection = Connection.of_transport transport in
    Bus.register_connection connection;
    ignore (notify connection "Hello, world!" "ocaml is fun!")

