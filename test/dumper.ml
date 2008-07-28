(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_pervasives
open Lwt

let make prefix t =
  OBus_transport.make
    ~backend:(OBus_transport.backend t)
    ~recv:begin let n = ref 0 in
      fun buf pos count ->
        OBus_transport.recv t buf pos count
        >>= fun len ->
          let f = open_out (Printf.sprintf "%s%03d.recv" prefix !n) in
            incr n;
            output f buf pos len;
            close_out f;
            return len
    end
    ~send:begin let n = ref 0 in
      fun buf pos count ->
        OBus_transport.send t buf pos count
        >>= fun len ->
          let f = open_out (Printf.sprintf "%s%03d.send" prefix !n) in
            incr n;
            output f buf pos len;
            close_out f;
            return len
    end
    ~close:(OBus_transport.close t)
    ()

let notify connection title msg =
  (perform
     (header, id) <-- OBus_connection.send_message_with_reply connection
       (OBus_header.method_call
          ~destination:"org.freedesktop.Notifications"
          ~path:"/org/freedesktop/Notifications"
          ~interface:"org.freedesktop.Notifications"
          ~member:"Notify" ())
       (ob_string -->
          (ob_uint32 -->
             (ob_string -->
                (ob_string -->
                   (ob_string -->
                      (ob_list ob_string -->
                         (ob_assoc ob_string ob_variant -->
                            (ob_int32 --> ob_reply ob_uint32))))))))
       (Filename.basename Sys.argv.(0)) 0l "info" title msg [] [] 5000l;
     return id)

(*let reply_header, return_id =

  (*      ~body:[string (Filename.basename Sys.argv.(0)); (* app_name *)
          uint32 0l; (* id *)
          string "info"; (* icon *)
          string title; (* summary *)
          string msg; (* body *)
          array `string []; (* actions *)
          dict `string `variant []; (* hints *)
          int32 5000l] (* timeout *)*)
  in*)

let _ =
  Lwt_unix.run
    (perform
       transport <-- OBus_transport.of_addresses(Lazy.force OBus_address.session);
       bus <-- OBus_connection.of_transport (make "/tmp/obus-" transport);
       (_, name) <-- OBus_connection.send_message_with_reply bus
         (OBus_header.method_call
            ~destination:"org.freedesktop.DBus"
            ~member:"Hello"
            ~path:"/org/freedesktop/DBus"
            ~interface:"org.freedesktop.DBus" ())
         (ob_reply ob_string);
       return (Printf.printf "my name is: %s\n%!" name);
       notify bus "Hello, world!" "ocaml is fun!")
