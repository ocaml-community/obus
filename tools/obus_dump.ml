(*
 * obus_dump.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

let out = ref "/dev/stderr"

let args = [
  "-o", Arg.Set_string out,
  "<file> output messages to this file instead of stderr";
]

let usage_msg = Printf.sprintf "Usage: %s <options> cmd args
Execute 'cmd' and dump all messages it sent to session and system bus
options are:" (Filename.basename (Sys.argv.(0)))

let rec loop pp action what_bus a b =
  (perform
     message <-- OBus_lowlevel.recv a;
     let _ =
       Format.fprintf pp "-----@\n@[<hv 2>%s on %s bus:@\n%a@]@." action what_bus
         OBus_message.print message
     in
     OBus_lowlevel.send b message;
     loop pp action what_bus a b)

let launch pp what_bus laddresses =
  (perform
     bus_addresses <-- Lazy.force laddresses;
     server <-- OBus_server.make_lowlevel
       (fun transport ->
          perform
            (_, bus) <-- OBus_lowlevel.transport_of_addresses bus_addresses;
            choose [loop pp "message received" what_bus bus transport;
                    loop pp "sending message" what_bus transport bus]);
     let _ = Unix.putenv (Printf.sprintf "DBUS_%s_BUS_ADDRESS" (String.uppercase what_bus))
       (OBus_address.to_string (OBus_server.addresses server)) in
     return ())

let cmd_args = ref []

let _ =
  Arg.parse args
    (fun s -> cmd_args := s :: !cmd_args)
    usage_msg;

  let cmd_args = List.rev !cmd_args in
  let cmd = match cmd_args with
    | [] ->
        Arg.usage args usage_msg;
        exit 2
    | x :: _ -> x
  in

  let oc = open_out !out in
  let pp = Format.formatter_of_out_channel oc in

  Lwt_unix.run
    (perform
       Lwt_util.join [launch pp "session" OBus_address.session;
                      launch pp "system" OBus_address.system];
       Lwt_unix.waitpid []
         (Unix.create_process cmd (Array.of_list cmd_args)
            Unix.stdin Unix.stdout Unix.stderr);
       let _ = close_out oc in
       return ())
