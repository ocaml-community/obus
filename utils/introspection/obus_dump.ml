(*
 * obus_dump.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt.Infix

let usage_msg = Printf.sprintf "Usage: %s <options> cmd args
Execute 'cmd' and dump all messages it sent to session and system bus
options are:" (Filename.basename (Sys.argv.(0)))

let rec loop pp action what_bus a b =
  let%lwt message = OBus_transport.recv a in
  Format.fprintf pp "-----@\n@[<hv 2>%s on %s bus:@\n%a@]@." action what_bus
    OBus_message.print message;
  let%lwt () = OBus_transport.send b message in
  loop pp action what_bus a b

let launch pp what_bus laddresses =
  let%lwt addresses = Lazy.force laddresses in
  let%lwt server =
    OBus_server.make_lowlevel ~capabilities:[`Unix_fd]
      (fun server transport ->
         ignore begin
           let%lwt (_, bus) = OBus_transport.of_addresses ~capabilities:[`Unix_fd] addresses in
           Lwt.choose [loop pp "message received" what_bus bus transport;
                       loop pp "sending message" what_bus transport bus]
         end)
  in
  Unix.putenv (Printf.sprintf "DBUS_%s_BUS_ADDRESS" (String.uppercase_ascii what_bus)) (OBus_address.to_string (OBus_server.addresses server));
  Lwt.return ()


let () =
  let out = ref "/dev/stderr" and cmd_args = ref [] in
  let anon_fun s = cmd_args := s :: !cmd_args in
  let args = [
    "-o", Arg.Set_string out, "<file> output messages to this file instead of stderr";
    "--", Arg.Rest anon_fun, "command separator";
  ] in
  Arg.parse args anon_fun usage_msg;

  let cmd_args = List.rev !cmd_args in
  let cmd = match cmd_args with
    | [] ->
        Arg.usage args usage_msg;
        exit 2
    | x :: _ -> x
  in

  let oc = open_out !out in
  let pp = Format.formatter_of_out_channel oc in

  Lwt_main.run begin
    let%lwt () = launch pp "session" OBus_address.session <&> launch pp "system" OBus_address.system in
    let%lwt _ = Lwt_unix.waitpid [] (Unix.create_process cmd (Array.of_list cmd_args) Unix.stdin Unix.stdout Unix.stderr) in
    close_out oc;
    Lwt.return ()
  end
