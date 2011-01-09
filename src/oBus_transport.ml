(*
 * oBus_transport.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(transport)"

open Unix
open Printf
open Lwt
open OBus_address

(* +-----------------------------------------------------------------+
   | Types and constructors                                          |
   +-----------------------------------------------------------------+ *)

type t = {
  recv : unit -> OBus_message.t Lwt.t;
  send : OBus_message.t -> unit Lwt.t;
  capabilities : OBus_auth.capability list;
  shutdown : unit -> unit Lwt.t;
}

let make ?switch ~recv ~send ?(capabilities=[]) ~shutdown () =
  let transport = {
    recv = recv;
    send = send;
    capabilities = capabilities;
    shutdown = shutdown;
  } in
  Lwt_switch.add_hook switch transport.shutdown;
  transport

let recv t = t.recv ()
let send t message = t.send message
let capabilities t = t.capabilities
let shutdown t = t.shutdown ()

(* +-----------------------------------------------------------------+
   | Socket transport                                                |
   +-----------------------------------------------------------------+ *)

let socket_of_wires ?switch ?(capabilities=[]) fd (reader, writer) =
  let transport =
    { recv = (fun _ -> OBus_wire.read_message_with_fds reader);
      send = (fun msg -> OBus_wire.write_message_with_fds writer msg);
      capabilities = capabilities;
      shutdown = (fun _ ->
                    lwt () = OBus_wire.close_reader reader <&> OBus_wire.close_writer writer in
                    Lwt_unix.shutdown fd SHUTDOWN_ALL;
                    Lwt_unix.close fd) }
  in
  Lwt_switch.add_hook switch transport.shutdown;
  transport

let socket_of_channels ?switch ?(capabilities=[]) fd (ic, oc) =
  let transport =
    { recv = (fun _ -> OBus_wire.read_message ic);
      send = (fun msg -> OBus_wire.write_message oc msg);
      capabilities = capabilities;
      shutdown = (fun _ ->
                    lwt () = Lwt_io.close ic <&> Lwt_io.close oc in
                    Lwt_unix.shutdown fd SHUTDOWN_ALL;
                    Lwt_unix.close fd) }
  in
  Lwt_switch.add_hook switch transport.shutdown;
  transport

let socket_and_auth_stream ?switch ?(capabilities=[]) fd =
  if List.mem `Unix_fd capabilities then
    let reader = OBus_wire.reader fd
    and writer = OBus_wire.writer fd in
    (socket_of_wires ?switch ~capabilities fd (reader, writer),
     OBus_wire.auth_stream (reader, writer))
  else
    let ic = Lwt_io.make ~mode:Lwt_io.input (Lwt_bytes.read fd)
    and oc = Lwt_io.make ~mode:Lwt_io.output (Lwt_bytes.write fd) in
    (socket_of_channels ?switch ~capabilities fd (ic, oc),
     OBus_auth.stream_of_channels (ic, oc))

let socket ?switch ?capabilities fd =
  fst (socket_and_auth_stream ?switch ?capabilities fd)

(* +-----------------------------------------------------------------+
   | Loopback transport                                              |
   +-----------------------------------------------------------------+ *)

let loopback () =
  let mvar = Lwt_mvar.create_empty () in
  { recv = (fun _ -> Lwt_mvar.take mvar);
    send = (fun m -> Lwt_mvar.put mvar { m with OBus_message.body = OBus_value.V.sequence_dup (OBus_message.body m) });
    capabilities = [`Unix_fd];
    shutdown = return }

(* +-----------------------------------------------------------------+
   | Addresses -> transport                                          |
   +-----------------------------------------------------------------+ *)

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  (try Lwt_unix.set_close_on_exec fd with _ -> ());
  try_lwt
    lwt () = Lwt_unix.connect fd addr in
    return (fd, domain)
  with exn ->
    lwt () = Lwt_unix.close fd in
    raise_lwt exn

let rec write_nonce fd nonce pos len =
  Lwt_unix.write fd nonce 0 16 >>= function
    | 0 ->
        raise_lwt (Failure "OBus_transport.connect: failed to send the nonce to the server")
    | n ->
        if n = len then
          return ()
        else
          write_nonce fd nonce (pos + n) (len - n)

let make_socket_nonce nonce_file domain typ addr =
  match nonce_file with
    | None ->
        raise_lwt (Invalid_argument "OBus_transport.connect: missing 'noncefile' parameter")
    | Some file_name ->
        lwt nonce =
          try_lwt
            Lwt_io.with_file ~mode:Lwt_io.input file_name (Lwt_io.read ~count:16)
          with
            | Unix.Unix_error(err, _, _) ->
                raise_lwt (Failure(Printf.sprintf "failed to read the nonce file '%s': %s" file_name (Unix.error_message err)))
            | End_of_file ->
                raise_lwt (Failure(Printf.sprintf "OBus_transport.connect: '%s' is an invalid nonce-file" file_name))
        in
        if String.length nonce <> 16 then
          raise_lwt (Failure(Printf.sprintf "OBus_transport.connect: '%s' is an invalid nonce-file" file_name))
        else begin
          lwt fd, domain = make_socket domain typ addr in
          lwt () = write_nonce fd nonce 0 16 in
          return (fd, domain)
        end

let rec connect address =
  match OBus_address.name address with
    | "unix" -> begin
        match (OBus_address.arg "path" address,
               OBus_address.arg "abstract" address,
               OBus_address.arg "tmpdir" address) with
          | Some path, None, None ->
              make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX(path))
          | None, Some abst, None ->
              make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ abst))
          | None, None, Some tmpd ->
              raise_lwt (Invalid_argument "OBus_transport.connect: unix tmpdir can only be used as a listening address")
          | _ ->
              raise_lwt (Invalid_argument "OBus_transport.connect: invalid unix address, must supply exactly one of 'path', 'abstract', 'tmpdir'")
      end
    | ("tcp" | "nonce-tcp") as name -> begin
        let host = match OBus_address.arg "host" address with
          | Some host -> host
          | None -> ""
        and port = match OBus_address.arg "port" address with
          | Some port -> port
          | None -> "0"
        in
        let opts = [AI_SOCKTYPE SOCK_STREAM] in
        let opts = match OBus_address.arg "family" address with
          | Some "ipv4" -> AI_FAMILY PF_INET :: opts
          | Some "ipv6" -> AI_FAMILY PF_INET6 :: opts
          | Some family -> Printf.ksprintf invalid_arg "OBus_transport.connect: unknown address family '%s'" family
          | None -> opts
        in
        Lwt_unix.getaddrinfo host port opts >>= function
          | [] ->
              fail
                (Failure
                   (Printf.sprintf
                      "OBus_transport.connect: no address info for host=%s port=%s%s"
                      host port
                      (match OBus_address.arg "family" address with
                         | None -> ""
                         | Some f -> " family=" ^ f)))
          | ai :: ais ->
              let make_socket =
                if name = "nonce-tcp" then
                  make_socket_nonce (OBus_address.arg "noncefile" address)
                else
                  make_socket
              in
              try_lwt
                make_socket ai.ai_family ai.ai_socktype ai.ai_addr
              with exn ->
                (* If the first connection failed, try with all the
                   other ones: *)
                let rec find = function
                  | [] ->
                      (* If all connection failed, raise the error for
                         the first address: *)
                      raise_lwt exn
                  | ai :: ais ->
                      try_lwt
                        make_socket ai.ai_family ai.ai_socktype ai.ai_addr
                      with exn ->
                        find ais
                in
                find ais
      end
    | "autolaunch" -> begin
        lwt addresses =
          lwt uuid = Lazy.force OBus_info.machine_uuid in
          lwt line =
            try_lwt
              Lwt_process.pread_line ("dbus-launch", [|"dbus-launch"; "--autolaunch"; OBus_uuid.to_string uuid; "--binary-syntax"|])
            with exn ->
              lwt () = Lwt_log.error_f ~section "autolaunch failed: %s" (Printexc.to_string exn) in
              raise_lwt exn
          in
          let line = try String.sub line 0 (String.index line '\000') with _ -> line in
          try_lwt
            return (OBus_address.of_string line)
          with OBus_address.Parse_failure(addr, pos, reason) as exn ->
            lwt () = Lwt_log.error_f ~section "autolaunch returned an invalid address %S, at position %d: %s" addr pos reason in
            raise_lwt exn
        in
        match addresses with
          | [] ->
              lwt () = Lwt_log.error_f ~section "'autolaunch' returned no addresses" in
              raise_lwt (Failure "'autolaunch' returned no addresses")
          | address :: rest ->
              try_lwt
                connect address
              with exn ->
                let rec find = function
                  | [] ->
                      raise_lwt exn
                  | address :: rest ->
                      try_lwt
                        connect address
                      with exn ->
                        find rest
                in
                find rest
      end

    | name ->
        raise_lwt (Failure ("unknown transport type: " ^ name))

let of_addresses ?switch ?(capabilities=OBus_auth.capabilities) ?mechanisms addresses =
  Lwt_switch.check switch;
  match addresses with
    | [] ->
        raise_lwt (Invalid_argument "OBus_transport.of_addresses: no address given")
    | addr :: rest ->
        (* Search an address for which connection succeed: *)
        lwt fd, domain =
          try_lwt
            connect addr
          with exn ->
            (* If the first try fails, try with the others: *)
            let rec find = function
              | [] ->
                  (* If they all fail, raise the first exception: *)
                  raise_lwt exn
              | addr :: rest ->
                  try_lwt
                    connect addr
                  with exn ->
                    find rest
            in
            find rest
        in
        let transport, stream = socket_and_auth_stream ~capabilities fd in
        (* Do authentication only once: *)
        try_lwt
          Lwt_unix.write fd "\x00" 0 1 >>= function
            | 0 ->
                raise_lwt (OBus_auth.Auth_failure "failed to send the initial null byte")
            | 1 ->
                lwt guid, capabilities =
                  OBus_auth.Client.authenticate
                    ~capabilities:(List.filter (function `Unix_fd -> domain = PF_UNIX) capabilities)
                    ?mechanisms
                    ~stream
                    ()
                in
                lwt () = Lwt_switch.add_hook_or_exec switch transport.shutdown in
                return (guid, transport)
            | n ->
                assert false
        with exn ->
          Lwt_unix.shutdown fd SHUTDOWN_ALL;
          lwt () = Lwt_unix.close fd in
          raise_lwt exn
