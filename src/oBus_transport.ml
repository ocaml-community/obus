(*
 * oBus_transport.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "obus(transport)" end)

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

let make ~recv ~send ?(capabilities=[]) ~shutdown () = {
  recv = recv;
  send = send;
  capabilities = capabilities;
  shutdown = shutdown;
}

let recv t = t.recv ()
let send t message = t.send message
let capabilities t = t.capabilities
let shutdown t = t.shutdown ()

(* +-----------------------------------------------------------------+
   | Socket transport                                                |
   +-----------------------------------------------------------------+ *)

let socket ?(capabilities=[]) fd =
  if List.mem `Unix_fd capabilities then
    let reader = OBus_wire.reader fd
    and writer = OBus_wire.writer fd in
    { recv = (fun _ -> OBus_wire.read_message_with_fds reader);
      send = (fun msg -> OBus_wire.write_message_with_fds writer msg);
      capabilities = capabilities;
      shutdown = (fun _ ->
                    lwt () = OBus_wire.close_reader reader <&> OBus_wire.close_writer writer in
                    Lwt_unix.shutdown fd SHUTDOWN_ALL;
                    Lwt_unix.close fd;
                    return ()) }
  else
    let ic = Lwt_io.make ~mode:Lwt_io.input (Lwt_unix.read fd)
    and oc = Lwt_io.make ~mode:Lwt_io.output (Lwt_unix.write fd) in
    { recv = (fun _ -> OBus_wire.read_message ic);
      send = (fun msg -> OBus_wire.write_message oc msg);
      capabilities = capabilities;
      shutdown = (fun _ ->
                    lwt () = Lwt_io.close ic <&> Lwt_io.close oc in
                    Lwt_unix.shutdown fd SHUTDOWN_ALL;
                    Lwt_unix.close fd;
                    return ()) }

(* +-----------------------------------------------------------------+
   | Loopback transport                                              |
   +-----------------------------------------------------------------+ *)

(* Note: since file descriptors will be closed by the connection
   dispatcher, we need to duplicate all file descriptors. *)

open OBus_value

let tbasic_contains_fds = function
  | Tunix_fd -> true
  | _ -> false

let rec tsingle_contains_fds = function
  | Tbasic t -> tbasic_contains_fds t
  | Tarray t -> tsingle_contains_fds t
  | Tdict(tk, tv) -> tbasic_contains_fds tk && tsingle_contains_fds tv
  | Tstructure t -> tsequence_contains_fds t
  | Tvariant -> false

and tsequence_contains_fds t = List.exists tsingle_contains_fds t

let basic_dup = function
  | Unix_fd fd -> Unix_fd(Unix.dup fd)
  | x -> x

let rec single_dup = function
  | Basic x ->
      basic (basic_dup x)
  | Array(t, l) as v ->
      if tsingle_contains_fds t then
        array t (List.map single_dup l)
      else
        v
  | Dict(tk, tv, l) as v ->
      if tbasic_contains_fds tk || tsingle_contains_fds tv then
        dict tk tv (List.map (fun (k, v) -> (basic_dup k, single_dup v)) l)
      else
        v
  | Structure l ->
      structure (sequence_dup l)
  | Byte_array _ as v ->
      v
  | Variant x ->
      variant (single_dup x)

and sequence_dup l =
  List.map single_dup l

let loopback () =
  let mvar = Lwt_mvar.create_empty () in
  { recv = (fun _ -> Lwt_mvar.take mvar);
    send = (fun m -> Lwt_mvar.put mvar { m with OBus_message.body = sequence_dup (OBus_message.body m) });
    capabilities = [`Unix_fd];
    shutdown = return }

(* +-----------------------------------------------------------------+
   | Addresses -> transport                                          |
   +-----------------------------------------------------------------+ *)

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  (try Unix.set_close_on_exec (Lwt_unix.unix_file_descr fd) with _ -> ());
  try_lwt
    lwt () = Lwt_unix.connect fd addr in
    return fd
  with exn ->
    Lwt_unix.close fd;
    fail exn

let string_of_socket_params domain typ addr =
  Printf.sprintf "domain=%s typ=%s addr=%s"
    (match domain with
       | PF_UNIX -> "unix"
       | PF_INET -> "inet"
       | PF_INET6 -> "inet6")
    (match typ with
       | SOCK_STREAM -> "stream"
       | SOCK_DGRAM -> "dgram"
       | SOCK_RAW -> "raw"
       | SOCK_SEQPACKET -> "seqpacket")
    (match addr with
       | ADDR_UNIX path -> sprintf "unix(%s)" path
       | ADDR_INET(addr, port) -> sprintf "inet(%s,%d)" (string_of_inet_addr addr) port)

let of_addresses ?(capabilities=[]) ?mechanisms addresses =
  let rec try_one domain typ addr fallback x =
    try_lwt
      lwt fd = make_socket domain typ addr in
      Lwt_unix.write fd "\x00" 0 1 >>= function
        | 0 ->
            fail (OBus_auth.Auth_failure "failed to send the initial null byte")
        | 1 ->
            lwt guid, capabilities =
              OBus_auth.Client.authenticate
                ~capabilities:(List.filter (function `Unix_fd -> domain = PF_UNIX) capabilities)
                ?mechanisms
                ~stream:(OBus_auth.stream_of_fd fd)
                ()
            in
            return (guid, socket ~capabilities fd)
        | n ->
            assert false
    with
      | OBus_auth.Auth_failure msg ->
          lwt () =
            Log.error_f "authentication failed for address: domain=%s: %s"
              (string_of_socket_params domain typ addr) msg
          in
          fallback x
      | exn ->
          lwt () =
            Log.exn_f exn "transport creation failed for address: %s"
              (string_of_socket_params domain typ addr)
          in
          fallback x
  in
  let rec aux = function
    | [] ->
        fail (Failure "no working D-Bus address found")
    | { OBus_address.address = address } :: rest ->
        match address with
          | Unix_path path ->
              try_one PF_UNIX SOCK_STREAM (ADDR_UNIX(path))
                aux rest

          | Unix_abstract path ->
              try_one PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ path))
                aux rest

          | Unix_tmpdir _ ->
              lwt () = Log.error "unix tmpdir can only be used as a listening address" in
              aux rest

          | Tcp { tcp_host = host; tcp_port = port; tcp_family = family } ->
              let opts = [AI_SOCKTYPE SOCK_STREAM] in
              let opts = match family with
                | Some `Ipv4 -> AI_FAMILY PF_INET :: opts
                | Some `Ipv6 -> AI_FAMILY PF_INET6 :: opts
                | None -> opts in
              let rec try_all = function
                | [] -> aux rest
                | ai :: ais -> try_one ai.ai_family ai.ai_socktype ai.ai_addr try_all ais
              in
              try_all (getaddrinfo host port opts)

          | Autolaunch ->
              lwt addresses =
                try_lwt
                  lwt uuid = Lazy.force OBus_info.machine_uuid in
                  lwt line =
                    try_lwt
                      Lwt_process.pread_line ("dbus-launch",
                                              [|"dbus-launch"; "--autolaunch"; OBus_uuid.to_string uuid; "--binary-syntax"|])
                    with exn ->
                      lwt () = Log.error_f "autolaunch failed: %s" (OBus_util.string_of_exn exn) in
                      fail exn
                  in
                  let line = try String.sub line 0 (String.index line '\000') with _ -> line in
                  return (OBus_address.of_string line)
                with exn ->
                  return []
              in
              aux (addresses @ rest)

          | _ ->
              aux rest
  in
  aux addresses
