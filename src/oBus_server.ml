(*
 * oBus_server.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Log = Log.Make(struct let section = "server" end)

open Unix
open Lwt
open OBus_internals
open OBus_address

type event =
  | Event_shutdown
  | Event_connection of Lwt_unix.file_descr * Unix.sockaddr

type listener = {
  listen_fd : Lwt_unix.file_descr;
  listen_address : OBus_address.t;
  listen_guid : OBus_address.guid;
  mutable listen_thread : unit Lwt.t;
}

type t = {
  mutable server_up : bool;
  server_abort : event Lwt.t;
  server_listeners : listener list;
  server_addresses : OBus_address.t list;
  server_mechanisms : OBus_auth.server_mechanism list option;
  server_on_connection : OBus_lowlevel.transport callback;
  mutable server_exit_hook : (unit -> unit Lwt.t) MSet.node option;
}

let socket fd chans =
  let tr = OBus_lowlevel.transport_of_channels chans in
  { tr with
      OBus_lowlevel.shutdown = fun _ ->
        Lwt.finalize tr.OBus_lowlevel.shutdown
          (fun _ ->
             Lwt_unix.shutdown fd SHUTDOWN_ALL;
             Lwt_unix.close fd;
             Lwt.return ()) }

let accept server listen =
  catch
    (fun _ ->
       perform
         (fd, addr) <-- Lwt_unix.accept listen.listen_fd;
         return (Event_connection(fd, addr)))
    (fun exn ->
       if server.server_up then
         Log.error "uncaught error: %s" (Util.string_of_exn exn);
       return Event_shutdown)

let rec listen_loop server listen =
  choose [server.server_abort; accept server listen] >>= function
    | Event_shutdown ->
        begin
          try
            Lwt_unix.close listen.listen_fd;
            match listen.listen_address with
              | (Unix_path p, _) ->
                  Unix.unlink p
              | _ ->
                  ()
          with
              _ -> ();
        end;
        return ()

    | Event_connection(fd, addr) ->
        let chans = (OBus_lowlevel.make_ichan (fun str ofs len -> Lwt_unix.read fd str ofs len),
                     OBus_lowlevel.make_ochan (fun str ofs len -> Lwt_unix.write fd str ofs len)) in
        catch
          (fun _ ->
             OBus_auth.server_authenticate
               ?mechanisms:server.server_mechanisms
               listen.listen_guid
               (OBus_lowlevel.auth_stream_of_channels chans))
          (fun exn ->
             Log.log "authentication failure for client from %a"
               (fun oc -> function
                  | ADDR_UNIX path -> output_string oc path
                  | ADDR_INET(ia, port) -> Printf.fprintf oc "%s:%d" (string_of_inet_addr ia) port) addr;
             return ())
        >>= fun _ ->
          let _ = callback_apply "on_connection" server.server_on_connection (socket fd chans) in
          listen_loop server listen

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  try
    Lwt_unix.bind fd addr;
    Lwt_unix.listen fd 10;
    return fd
  with exn -> Lwt_unix.close fd; fail exn

let make_path path =
  make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX(path))

let make_abstract path =
  make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ path))

let uniq addr t = t >>= fun fd -> return ([fd], addr)

let fds_of_address addr = match addr with
  | Unix_path path -> uniq addr (make_path path)
  | Unix_abstract path -> uniq addr (make_abstract path)
  | Unix_tmpdir dir ->
      let path = Filename.concat dir ("obus-" ^ Util.hex_encode (Util.random_string 10)) in
      (* Try with abstract name first *)
      catch
        (fun _ -> uniq (Unix_abstract path) (make_abstract path))
        (fun exn ->
           (* And fallback to path in the filesystem *)
           Log.debug "failed to create listening socket with abstract path name %s: %s" path (Util.string_of_exn exn);
           uniq (Unix_path path) (make_path path))

  | Tcp { tcp_bind = bind_addr; tcp_port = port; tcp_family = family } ->
      let opts = [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE] in
      let opts = match family with
        | Some `Ipv4 -> AI_FAMILY PF_INET :: opts
        | Some `Ipv6 -> AI_FAMILY PF_INET6 :: opts
        | None -> opts in
      (perform
         fds <-- Lwt_util.fold_left
           (fun fds ai ->
              catch
                (fun _ -> perform
                   fd <-- make_socket ai.ai_family ai.ai_socktype ai.ai_addr;
                   return (fd :: fds))
                (fun exn ->
                   (* Close all previously opened file descriptor *)
                   List.iter (fun fd -> try Lwt_unix.close fd with _ -> ()) fds;
                   fail exn)) [] (getaddrinfo bind_addr port opts);
         return (fds, addr))

  | Autolaunch ->
      fail (Failure "autolaunch can not be used as a listenning address")

  | Unknown(name, params) ->
      fail (Failure ("listening on " ^ name ^ " addresses is not implemented"))

let shutdown server =
  begin match server.server_exit_hook with
    | Some n ->
        server.server_exit_hook <- None;
        MSet.remove n
    | None ->
        ()
  end;
  if server.server_up then begin
    server.server_up <- false;
    wakeup server.server_abort Event_shutdown
  end;
  Lwt_util.iter (fun listen -> listen.listen_thread) server.server_listeners

let make_lowlevel ?mechanisms ?(addresses=[Unix_tmpdir Filename.temp_dir_name]) ?(serial=false) on_connection =
  match addresses with
    | [] -> fail (Invalid_argument "OBus_server.make: no addresses given")
    | addresses ->
        (perform
           l <-- Lwt_util.fold_left
             (fun acc address ->
                catch
                  (fun _ -> perform
                     x <-- fds_of_address address;
                     return (x :: acc))
                  (fun exn ->
                     (* Close all previously opened fds *)
                     List.iter (fun (fds, addr) ->
                                  List.iter (fun fd -> try Lwt_unix.close fd with _ -> ()) fds) acc;
                     fail exn)) [] addresses;

           (* Fail if no listening file descriptor has been created *)
           if List.for_all (fun (fds, addr) -> fds = []) l then
             fail (Failure "unable to listening on any address")

           else begin
             let guids = List.map (fun _ -> OBus_uuid.generate ()) l in

             let server = {
               server_up = true;
               server_abort = wait ();
               server_listeners = List.flatten
                 (List.map2
                    (fun (fds, addr) guid ->
                       List.map (fun fd -> { listen_fd = fd;
                                             listen_address = (addr, Some guid);
                                             listen_guid = guid;
                                             listen_thread = return () })
                         fds) l guids);
               server_addresses = List.map2 (fun (fds, addr) guid -> (addr, Some guid)) l guids;
               server_mechanisms = mechanisms;
               server_on_connection = make_callback serial on_connection;
               server_exit_hook = None;
             } in

             server.server_exit_hook <- Some(MSet.add exit_hooks (fun _ -> shutdown server));

             (* Launch waiting loops *)
             List.iter (fun listen -> listen.listen_thread <- listen_loop server listen) server.server_listeners;

             return server
           end)

let make ?mechanisms ?addresses ?serial on_connection = make_lowlevel ?mechanisms ?addresses ?serial
  (fun transport -> on_connection (OBus_connection.of_transport ~up:false transport))

let addresses server = server.server_addresses
