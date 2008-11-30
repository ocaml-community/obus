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
open OBus_address

type t = {
  listeners : (Lwt_unix.file_descr * OBus_address.guid) list;
  addresses : OBus_address.t list;
  mechanisms : OBus_auth.server_mechanism list option;
  on_connection : (OBus_lowlevel.transport -> unit);
}

exception Shutdown

let socket fd (ic, oc) = OBus_lowlevel.make_transport
  ~recv:(fun _ -> OBus_lowlevel.get_message ic)
  ~send:(fun msg ->
           perform
             OBus_lowlevel.put_message oc msg;
             Lwt_chan.flush oc)
  ~shutdown:(fun _ ->
               Lwt_unix.shutdown fd SHUTDOWN_ALL;
               Lwt_unix.close fd)

let rec loop server (listen_fd, guid) =
  catch
    (fun _ -> perform
       (fd, addr) <-- Lwt_unix.accept listen_fd;
       let chans = (Lwt_chan.in_channel_of_descr fd,
                    Lwt_chan.out_channel_of_descr fd) in
       catch
         (fun _ -> OBus_auth.server_authenticate ?mechanisms:server.mechanisms guid chans)
         (fun exn ->
            Log.log "authentication failure for client from %a"
              (fun oc -> function
                 | ADDR_UNIX path -> output_string oc path
                 | ADDR_INET(ia, port) -> Printf.fprintf oc "%s:%d" (string_of_inet_addr ia) port) addr;
            return ());
       let _ =
         try
           server.on_connection (socket fd chans)
         with
             exn ->
               Log.failure exn "on_connection failed with"
       in
       return true)
    (function
       | Shutdown ->
           return false
       | exn ->
           Log.error "uncaught error: %s" (Printexc.to_string exn);
           return false)
  >>= function
    | true -> loop server (listen_fd, guid)
    | false -> return ()

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

let make_lowlevel ?mechanisms ?(addresses=[Unix_tmpdir Filename.temp_dir_name]) on_connection =
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
               listeners = List.flatten (List.map2 (fun (fds, addr) guid ->
                                                      List.map (fun fd -> (fd, guid)) fds) l guids);
               addresses = List.map2 (fun (fds, addr) guid -> (addr, Some guid)) l guids;
               mechanisms = mechanisms;
               on_connection = on_connection;
             } in

             (* Launch waiting loops *)
             List.iter (fun listener -> ignore (loop server listener)) server.listeners;

             return server
           end)

let make ?mechanisms ?addresses on_connection = make_lowlevel ?mechanisms ?addresses
  (fun transport -> on_connection (OBus_connection.of_transport ~up:false transport))

let addresses server = server.addresses

let shutdown server =
  List.iter (fun (fd, guid) -> Lwt_unix.abort fd Shutdown) server.listeners
