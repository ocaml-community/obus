(*
 * oBus_server.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "obus(server)" end)

open Unix
open Lwt
open OBus_private

class type t = object
  method event : OBus_connection.t React.event
  method addresses : OBus_address.t list
  method shutdown : unit Lwt.t
end

class type lowlevel = object
  method event : OBus_transport.t React.event
  method addresses : OBus_address.t list
  method shutdown : unit Lwt.t
end

type event =
  | Event_shutdown
  | Event_connection of Lwt_unix.file_descr * Unix.sockaddr

type listener = {
  listen_fd : Lwt_unix.file_descr;
  listen_address : OBus_address.t;
  listen_guid : OBus_address.guid;
  listen_capabilities : OBus_auth.capability list;
}

type server = {
  mutable server_up : bool;
  server_abort : event Lwt.t;
  server_mechanisms : OBus_auth.Server.mechanism list option;
  server_push : OBus_transport.t -> unit;
  server_allow_anonymous : bool;
}

let accept server listen =
  try_lwt
    lwt fd, address = Lwt_unix.accept listen.listen_fd in
    (try Unix.set_close_on_exec (Lwt_unix.unix_file_descr fd) with _ -> ());
    return (Event_connection(fd, address))
  with Unix_error(err, _, _) ->
    lwt () =
      if server.server_up then
        Log.error_f "uncaught error: %s" (error_message err)
      else
        return ()
    in
    return Event_shutdown

let string_of_address = function
  | ADDR_UNIX path ->
      path
  | ADDR_INET(ia, port) ->
      Printf.sprintf "%s:%d" (string_of_inet_addr ia) port

let rec listen_loop server listen =
  choose [server.server_abort; accept server listen] >>= function
    | Event_shutdown ->
        lwt () =
          try
            Lwt_unix.close listen.listen_fd;
            return ()
          with Unix_error(err, _, _) ->
            Log.error_f "cannot close listenning socket: %s" (Unix.error_message err)
        in
        lwt () =
          match OBus_address.name listen.listen_address with
            | "unix" -> begin
                match OBus_address.arg "path" listen.listen_address with
                  | Some path when String.length path >= 1 && path.[0] <> '\x00' -> begin
                      try
                        Unix.unlink path;
                        return ()
                      with Unix_error(err, _, _) ->
                        Log.error_f "cannot unlink %S: %s" path (Unix.error_message err)
                    end
                  | _ ->
                      return ()
              end
            | _ ->
                return ()
        in
        return ()

    | Event_connection(fd, address) ->
        lwt () =
          try_lwt
            let buf = String.create 1 in
            Lwt_unix.read fd buf 0 1 >>= function
              | 0 ->
                  fail (OBus_auth.Auth_failure "did not receive the initial null byte")
              | 1 ->
                  let user_id =
                    try
                      Some((Lwt_unix.get_credentials fd).Lwt_unix.cred_uid)
                    with Unix.Unix_error(error, _, _) ->
                      ignore (Log.info_f "cannot read credential: %s" (Unix.error_message error));
                      None
                  in
                  lwt user_id, capabilities =
                    OBus_auth.Server.authenticate
                      ~capabilities:listen.listen_capabilities
                      ?mechanisms:server.server_mechanisms
                      ?user_id
                      ~guid:listen.listen_guid
                      ~stream:(OBus_auth.stream_of_fd fd)
                      ()
                  in
                  if user_id = None && not server.server_allow_anonymous then begin
                    ignore (Log.info_f "client from %s rejected because anonymous connection are not allowed" (string_of_address address));
                    try
                      Lwt_unix.shutdown fd SHUTDOWN_ALL;
                      Lwt_unix.close fd
                    with exn ->
                      ignore (Log.exn exn "shutdown socket failed with")
                  end else begin
                    try
                      server.server_push (OBus_transport.socket ~capabilities fd)
                    with exn ->
                      ignore (Log.exn exn "failed to push new transport with")
                  end;
                  return ()
              | _ ->
                  assert false
          with
            | OBus_auth.Auth_failure msg ->
                Log.info_f "authentication failure for client from %s: %s" (string_of_address address) msg
            | exn ->
                Log.exn_f exn "authentication for client from %s failed with" (string_of_address address)
        in
        listen_loop server listen

let make_socket domain typ address =
  let fd = Lwt_unix.socket domain typ 0 in
  (try Unix.set_close_on_exec (Lwt_unix.unix_file_descr fd) with _ -> ());
  try
    Lwt_unix.bind fd address;
    Lwt_unix.listen fd 10;
    return fd
  with Unix_error(err, _, _) as exn ->
    lwt () =
      Log.error_f "failed to create listenning socket with %s: %s"
        (match address with
           | ADDR_UNIX path ->
               let len = String.length path in
               if len > 0 && path.[0] = '\x00' then
                 Printf.sprintf "unix abstract path %S" (String.sub path 1 (len - 1))
               else
                 Printf.sprintf "unix path %S" path
           | ADDR_INET(ia, port) ->
               Printf.sprintf "address %s:%d" (string_of_inet_addr ia) port)
        (Unix.error_message err)
    in
    Lwt_unix.close fd;
    fail exn

let make_path path =
  make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX(path))

let make_abstract path =
  make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ path))

let fds_of_address address = match OBus_address.name address with
  | "unix" -> begin
      match (OBus_address.arg "path" address,
             OBus_address.arg "abstract" address,
             OBus_address.arg "tmpdir" address) with
        | Some path, None, None ->
            lwt fd = make_path path in
            return ([fd], address)
        | None, Some abst, None ->
            lwt fd = make_abstract abst in
            return ([fd], address)
        | None, None, Some tmpd -> begin
            let path = Filename.concat tmpd ("obus-" ^ OBus_util.hex_encode (OBus_util.random_string 10)) in
            (* Try with abstract name first *)
            try_lwt
              lwt fd = make_abstract path in
              return ([fd], OBus_address.make ~name:"unix" ~args:[("abstract", path)])
            with exn ->
              (* And fallback to path in the filesystem *)
              lwt fd = make_path path in
              return ([fd], OBus_address.make ~name:"unix" ~args:[("path", path)])
          end
        | _ ->
            fail (Invalid_argument "OBus_transport.connect: invalid unix address, must supply exactly one of 'path', 'abstract', 'tmpdir'")
    end

  | "tcp" -> begin
      let port = match OBus_address.arg "port" address with
        | Some port -> port
        | None -> "0"
      and bind = match OBus_address.arg "bind" address with
        | Some bind -> bind
        | None -> "*"
      in
      let opts = [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE] in
      let opts = match OBus_address.arg "family" address with
        | Some "ipv4" -> AI_FAMILY PF_INET :: opts
        | Some "ipv6" -> AI_FAMILY PF_INET6 :: opts
        | Some family -> Printf.ksprintf invalid_arg "OBus_server.make_server: unknown address family '%s'" family
        | None -> opts
      in
      match getaddrinfo bind port opts with
        | [] ->
            Printf.ksprintf
              failwith
              "OBus_transport.make_server: no address info for bind=%s port=%s%s"
              bind port
              (match OBus_address.arg "family" address with
                 | None -> ""
                 | Some f -> " family=" ^ f)
        | ais ->
            lwt results = Lwt_list.fold_left_s
              (fun acc ai ->
                 try_lwt
                   lwt fd = make_socket ai.ai_family ai.ai_socktype ai.ai_addr in
                   return (`Success fd :: acc)
                 with exn ->
                   return (`Failure exn :: acc))
              [] ais
            in
            let fds =
              OBus_util.filter_map
                (function
                   | `Success fd -> Some fd
                   | `Failure _ -> None)
                results
            in
            if fds = [] then
              (* If no fds have been created, raires the first failure: *)
              match OBus_util.find_map (function `Failure e -> Some e | `Success _ -> None) results with
                | Some exn -> fail exn
                | None -> assert false
            else
              return (fds, address)
    end

  | "autolaunch" ->
      fail (Failure "OBus_server.make_server: autolaunch can not be used as a listenning address")

  | name ->
      fail (Failure ("OBus_server.make_server: unknown transport type: " ^ name))

let default_address = OBus_address.make ~name:"unix" ~args:[("tmpdir", Filename.temp_dir_name)]

let make_server ?(capabilities=OBus_auth.capabilities) ?mechanisms ?(addresses=[default_address]) ?(allow_anonymous=false) () =
  match addresses with
    | [] ->
        fail (Invalid_argument "OBus_server.make: no addresses given")
    | addresses ->
        (* Construct the list of all listening fds for each
           address: *)
        lwt result_by_address = Lwt_list.fold_left_s
          (fun acc address ->
             try_lwt
               lwt x = fds_of_address address in
               return (`Success x :: acc)
             with e ->
               return (`Failure e :: acc))
          [] addresses
        in

        (* Keeps only success: *)
        let successes =
          OBus_util.filter_map
            (function
               | `Failure _ | `Success([], _) -> None
               | `Success(fds, address) -> Some(fds, address))
            result_by_address
        in

        (* Fails if no listening file descriptor have been created: *)
        if successes = [] then
          match OBus_util.find_map (function `Failure exn -> Some exn | _ -> None) result_by_address with
            | Some exn ->
                (* Fails with the first error, if any *)
                fail exn
            | None ->
                (* Otherwise just returns a Failure *)
                fail (Failure "OBus_server.make_server: unable to listening on any address")

        else begin
          let guids = List.map (fun _ -> OBus_uuid.generate ()) successes
          and event, push = React.E.create ()
          and abort_waiter, abort_wakener = Lwt.wait () in

          let successes =
            List.map2
              (fun (fds, address) guid ->
                 (fds, {
                    OBus_address.name = OBus_address.name address;
                    OBus_address.args = ("guid", OBus_uuid.to_string guid) :: OBus_address.args address;
                  }))
              successes guids
          in

          let listeners = List.flatten
            (List.map2
               (fun (fds, address) guid ->
                  List.map
                    (fun fd -> { listen_fd = fd;
                                 listen_address = address;
                                 listen_capabilities = (List.filter
                                                          (fun `Unix_fd ->
                                                             match (OBus_address.arg "path" address,
                                                                    OBus_address.arg "abstract" address) with
                                                               | None, None -> false
                                                               | _ -> true)
                                                          capabilities);
                                 listen_guid = guid })
                    fds)
               successes guids)
          and listener_threads = ref [] in

          let server = {
            server_up = true;
            server_abort = abort_waiter;
            server_mechanisms = mechanisms;
            server_push = push;
            server_allow_anonymous = allow_anonymous;
          } in

          let rec shutdown = lazy(
            if server.server_up then begin
              server.server_up <- false;
              wakeup abort_wakener Event_shutdown
            end;
            (* Wait for all listenners to exit: *)
            Lwt.join !listener_threads
          ) in

          (* Launch waiting loops. Yield so the user have the time to
             bind the event before the first connection: *)
          List.iter (fun listen -> listener_threads := (Lwt_unix.yield () >> listen_loop server listen) :: !listener_threads) listeners;

          return (event, (List.map snd successes), shutdown)
        end

let make_lowlevel ?capabilities ?mechanisms ?addresses ?allow_anonymous () =
  lwt event, addresses, shutdown = make_server ?capabilities ?mechanisms ?addresses ?allow_anonymous () in
  return (object
            method event = event
            method addresses = addresses
            method shutdown = Lazy.force shutdown
          end)

let make ?capabilities ?mechanisms ?addresses ?allow_anonymous () =
  lwt event, addresses, shutdown = make_server ?capabilities ?mechanisms ?addresses ?allow_anonymous () in
  let event = React.E.map (OBus_connection.of_transport ~up:false) event in
  return (object
            method event = event
            method addresses = addresses
            method shutdown = Lazy.force shutdown
          end)
