(*
 * oBus_server.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(server)"

open Unix
open Lwt.Infix

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* Type of a listener. A server have one or more listeners. Each
   listener listen for new clients on a givne address *)
type listener = {
  lst_fd : Lwt_unix.file_descr;
  lst_address : OBus_address.t;
  lst_guid : OBus_address.guid;
  lst_capabilities : OBus_auth.capability list;
}

(* Type of events received by a listener *)
type event =
  | Event_shutdown
      (* Event fired when the user shutdown the server, or when a
         listener fails. *)
  | Event_connection of Lwt_unix.file_descr * Unix.sockaddr
      (* A new client connects to the server *)

(* Type of a server *)
type t = {
  mutable srv_up : bool;
  (* The server state *)

  srv_addresses : OBus_address.t list;
  (* List of connecting addresses of the server *)

  srv_callback : (t -> OBus_transport.t -> unit);
  (* The callback function *)

  srv_abort_waiter : event Lwt.t;
  srv_abort_wakener : event Lwt.u;
  (* Sleeping thread which is wakeup with the value [Event_shutdown]
     when the server is shutdown *)

  srv_mechanisms : OBus_auth.Server.mechanism list option;
  (* List of mechanisms supported by this server *)

  srv_allow_anonymous : bool;
  (* Does the server allow anonymous clients ? *)

  srv_nonce : string;
  (* The server nonce, for the "tcp-nonce" transport *)

  srv_nonce_file : string;
  (* The file in which the nonce is stored *)

  mutable srv_loops : unit Lwt.t;
  (* [srv_loops] is the join of all listener's loops *)
}

(* +-----------------------------------------------------------------+
   | Accepting new connecctions                                      |
   +-----------------------------------------------------------------+ *)

(* Reads the nonce sent by the client before authentication. The nonce
   is composed of the first 16 bytes sent by the client. *)
let read_nonce fd =
  let nonce = Bytes.create 16 in
  let rec loop ofs len =
    Lwt_unix.read fd nonce ofs len >>= function
      | 0 ->
          Lwt.fail End_of_file
      | n ->
          if n = len then
            Lwt.return (Bytes.unsafe_to_string nonce)
          else
            loop (ofs + n) (len - n)
  in
  loop 0 16

(* Wait for a client to connects *)
let rec accept server listener =
  begin
    try%lwt
      let%lwt result = Lwt_unix.accept listener.lst_fd in
      Lwt.return (`Accept result)
    with Unix_error(err, _, _) ->
      let%lwt () =
        if server.srv_up then
          Lwt_log.error_f ~section "uncaught error: %s" (error_message err)
        else
          (* Ignore errors that happens after a shutdown *)
          Lwt.return ()
      in
      Lwt.return `Shutdown
  end >>= function
    | `Accept(fd, address) ->
        if OBus_address.name listener.lst_address = "nonce-tcp" then begin
          begin
            try%lwt
              let%lwt nonce = read_nonce fd in
              if nonce <> server.srv_nonce then begin
                let%lwt () = Lwt_log.notice_f ~section "client rejected because of invalid nonce" in
                Lwt.return `Drop
              end else
                Lwt.return `OK
            with
              | End_of_file ->
                  let%lwt () = Lwt_log.warning ~section "cannot read nonce from socket" in
                  Lwt.return `Drop
              | Unix.Unix_error(err, _, _) ->
                  let%lwt () = Lwt_log.warning_f ~section "cannot read nonce from socket: %s" (Unix.error_message err) in
                  Lwt.return `Drop
          end >>= function
            | `OK ->
                Lwt.return (Event_connection(fd, address))
            | `Drop ->
                let%lwt () =
                  try
                    Lwt_unix.shutdown fd SHUTDOWN_ALL;
                    Lwt_unix.close fd
                  with Unix.Unix_error(err, _, _) ->
                    Lwt_log.error_f ~section "cannot shutdown socket: %s" (Unix.error_message err)
                in
                accept server listener
        end else
          Lwt.return (Event_connection(fd, address))
    | `Shutdown ->
        Lwt.return Event_shutdown

(* +-----------------------------------------------------------------+
   | Listeners                                                       |
   +-----------------------------------------------------------------+ *)

(* Cleans up resources allocated for the given listenning address *)
let cleanup address =
  match OBus_address.name address with
    | "unix" -> begin
        match OBus_address.arg "path" address with
          | Some path -> begin
              (* Sockets in the file system must be removed manually *)
              try
                Lwt_unix.unlink path
              with Unix_error(err, _, _) ->
                Lwt_log.error_f ~section "cannot unlink '%s': %s" path (Unix.error_message err)
            end
          | None ->
              Lwt.return ()
      end
    | _ ->
        Lwt.return ()

let string_of_address = function
  | ADDR_UNIX path ->
      let len = String.length path in
      if len > 0 && path.[0] = '\x00' then
        Printf.sprintf "unix abstract path %S" (String.sub path 1 (len - 1))
      else
        Printf.sprintf "unix path %S" path
  | ADDR_INET(ia, port) ->
      Printf.sprintf "internet address %s:%d" (string_of_inet_addr ia) port

(* Handle new clients. This function never fails. *)
let handle_client server listener fd address =
  let shutdown = lazy(
    try%lwt
      Lwt_unix.shutdown fd SHUTDOWN_ALL;
      Lwt_unix.close fd
    with Unix.Unix_error(err, _, _) ->
      Lwt_log.error_f ~section "cannot shutdown socket: %s" (Unix.error_message err)
  ) in
  try%lwt
    let buf = Bytes.create 1 in
    Lwt_unix.read fd buf 0 1 >>= function
      | 0 ->
          Lwt.fail (OBus_auth.Auth_failure "did not receive the initial null byte")
      | 1 ->
          let user_id =
            try
              Some((Lwt_unix.get_credentials fd).Lwt_unix.cred_uid)
            with Unix.Unix_error(error, _, _) ->
              ignore (Lwt_log.info_f ~section "cannot read credential: %s" (Unix.error_message error));
              None
          in
          let%lwt user_id, capabilities =
            OBus_auth.Server.authenticate
              ~capabilities:listener.lst_capabilities
              ?mechanisms:server.srv_mechanisms
              ?user_id
              ~guid:listener.lst_guid
              ~stream:(OBus_auth.stream_of_fd fd)
              ()
          in
          if user_id = None && not server.srv_allow_anonymous then begin
            let%lwt () = Lwt_log.notice_f ~section "client from %s rejected because anonymous connections are not allowed" (string_of_address address) in
            Lazy.force shutdown
          end else begin
            try
              server.srv_callback server (OBus_transport.socket ~capabilities fd);
              Lwt.return ()
            with exn ->
              let%lwt () = Lwt_log.error ~section ~exn "server callback failed failed with" in
              Lazy.force shutdown
          end
      | _ ->
          assert false
  with exn ->
    let%lwt () =
      match exn with
        | OBus_auth.Auth_failure msg ->
            Lwt_log.notice_f ~section "authentication failure for client from %s: %s" (string_of_address address) msg
        | exn ->
            Lwt_log.error_f ~section ~exn "authentication for client from %s failed with" (string_of_address address)
    in
    Lazy.force shutdown

(* Accept clients until the server is shutdown, or an accept fails: *)
let rec lst_loop server listener =
  Lwt.pick [server.srv_abort_waiter; accept server listener] >>= function
    | Event_shutdown ->
        let%lwt () =
          try
            Lwt_unix.close listener.lst_fd
          with Unix_error(err, _, _) ->
            Lwt_log.error_f ~section "cannot close listenning socket: %s" (Unix.error_message err)
        in
        cleanup listener.lst_address

    | Event_connection(fd, address) ->
        (* Launch authentication and dispatching in parallel: *)
        ignore (handle_client server listener fd address);
        lst_loop server listener

(* +-----------------------------------------------------------------+
   | Address -> transport                                            |
   +-----------------------------------------------------------------+ *)

(* Tries to create a socket using the given parameters *)
let make_socket domain typ address =
  let fd = Lwt_unix.socket domain typ 0 in
  (try Lwt_unix.set_close_on_exec fd with _ -> ());
  try
    let%lwt () = Lwt_unix.bind fd address in
    Lwt_unix.listen fd 10;
    Lwt.return fd
  with Unix_error(err, _, _) as exn ->
    let%lwt () = Lwt_log.error_f ~section "failed to create listenning socket with %s: %s" (string_of_address address) (Unix.error_message err) in
    let%lwt () = Lwt_unix.close fd in
    Lwt.fail exn

let make_path path =
  make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX(path))

let make_abstract path =
  make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ path))

(* Takes a D-Bus listenning address and returns the list of [(fd,
   client-address)] it denotes *)
let fd_addr_list_of_address address = match OBus_address.name address with
  | "unix" -> begin
      match (OBus_address.arg "path" address,
             OBus_address.arg "abstract" address,
             OBus_address.arg "tmpdir" address) with
        | Some path, None, None ->
            let%lwt fd = make_path path in
            Lwt.return [(fd, address)]
        | None, Some abst, None ->
            let%lwt fd = make_abstract abst in
            Lwt.return [(fd, address)]
        | None, None, Some tmpd -> begin
            let path = Filename.concat tmpd ("obus-" ^ OBus_util.hex_encode (OBus_util.random_string 10)) in
            (* Try with abstract name first *)
            try%lwt
              let%lwt fd = make_abstract path in
              Lwt.return [(fd, OBus_address.make ~name:"unix" ~args:[("abstract", path)])]
            with exn ->
              (* And fallback to path in the filesystem *)
              let%lwt fd = make_path path in
              Lwt.return [(fd, OBus_address.make ~name:"unix" ~args:[("path", path)])]
          end
        | _ ->
            Lwt.fail (Invalid_argument "OBus_transport.connect: invalid unix address, must supply exactly one of 'path', 'abstract', 'tmpdir'")
    end

  | ("tcp" | "nonce-tcp") as name -> begin
      let port = match OBus_address.arg "port" address with
        | Some port -> port
        | None -> "0"
      and bind = match OBus_address.arg "bind" address with
        | Some bind -> bind
        | None -> match OBus_address.arg "host" address with
            | Some host -> host
            | None -> "*"
      in
      let opts = [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE] in
      let opts = match OBus_address.arg "family" address with
        | Some "ipv4" -> AI_FAMILY PF_INET :: opts
        | Some "ipv6" -> AI_FAMILY PF_INET6 :: opts
        | Some family -> Printf.ksprintf invalid_arg "OBus_server.make_server: unknown address family '%s'" family
        | None -> opts
      in
      let ais = getaddrinfo bind port opts in
      (* Remove duplicate address info: *)
      let module AI_set = Set.Make(struct type t  = addr_info let compare = compare end) in
      let ais = AI_set.elements (List.fold_left (fun set ai -> AI_set.add ai set) AI_set.empty ais) in
      match ais with
        | [] ->
            Printf.ksprintf
              failwith
              "OBus_transport.make_server: no address info for bind=%s port=%s%s"
              bind port
              (match OBus_address.arg "family" address with
                 | None -> ""
                 | Some f -> " family=" ^ f)
        | ais ->
            let%lwt results = Lwt_list.map_p
              (fun ai ->
                 try%lwt
                   let%lwt fd = make_socket ai.ai_family ai.ai_socktype ai.ai_addr in
                   match getsockname (Lwt_unix.unix_file_descr fd) with
                     | ADDR_UNIX path ->
                         assert false
                     | ADDR_INET(host, port) ->
                         Lwt.return (`Success(fd, OBus_address.make ~name ~args:[("host", string_of_inet_addr host);
                                                                             ("port", string_of_int port);
                                                                             ("family",
                                                                              match ai.ai_family with
                                                                                | PF_UNIX -> assert false
                                                                                | PF_INET -> "ipv4"
                                                                                | PF_INET6 -> "ipv6")]))
                 with exn ->
                   Lwt.return (`Failure exn))
              ais
            in
            let fd_addr_list =
              OBus_util.filter_map
                (function
                   | `Success x -> Some x
                   | `Failure _ -> None)
                results
            in
            if fd_addr_list = [] then
              (* If no fds have been created, raises the first failure: *)
              match OBus_util.find_map (function `Failure e -> Some e | `Success _ -> None) results with
                | Some exn -> Lwt.fail exn
                | None -> assert false
            else
              Lwt.return fd_addr_list
    end

  | "autolaunch" ->
      Lwt.fail (Failure "OBus_server.make_server: autolaunch can not be used as a listenning address")

  | name ->
      Lwt.fail (Failure ("OBus_server.make_server: unknown transport type: " ^ name))

(* +-----------------------------------------------------------------+
   | Servers                                                         |
   +-----------------------------------------------------------------+ *)

let addresses server = server.srv_addresses

let shutdown server =
  if server.srv_up then begin
    server.srv_up <- false;
    Lwt.wakeup server.srv_abort_wakener Event_shutdown;
    let%lwt () =
      if server.srv_nonce_file <> "" then begin
        try
          Lwt_unix.unlink server.srv_nonce_file
        with Unix_error(err, _, _) ->
          Lwt_log.error_f ~section "cannot unlink '%s': %s" server.srv_nonce_file (Unix.error_message err)
      end else
        Lwt.return ()
    in
    (* Wait for all listenners to exit: *)
    server.srv_loops
  end else
    server.srv_loops

let default_address = OBus_address.make ~name:"unix" ~args:[("tmpdir", Filename.get_temp_dir_name ())]

let make_lowlevel ?switch ?(capabilities=OBus_auth.capabilities) ?mechanisms ?(addresses=[default_address]) ?(allow_anonymous=false) callback =
  Lwt_switch.check switch;
  match addresses with
    | [] ->
        Lwt.fail (Invalid_argument "OBus_server.make: no addresses given")

    | addresses ->
        (* Construct the list of all listening fds for each
           address: *)
        let%lwt result_by_address =
          Lwt_list.map_p
            (fun address ->
               try%lwt
                 let%lwt x = fd_addr_list_of_address address in
                 Lwt.return (`Success x)
               with e ->
                 Lwt.return (`Failure e))
            addresses
        in

        (* Close all listening file descriptors and fail: *)
        let abort exn =
          let%lwt () =
            Lwt_list.iter_p
              (function
                 | `Success fd_addr_list ->
                     Lwt_list.iter_p
                       (fun (fd, address) ->
                          try%lwt
                            let%lwt () = Lwt_unix.close fd in
                            cleanup address
                          with Unix_error(err, _, _) ->
                            Lwt_log.error_f ~section "failed to close listenning file descriptor: %s" (Unix.error_message err))
                       fd_addr_list
                 | `Failure e ->
                     Lwt.return ())
              result_by_address
          in
          Lwt.fail exn
        in

        match OBus_util.find_map (function `Success _ -> None | `Failure e -> Some e) result_by_address with
          | Some exn ->
              abort exn

          | None ->
              let%lwt nonce, nonce_file =
                if List.exists (fun addr -> OBus_address.name addr = "nonce-tcp") addresses then begin
                  let nonce = OBus_util.random_string 16 in
                  let file_name = Filename.concat (Filename.get_temp_dir_name ()) ("obus-" ^ OBus_util.hex_encode (OBus_util.random_string 10)) in
                  try%lwt
                    let%lwt () = Lwt_io.with_file ~mode:Lwt_io.output file_name (fun oc -> Lwt_io.write oc nonce) in
                    Lwt.return (nonce, file_name)
                  with Unix.Unix_error(err, _, _) ->
                    abort (Failure(Printf.sprintf "cannot create nonce file '%s': %s" file_name (Unix.error_message err)))
                end else
                  Lwt.return ("", "")
              in

              let successes =
                List.map
                  (function
                     | `Failure _ -> assert false
                     | `Success x -> x)
                  result_by_address
              in

              let guids = List.map (fun _ -> OBus_uuid.generate ()) successes in

              let successes =
                List.map2
                  (fun fd_addr_list guid ->
                     List.map
                       (fun (fd, addr) ->
                          let args = ("guid", OBus_uuid.to_string guid) :: OBus_address.args addr in
                          let args =
                            if OBus_address.name addr = "nonce-tcp" then
                              ("noncefile", nonce_file) :: args
                            else
                              args
                          in
                          (fd, { addr with OBus_address.args = args }))
                       fd_addr_list)
                  successes guids
              in

              let listeners = List.flatten
                (List.map2
                   (fun fd_addr_list guid ->
                      List.map
                        (fun (fd, address) -> {
                           lst_fd = fd;
                           lst_address = address;
                           lst_capabilities = (List.filter
                                                 (fun `Unix_fd ->
                                                    match (OBus_address.arg "path" address,
                                                           OBus_address.arg "abstract" address) with
                                                      | None, None -> false
                                                      | _ -> true)
                                                 capabilities);
                           lst_guid = guid;
                         })
                        fd_addr_list)
                   successes guids)
              in

              let abort_waiter, abort_wakener = Lwt.wait () in
              let server = {
                srv_up = true;
                srv_addresses = List.map snd (List.flatten successes);
                srv_callback = callback;
                srv_abort_waiter = abort_waiter;
                srv_abort_wakener = abort_wakener;
                srv_mechanisms = mechanisms;
                srv_allow_anonymous = allow_anonymous;
                srv_nonce = nonce;
                srv_nonce_file = nonce_file;
                srv_loops = Lwt.return ();
              } in
              server.srv_loops <- Lwt.join (List.map (fun listener -> lst_loop server listener) listeners);

              let%lwt () = Lwt_switch.add_hook_or_exec switch (fun () -> shutdown server) in
              Lwt.return server

let make ?switch ?capabilities ?mechanisms ?addresses ?allow_anonymous callback =
  make_lowlevel ?switch ?capabilities ?mechanisms ?addresses ?allow_anonymous
    (fun server transport -> callback server (OBus_connection.of_transport ~up:false transport))
