(*
 * oBus_auth.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Log = Log.Make(struct let section = "auth" end)

open Printf
open Lwt

exception Auth_failure of string
let auth_failure fmt = ksprintf (fun msg -> fail (Auth_failure msg)) fmt

(* Maximum line length, if line greated are received, authentication
   will fail *)
let max_line_length = 42 * 1024

(* Maximum number of reject, if a client is rejected more than that,
   authentication will fail *)
let max_reject = 42

type data = string

type client_mechanism_return =
  | Client_mech_continue of data
  | Client_mech_ok of data
  | Client_mech_error of string

type server_mechanism_return =
  | Server_mech_continue of data
  | Server_mech_ok
  | Server_mech_reject

type client_command =
  | Client_auth of (string * data option) option
  | Client_cancel
  | Client_begin
  | Client_data of data
  | Client_error of string

type server_command =
  | Server_rejected of string list
  | Server_ok of OBus_address.guid
  | Server_data of data
  | Server_error of string

class virtual client_mechanism_handler = object
  method virtual init : client_mechanism_return Lwt.t
  method data (chall : data) = return (Client_mech_error("no data expected for this mechanism"))
  method abort = ()
end

class virtual server_mechanism_handler = object
  method init = return (None : data option)
  method virtual data : data -> server_mechanism_return Lwt.t
  method abort = ()
end

type client_mechanism = string * (unit -> client_mechanism_handler)
type server_mechanism = string * (unit -> server_mechanism_handler)

let hex_encode = Util.hex_encode
let hex_decode str =
  try
    Util.hex_decode str
  with
    | Invalid_argument _ -> failwith "invalid hex-encoded data"

(***** Predefined client mechanisms *****)

class client_mech_external_handler = object
  inherit client_mechanism_handler
  method init = return (Client_mech_ok(string_of_int (Unix.getuid ())))
end

class client_mech_anonymous_handler = object
  inherit client_mechanism_handler
  method init = return (Client_mech_ok("obus " ^ OBus_info.version))
end

let keyring_file_name context = sprintf "%s/.dbus-keyrings/%s" (Lazy.force Util.homedir) context

let load_cookie context id =
  let rec aux ic =
    (perform
       line <-- Lwt_chan.input_line ic;
       (id', cookie) <-- catch
         (fun _ -> Scanf.sscanf line "%ld %Ld %s"
            (fun id' time cookie -> return (id', cookie)))
         (fun _ -> fail (Failure "invalid cookie file contents"));
       if id = id' then
         return cookie
       else
         aux ic)
  in
  catch
    (fun _ -> Util.with_open_in (keyring_file_name context) aux)
    (function
       | End_of_file -> fail (Failure "cookie id not found")
       | exn ->
           Log.error "failed to load cookie %ld from %s: %s" id (keyring_file_name context) (Util.string_of_exn exn);
           fail exn)

class client_mech_dbus_cookie_sha1_handler = object
  method init = return (Client_mech_continue(string_of_int (Unix.getuid ())))
  method data chal =
    Log.debug "client: dbus_cookie_sha1: chal: %s" chal;
    Scanf.sscanf chal "%[^/\\ \n\r.] %ld %[a-fA-F0-9]%!" begin fun context id chal ->
      load_cookie context id >>= fun cookie ->
        let rand = hex_encode (Util.random_string 16) in
        let resp = sprintf "%s %s" rand
          (hex_encode (Util.sha_1 (sprintf "%s:%s:%s" chal rand cookie))) in
        Log.debug "client: dbus_cookie_sha1: resp: %s" resp;
        return (Client_mech_ok resp)
    end
  method abort = ()
end

let client_mech_external = ("EXTERNAL", fun _ -> new client_mech_external_handler)
let client_mech_anonymous = ("ANONYMOUS", fun _ -> new client_mech_anonymous_handler)
let client_mech_dbus_cookie_sha1 = ("DBUS_COOKIE_SHA1", fun _ -> new client_mech_dbus_cookie_sha1_handler)

let default_client_mechanisms = [client_mech_external;
                                 client_mech_dbus_cookie_sha1;
                                 client_mech_anonymous]

(***** Predefined server mechanisms *****)

(* This two mechanisms do not work since we need to get credentials
   which are not available in ocaml.

class server_mech_external_handler = object
  inherit server_mechanism_handler
  method data _ = return Server_mech_ok
end

class server_mech_anonymous_handler = object
  inherit server_mechanism_handler
  method data _ = return Server_mech_ok
end
*)

let lock_file fname =
  let really_lock () =
    Unix.close(Unix.openfile fname
                 [Unix.O_WRONLY;
                  Unix.O_EXCL;
                  Unix.O_CREAT] 0o600)
  and remove_stale_lock () =
    Unix.unlink fname;
    Log.log "stale lock file %s removed" fname
  in
  let rec aux = function
    | 0 ->
        (perform
           Util.call remove_stale_lock
             (fun m -> Log.error "failed to remove stale lock file %s: %s" fname m);
           Util.call really_lock
             (fun m -> Log.error "failed to lock file %s after removing it: %s" fname m))
    | n ->
        try
          really_lock ();
          return ()
        with
            exn ->
              Log.debug "waiting for lock file %s" fname;
              Lwt_unix.sleep 0.250 >>= fun _ -> aux (n - 1)
  in
  aux 32

let unlock_file fname =
  Util.call
    (fun _ -> Unix.unlink fname)
    (fun m -> Log.error "failed to unlink file %s: %s" fname m)

let save_keyring context content =
  let fname = keyring_file_name context in
  let tmp_fname = fname ^ "." ^ hex_encode (Util.random_string 8) in
  let lock_fname = fname ^ ".lock" in
  let dir = sprintf "%s/.dbus-keyrings" (Lazy.force Util.homedir) in
  finalize
    (fun _ -> perform
       (try
          Unix.access dir [Unix.F_OK];
          return ()
        with _ -> Util.call
          (fun _ -> Unix.mkdir dir 0o600)
          (fun m -> Log.error "failed to create directory %s with permissions 0600%s" dir m));
       lock_file lock_fname;
       catch
         (fun _ -> Util.with_open_out tmp_fname
            (fun oc ->
               Lwt_util.iter_serial
                 (fun (id, time, cookie) ->
                    Lwt_chan.output_string oc (sprintf  "%ld %Ld %s\n" id time cookie))
                 content))
         (fun exn ->
            Log.error "unable to write temporary keyring file %s: %s" tmp_fname (Util.string_of_exn exn);
            fail exn);
       Util.call
         (fun _ -> Unix.rename tmp_fname fname)
         (fun m -> Log.error "unable to rename file %s to %s: %s" tmp_fname fname m))
    (fun _ -> unlock_file lock_fname)

let load_keyring context =
  let rec aux acc ic =
    catch
      (fun _ -> Lwt_chan.input_line ic >>= fun line -> return (Some line))
      (function
         | End_of_file -> return None
         | exn -> fail exn)
    >>= function
      | Some line ->
          (try
             Scanf.sscanf line "%ld %Ld %[a-fA-F0-9]\n"
               (fun id time cookie -> return (id, time, cookie))
           with
               exn -> fail Exit) >>= (fun entry -> aux (entry :: acc) ic)
      | None -> return acc
  in
  catch
    (fun _ -> Util.with_open_in (keyring_file_name context) (aux []))
    (fun _ -> return [])

class server_mech_dbus_cookie_sha1_handler = object
  inherit server_mechanism_handler

  val context = "org_freedesktop_general"
  val mutable state = `State1

  method data resp =
    try
      Log.debug "server: dbus_cookie_sha1: resp: %s" resp;
      match state with
        | `State1 ->
            load_keyring context >>= begin fun keyring ->
              let cur_time = Int64.of_float (Unix.time ()) in
              (* Filter old and future keys *)
              let keyring = List.filter
                (fun (id, time, cookie) -> Int64.abs (Int64.sub time cur_time) <= 300L) keyring in

              (* Find a working cookie *)
              begin match keyring with

                (* There is still valid cookies, just choose one *)
                | (id, time, cookie) :: _ -> return (id, cookie)

                (* No one left, generate a new one *)
                | [] ->
                    let id = Int32.abs (Util.random_int32 ()) in
                    let cookie = hex_encode (Util.random_string 24) in
                    (perform
                       save_keyring context [(id, cur_time, cookie)];
                       return (id, cookie))
              end >>= fun (id, cookie) ->
                let rand = hex_encode (Util.random_string 16) in
                let chal = sprintf "%s %ld %s" context id rand in
                let _ =
                  Log.debug "server: dbus_cookie_sha1: chal: %s" chal;
                  state <- `State2(cookie, rand)
                in
                return (Server_mech_continue chal)
            end

        | `State2(cookie, my_rand) ->
            Scanf.sscanf resp "%s %s"
              (fun its_rand comp_sha1 ->
                 if Util.sha_1 (sprintf "%s:%s:%s" my_rand its_rand cookie) = hex_decode comp_sha1 then
                   return Server_mech_ok
                 else
                   return Server_mech_reject)

    with _ -> return Server_mech_reject

  method abort = ()
end

(*
let server_mech_external = ("EXTERNAL", fun _ -> new server_mech_external_handler)
let server_mech_anonymous = ("ANONYMOUS", fun _ -> new server_mech_anonymous_handler)
*)
let server_mech_dbus_cookie_sha1 = ("DBUS_COOKIE_SHA1", fun _ -> new server_mech_dbus_cookie_sha1_handler)

let default_server_mechanisms = [server_mech_dbus_cookie_sha1]

(***** Transport *****)

type stream = {
  get_char : unit -> char Lwt.t;
  put_char : char -> unit Lwt.t;
  flush : unit -> unit Lwt.t;
}

let make_stream ~get_char ~put_char ~flush = {
  get_char = get_char;
  put_char = put_char;
  flush = flush;
}

let stream_of_lwt_channels (ic, oc) = {
  get_char = (fun _ -> Lwt_chan.input_char ic);
  put_char = (fun c -> Lwt_chan.output_char oc c);
  flush = (fun _ -> Lwt_chan.flush oc);
}

let send_line mode stream line =
  Log.debug "%s: sending: %S" mode line;
  let rec aux i =
    if i = String.length line then
      (perform
         stream.put_char '\r';
         stream.put_char '\n';
         stream.flush ())
    else
      (perform
         stream.put_char line.[i];
         aux (i + 1))
  in
  aux 0

let rec recv_line buffer stream eol_state =
  (* We need a limit to avoid consuming all the ram... *)
  if Buffer.length buffer > max_line_length then
    auth_failure "line too long received (>%d)" max_line_length
  else
    (perform
       ch <-- stream.get_char ();
       let _ = Buffer.add_char buffer ch in
       match eol_state, ch with
         | 0, '\r' -> recv_line buffer stream 1
         | 1, '\n' -> return (Buffer.sub buffer 0 (Buffer.length buffer - 2))
         | _ -> recv_line buffer stream 0)

let rec first f str pos =
  if pos = String.length str then
    pos
  else match f str.[pos] with
    | true -> pos
    | false -> first f str (pos + 1)

let rec last f str pos =
  if pos = 0 then
    pos
  else match f str.[pos - 1] with
    | true -> pos
    | false -> first f str (pos - 1)

let blank ch = ch = ' ' || ch = '\t'
let not_blank ch = not (blank ch)

let sub_strip str i j =
  let i = first not_blank str i in
  let j = last not_blank str j in
  if i < j then String.sub str i (j - i) else ""

let split str =
  let rec aux i =
    let i = first not_blank str i in
    if i = String.length str then
      []
    else
      let j = first blank str i in
      String.sub str i (j - i) :: aux j
  in
  aux 0

let preprocess_line line =
  (* Check for ascii-only *)
  String.iter (function
                 | '\x01'..'\x7f' -> ()
                 | _ -> failwith "non-ascii characters in command") line;
  (* Extract the command *)
  let i = first blank line 0 in
  if i = 0 then failwith "empty command";
  (String.sub line 0 i, sub_strip line i (String.length line))

let rec recv mode command_parser stream =
  (perform
     line <-- recv_line (Buffer.create 42) stream 0;
     let _ = Log.debug "%s: received: %S" mode line in

     (* If a parse failure occur, return an error and try again *)
     match
       try
         let command, args = preprocess_line line in
         `Success(command_parser command args)
       with
           exn -> `Failure(exn)
     with
       | `Success x -> return x
       | `Failure(Failure msg) ->
           (perform
              send_line mode stream ("ERROR \"" ^ msg ^ "\"");
              recv mode command_parser stream)
       | `Failure exn -> fail exn)

let client_recv = recv "client"
  (fun command args -> match command with
     | "REJECTED" -> Server_rejected (split args)
     | "OK" -> Server_ok(try OBus_uuid.of_string args with _ -> failwith "invalid hex-encoded guid")
     | "DATA" -> Server_data(hex_decode args)
     | "ERROR" -> Server_error args
     | _ -> failwith "invalid command")

let server_recv = recv "server"
  (fun command args -> match command with
     | "AUTH" -> Client_auth(match split args with
                               | [] -> None
                               | [mech] -> Some(mech, None)
                               | [mech; data] -> Some(mech, Some(hex_decode data))
                               | _ -> failwith "too many arguments")
     | "CANCEL" -> Client_cancel
     | "BEGIN" -> Client_begin
     | "DATA" -> Client_data(hex_decode args)
     | "ERROR" -> Client_error args
     | _ -> failwith "invalid command")

let client_send chans cmd = send_line "client" chans
  (match cmd with
     | Client_auth None -> "AUTH"
     | Client_auth(Some(mechanism, None)) -> sprintf "AUTH %s" mechanism
     | Client_auth(Some(mechanism, Some data)) -> sprintf "AUTH %s %s" mechanism (hex_encode data)
     | Client_cancel -> "CANCEL"
     | Client_begin -> "BEGIN"
     | Client_data data -> sprintf "DATA %s" (hex_encode data)
     | Client_error msg -> sprintf "ERROR \"%s\"" msg)

let server_send chans cmd = send_line "server" chans
  (match cmd with
     | Server_rejected mechs -> String.concat " " ("REJECTED" :: mechs)
     | Server_ok guid -> sprintf "OK %s" (OBus_uuid.to_string guid)
     | Server_data data -> sprintf "DATA %s" (hex_encode data)
     | Server_error msg -> sprintf "ERROR \"%s\"" msg)

(***** Client-side protocol ******)

module Client =
struct

  type state =
    | Waiting_for_data of client_mechanism_handler
    | Waiting_for_ok
    | Waiting_for_reject

  type transition =
    | Transition of client_command * state * client_mechanism list
    | Success of OBus_address.guid
    | Failure

  let find_working_mech implemented_mechanisms available_mechs =
    let rec aux = function
      | [] ->
          return Failure
      | (name, f) :: mechs ->
          match available_mechs with
            | Some l when not (List.mem name l) ->
                aux mechs
            | _ ->
                let mech = f () in
                catch
                  (fun _ -> mech#init >>= function
                     | Client_mech_continue resp ->
                         return (Transition(Client_auth(Some (name, Some resp)),
                                            Waiting_for_data mech,
                                            mechs))
                     | Client_mech_ok resp ->
                         return (Transition(Client_auth(Some (name, Some resp)),
                                            Waiting_for_ok,
                                            mechs))
                     | Client_mech_error msg ->
                         aux mechs)
                  (fun _ -> aux mechs)
    in
    aux implemented_mechanisms

  let initial mechs = find_working_mech mechs None
  let next mechs available = find_working_mech mechs (Some available)

  let transition mechs state cmd = match state with
    | Waiting_for_data mech -> begin match cmd with
        | Server_data chal ->
            catch
              (fun _ -> mech#data chal >>= function
                 | Client_mech_continue resp ->
                     return (Transition(Client_data resp,
                                        Waiting_for_data mech,
                                        mechs))
                 | Client_mech_ok resp ->
                     return (Transition(Client_data resp,
                                        Waiting_for_ok,
                                        mechs))
                 | Client_mech_error msg ->
                     return (Transition(Client_error msg,
                                        Waiting_for_data mech,
                                        mechs)))
              (fun exn -> return (Transition(Client_error(Util.string_of_exn exn),
                                             Waiting_for_data mech,
                                             mechs)))
        | Server_rejected am ->
            mech#abort;
            next mechs am
        | Server_error _ ->
            mech#abort;
            return (Transition(Client_cancel,
                               Waiting_for_reject,
                               mechs))
        | Server_ok guid ->
            mech#abort;
            return (Success guid)
              (*        | _ ->
                        Transition(Client_error "unexpected command",
                        Waiting_for_data mech,
                        mechs)*)
      end

    | Waiting_for_ok -> begin match cmd with
        | Server_ok guid -> return (Success guid)
        | Server_rejected am -> next mechs am
        | Server_data _
        | Server_error _ -> return (Transition(Client_cancel,
                                               Waiting_for_reject,
                                               mechs))
            (*        | _ ->
                      Transition(Client_error "unexpected command",
                      Waiting_for_ok,
                      mechs)*)
      end

    | Waiting_for_reject -> begin match cmd with
        | Server_rejected am -> next mechs am
        | _ -> return Failure
      end

  let exec mechs stream =
    let rec loop = function
      | Transition(cmd, state, mechs) ->
          (perform
             client_send stream cmd;
             cmd <-- client_recv stream;
             transition mechs state cmd >>= loop)
      | Success guid ->
          (perform
             client_send stream Client_begin;
             return guid)
      | Failure ->
          auth_failure "authentification failure"
    in
    (perform
       stream.put_char '\000';
       initial mechs >>= loop)
end

(***** Server-side protocol ******)

module Server =
struct

  type state =
    | Waiting_for_auth
    | Waiting_for_data of server_mechanism_handler
    | Waiting_for_begin

  type server_machine_transition =
    | Transition of server_command * state
    | Accept
    | Failure

  let reject mechs =
    return (Transition(Server_rejected (List.map fst mechs),
                       Waiting_for_auth))

  let error msg =
    return (Transition(Server_error msg,
                       Waiting_for_auth))

  let transition guid mechs state cmd = match state with
    | Waiting_for_auth -> begin match cmd with
        | Client_auth None ->
            reject mechs
        | Client_auth(Some(name, resp)) ->
            begin match Util.assoc name mechs with
              | None -> reject mechs
              | Some f ->
                  let mech = f () in
                  catch
                    (fun _ -> perform
                       init <-- mech#init;
                       match init, resp with
                         | None, None ->
                             return (Transition(Server_data "",
                                                Waiting_for_data mech))
                         | Some chal, None ->
                             return (Transition(Server_data chal,
                                                Waiting_for_data mech))
                         | Some chal, Some rest ->
                             reject mechs
                         | None, Some resp -> mech#data resp >>= function
                             | Server_mech_continue chal ->
                                 return (Transition(Server_data chal,
                                                    Waiting_for_data mech))
                             | Server_mech_ok ->
                                 return (Transition(Server_ok guid,
                                                    Waiting_for_begin))
                             | Server_mech_reject ->
                                 reject mechs)
                    (fun _ -> reject mechs)
            end
        | Client_begin -> return Failure
        | Client_error msg -> reject mechs
        | _ -> error "AUTH command expected"
      end

    | Waiting_for_data mech -> begin match cmd with
        | Client_data "" ->
            return (Transition(Server_data "",
                               Waiting_for_data mech))
        | Client_data resp ->
            catch
              (fun _ -> mech#data resp >>= function
                 | Server_mech_continue chal ->
                     return (Transition(Server_data chal,
                                        Waiting_for_data mech))
                 | Server_mech_ok ->
                     return (Transition(Server_ok guid,
                                        Waiting_for_begin))
                 | Server_mech_reject ->
                     reject mechs)
              (fun _ -> reject mechs)
        | Client_begin -> mech#abort; return Failure
        | Client_cancel -> mech#abort; reject mechs
        | Client_error _ -> mech#abort; reject mechs
        | _ -> mech#abort; error "DATA command expected"
      end

    | Waiting_for_begin -> begin match cmd with
        | Client_begin -> return Accept
        | Client_cancel -> reject mechs
        | Client_error _ -> reject mechs
        | _ -> error "BEGIN command expected"
      end

  let exec mechs guid stream =
    let rec loop state count =
      (perform
         cmd <-- server_recv stream;
         transition guid mechs state cmd >>= function
           | Transition(cmd, state) ->
               let count = match cmd with
                 | Server_rejected _ -> count + 1
                 | _ -> count in
               (* Specification do not specify a limit for rejected,
                  so we choose one arbitrary *)
               if count >= max_reject then
                 auth_failure "too many reject"
               else
                 (perform
                    server_send stream cmd;
                    loop state count)
           | Accept ->
               return ()
           | Failure ->
               auth_failure "authentification failure")
    in
    stream.get_char () >>= function
      | '\000' ->
          loop Waiting_for_auth 0
      | _ ->
          auth_failure "initial null byte missing"
end

let client_authenticate ?(mechanisms=default_client_mechanisms) = Client.exec mechanisms
let server_authenticate ?(mechanisms=default_server_mechanisms) = Server.exec mechanisms
