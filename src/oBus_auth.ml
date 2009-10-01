(*
 * oBus_auth.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open Lwt

(* Maximum line length, if line greated are received, authentication
   will fail *)
let max_line_length = 42 * 1024

(* Maximum number of reject, if a client is rejected more than that,
   authentication will fail *)
let max_reject = 42

exception Auth_failure of string
let auth_failure fmt = ksprintf (fun msg -> fail (Auth_failure msg)) fmt

let hex_encode = OBus_util.hex_encode
let hex_decode str =
  try
    OBus_util.hex_decode str
  with
    | Invalid_argument _ -> failwith "invalid hex-encoded data"

type data = string

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

(* +-----------------------------------------------------------------+
   | Keyring for the SHA-1 method                                    |
   +-----------------------------------------------------------------+ *)

module Cookie =
struct
  type t = {
    id : int32;
    time : int64;
    cookie : string;
  } with projection
end

module Keyring : sig

  type context = string
      (** A context for the SHA-1 method *)

  val load : context -> Cookie.t list Lwt.t
    (** [load context] load all cookies for context [context] *)

  val save : context -> Cookie.t list -> unit Lwt.t
    (** [save context cookies] save all cookies with context
        [context] *)
end = struct

  type context = string

  let keyring_directory = lazy(Filename.concat (Lazy.force OBus_util.homedir) ".dbus-keyrings")

  let keyring_file_name context =
    Filename.concat (Lazy.force keyring_directory) context

  let parse_line line =
    Scanf.sscanf line "%ld %Ld %[a-fA-F0-9]"
      (fun id time cookie -> { Cookie.id = id;
                               Cookie.time = time;
                               Cookie.cookie = cookie })

  let print_line cookie =
    sprintf  "%ld %Ld %s" (Cookie.id cookie) (Cookie.time cookie) (Cookie.cookie cookie)

  let load context =
    let fname = keyring_file_name context in
    if Sys.file_exists fname then
      try_lwt
        Lwt_stream.get_while (fun _ -> true) (Lwt_stream.map parse_line (Lwt_io.lines_of_file fname))
      with exn ->
        ERROR("failed to load cookie file %s: %s" (keyring_file_name context) (OBus_util.string_of_exn exn));
        fail exn
    else
      return []

  let lock_file fname =
    let really_lock () =
      Unix.close(Unix.openfile fname
                   [Unix.O_WRONLY;
                    Unix.O_EXCL;
                    Unix.O_CREAT] 0o600)
    in
    let rec aux = function
      | 0 ->
          (try
             Unix.unlink fname;
             LOG("stale lock file %s removed" fname)
           with Unix.Unix_error(error, _, _) as exn ->
             ERROR("failed to remove stale lock file %s: %s" fname (Unix.error_message error));
             raise exn);
          (try
             really_lock ();
             return ()
           with Unix.Unix_error(error, _, _) as exn ->
             ERROR("failed to lock file %s after removing it: %s" fname (Unix.error_message error));
             raise exn)
      | n ->
          try
            really_lock ();
            return ()
          with exn ->
            LOG("waiting for lock file (%d) %s" n fname);
            lwt () = Lwt_unix.sleep 0.250 in
            aux (n - 1)
    in
    aux 32

  let unlock_file fname =
    try
      Unix.unlink fname
    with Unix.Unix_error(error, _, _) as exn ->
      ERROR("failed to unlink file %s: %s" fname (Unix.error_message error));
      raise exn

  let save context cookies =
    let fname = keyring_file_name context in
    let tmp_fname = fname ^ "." ^ hex_encode (OBus_util.random_string 8) in
    let lock_fname = fname ^ ".lock" in
    let lazy dir = keyring_directory in
    (* Check that the keyring directory exists, or create it *)
    if not (Sys.file_exists dir) then begin
      try
        Unix.mkdir dir 0o700
      with Unix.Unix_error(error, _, _) as exn ->
        ERROR("failed to create directory %s with permissions 0600: %s" dir (Unix.error_message error));
        raise exn
    end;
    lwt () = lock_file lock_fname in
    try_lwt
      lwt () =
        try_lwt
          Lwt_io.lines_to_file tmp_fname (Lwt_stream.map print_line (Lwt_stream.of_list cookies))
        with exn ->
          ERROR("unable to write temporary keyring file %s: %s" tmp_fname (OBus_util.string_of_exn exn));
          fail exn
      in
      try
        Unix.rename tmp_fname fname;
        return ()
      with Unix.Unix_error(error, _, _) as exn ->
        ERROR("unable to rename file %s to %s: %s" tmp_fname fname (Unix.error_message error));
        fail exn
      finally
        unlock_file lock_fname;
        return ()
end

(* +-----------------------------------------------------------------+
   | Communication                                                   |
   +-----------------------------------------------------------------+ *)

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

let stream_of_channels ic oc = {
  get_char = (fun _ -> Lwt_io.read_char ic);
  put_char = (fun c -> Lwt_io.write_char oc c);
  flush = (fun _ -> Lwt_io.flush oc);
}

let send_line mode stream line =
  DEBUG("%s: sending: %S" mode line);
  let rec aux i =
    if i = String.length line then
      lwt () = stream.put_char '\r' in
      lwt () = stream.put_char '\n' in
      stream.flush ()
    else
      lwt () = stream.put_char line.[i] in
      aux (i + 1)
  in
  aux 0

let rec recv_line buffer stream eol_state =
  (* We need a limit to avoid consuming all the ram... *)
  if Buffer.length buffer > max_line_length then
    auth_failure "line too long received (>%d)" max_line_length
  else
    lwt ch = stream.get_char () in
    Buffer.add_char buffer ch;
    match eol_state, ch with
      | 0, '\r' -> recv_line buffer stream 1
      | 1, '\n' -> return (Buffer.sub buffer 0 (Buffer.length buffer - 2))
      | _ -> recv_line buffer stream 0

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
  lwt line = recv_line (Buffer.create 42) stream 0 in
  DEBUG("%s: received: %S" mode line);

  (* If a parse failure occur, return an error and try again *)
  match
    try
      let command, args = preprocess_line line in
      `Success(command_parser command args)
    with exn ->
      `Failure(exn)
  with
    | `Success x -> return x
    | `Failure(Failure msg) ->
        lwt () = send_line mode stream ("ERROR \"" ^ msg ^ "\"") in
        recv mode command_parser stream
    | `Failure exn -> fail exn

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

(* +-----------------------------------------------------------------+
   | Client side authentication                                      |
   +-----------------------------------------------------------------+ *)

module Client =
struct

  type mechanism_return =
    | Mech_continue of data
    | Mech_ok of data
    | Mech_error of string

  class virtual mechanism_handler = object
    method virtual init : mechanism_return Lwt.t
    method data (chall : data) = return (Mech_error("no data expected for this mechanism"))
    method abort = ()
  end

  type mechanism = string * (unit -> mechanism_handler)

  (* +---------------------------------------------------------------+
     | Predefined client mechanisms                                  |
     +---------------------------------------------------------------+ *)

  class mech_external_handler = object
    inherit mechanism_handler
    method init = return (Mech_ok(string_of_int (Unix.getuid ())))
  end

  class mech_anonymous_handler = object
    inherit mechanism_handler
    method init = return (Mech_ok("obus " ^ OBus_info.version))
  end

  class mech_dbus_cookie_sha1_handler = object
    method init = return (Mech_continue(string_of_int (Unix.getuid ())))
    method data chal =
      DEBUG("client: dbus_cookie_sha1: chal: %s" chal);
      let context, id, chal = Scanf.sscanf chal "%[^/\\ \n\r.] %ld %[a-fA-F0-9]%!" (fun context id chal -> (context, id, chal)) in
      lwt keyring = Keyring.load context in
      let cookie =
        try
          List.find (fun cookie -> cookie.Cookie.id = id) keyring
        with Not_found ->
          ksprintf failwith "cookie %ld not found in context %S" id context
      in
      let rand = hex_encode (OBus_util.random_string 16) in
      let resp = sprintf "%s %s" rand (hex_encode (OBus_util.sha_1 (sprintf "%s:%s:%s" chal rand cookie.Cookie.cookie))) in
      DEBUG("client: dbus_cookie_sha1: resp: %s" resp);
      return (Mech_ok resp)
    method abort = ()
  end

  let mech_external = ("EXTERNAL", fun _ -> new mech_external_handler)
  let mech_anonymous = ("ANONYMOUS", fun _ -> new mech_anonymous_handler)
  let mech_dbus_cookie_sha1 = ("DBUS_COOKIE_SHA1", fun _ -> new mech_dbus_cookie_sha1_handler)

  let default_mechanisms = [mech_external;
                            mech_dbus_cookie_sha1;
                            mech_anonymous]

  (* +---------------------------------------------------------------+
     | Client-side protocol                                          |
     +---------------------------------------------------------------+ *)

  type state =
    | Waiting_for_data of mechanism_handler
    | Waiting_for_ok
    | Waiting_for_reject

  type transition =
    | Transition of client_command * state * mechanism list
    | Success of OBus_address.guid
    | Failure

  (* Try to find a mechanism that can be initialised *)
  let find_working_mech implemented_mechanisms available_mechanisms =
    let rec aux = function
      | [] ->
          return Failure
      | (name, f) :: mechs ->
          match available_mechanisms with
            | Some l when not (List.mem name l) ->
                aux mechs
            | _ ->
                let mech = f () in
                try_lwt
                  mech#init >>= function
                    | Mech_continue resp ->
                        return (Transition(Client_auth(Some (name, Some resp)),
                                           Waiting_for_data mech,
                                           mechs))
                    | Mech_ok resp ->
                        return (Transition(Client_auth(Some (name, Some resp)),
                                           Waiting_for_ok,
                                           mechs))
                    | Mech_error msg ->
                        aux mechs
                with exn ->
                  aux mechs
    in
    aux implemented_mechanisms

  let initial mechs = find_working_mech mechs None
  let next mechs available = find_working_mech mechs (Some available)

  let transition mechs state cmd = match state with
    | Waiting_for_data mech -> begin match cmd with
        | Server_data chal ->
            begin
              try_lwt
                mech#data chal >>= function
                  | Mech_continue resp ->
                      return (Transition(Client_data resp,
                                         Waiting_for_data mech,
                                         mechs))
                  | Mech_ok resp ->
                      return (Transition(Client_data resp,
                                         Waiting_for_ok,
                                         mechs))
                  | Mech_error msg ->
                      return (Transition(Client_error msg,
                                         Waiting_for_data mech,
                                         mechs))
              with exn ->
                return (Transition(Client_error(OBus_util.string_of_exn exn),
                                   Waiting_for_data mech,
                                   mechs))
            end
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
      end

    | Waiting_for_ok -> begin match cmd with
        | Server_ok guid -> return (Success guid)
        | Server_rejected am -> next mechs am
        | Server_data _
        | Server_error _ -> return (Transition(Client_cancel,
                                               Waiting_for_reject,
                                               mechs))
      end

    | Waiting_for_reject -> begin match cmd with
        | Server_rejected am -> next mechs am
        | _ -> return Failure
      end


  let authenticate ?(mechanisms=default_mechanisms) stream =
    let rec loop = function
      | Transition(cmd, state, mechs) ->
          lwt () = client_send stream cmd in
          lwt cmd = client_recv stream in
          transition mechs state cmd >>= loop
      | Success guid ->
          lwt () = client_send stream Client_begin in
          return guid
      | Failure ->
          auth_failure "authentification failure"
    in
    lwt () = stream.put_char '\000' in
    initial mechanisms >>= loop
end

(* +-----------------------------------------------------------------+
   | Server-side authentication                                      |
   +-----------------------------------------------------------------+ *)

module Server =
struct

  type mechanism_return =
    | Mech_continue of data
    | Mech_ok
    | Mech_reject

  class virtual mechanism_handler = object
    method init = return (None : data option)
    method virtual data : data -> mechanism_return Lwt.t
    method abort = ()
  end

  type mechanism = string * (unit -> mechanism_handler)

  (* +---------------------------------------------------------------+
     | Predefined server mechanisms                                  |
     +---------------------------------------------------------------+ *)

  (* This two mechanisms do not work since we need to get credentials
     using functions that are not available in ocaml.

     class server_mech_external_handler = object
       inherit server_mechanism_handler
       method data _ = return Mech_ok
     end

     class server_mech_anonymous_handler = object
       inherit server_mechanism_handler
       method data _ = return Mech_ok
     end
  *)

  class mech_dbus_cookie_sha1_handler = object
    inherit mechanism_handler

    val context = "org_freedesktop_general"
    val mutable state = `State1

    method data resp =
      try
        DEBUG("server: dbus_cookie_sha1: resp: %s" resp);
        match state with
          | `State1 ->
              lwt keyring = Keyring.load context in
              let cur_time = Int64.of_float (Unix.time ()) in
              (* Filter old and future keys *)
              let keyring = List.filter (fun { Cookie.time = time } -> time <= cur_time && Int64.sub cur_time time <= 300L) keyring in
              (* Find a working cookie *)
              lwt id, cookie = match keyring with
                | { Cookie.id = id; Cookie.cookie = cookie } :: _ ->
                    (* There is still valid cookies, just choose one *)
                    return (id, cookie)
                | [] ->
                    (* No one left, generate a new one *)
                    let id = Int32.abs (OBus_util.random_int32 ()) in
                    let cookie = hex_encode (OBus_util.random_string 24) in
                    lwt () = Keyring.save context [{ Cookie.id = id; Cookie.time = cur_time; Cookie.cookie = cookie }] in
                    return (id, cookie)
              in
              let rand = hex_encode (OBus_util.random_string 16) in
              let chal = sprintf "%s %ld %s" context id rand in
              DEBUG("server: dbus_cookie_sha1: chal: %s" chal);
              state <- `State2(cookie, rand);
              return (Mech_continue chal)

          | `State2(cookie, my_rand) ->
              Scanf.sscanf resp "%s %s"
                (fun its_rand comp_sha1 ->
                   if OBus_util.sha_1 (sprintf "%s:%s:%s" my_rand its_rand cookie) = hex_decode comp_sha1 then
                     return Mech_ok
                   else
                     return Mech_reject)

      with _ ->
        return Mech_reject

    method abort = ()
  end

  (*
    let server_mech_external = ("EXTERNAL", fun _ -> new server_mech_external_handler)
    let server_mech_anonymous = ("ANONYMOUS", fun _ -> new server_mech_anonymous_handler)
  *)
  let mech_dbus_cookie_sha1 = ("DBUS_COOKIE_SHA1", fun _ -> new mech_dbus_cookie_sha1_handler)

  let default_mechanisms = [mech_dbus_cookie_sha1]

  (* +---------------------------------------------------------------+
     | Server-side protocol                                          |
     +---------------------------------------------------------------+ *)

  type state =
    | Waiting_for_auth
    | Waiting_for_data of mechanism_handler
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
            begin match OBus_util.assoc name mechs with
              | None ->
                  reject mechs
              | Some f ->
                  let mech = f () in
                  try_lwt
                    lwt init = mech#init in
                    match init, resp with
                      | None, None ->
                          return (Transition(Server_data "",
                                             Waiting_for_data mech))
                      | Some chal, None ->
                          return (Transition(Server_data chal,
                                             Waiting_for_data mech))
                      | Some chal, Some rest ->
                          reject mechs
                      | None, Some resp ->
                          mech#data resp >>= function
                            | Mech_continue chal ->
                                return (Transition(Server_data chal,
                                                   Waiting_for_data mech))
                            | Mech_ok ->
                                return (Transition(Server_ok guid,
                                                   Waiting_for_begin))
                            | Mech_reject ->
                                reject mechs
                  with exn ->
                    reject mechs
            end
        | Client_begin -> return Failure
        | Client_error msg -> reject mechs
        | _ -> error "AUTH command expected"
      end

    | Waiting_for_data mech -> begin match cmd with
        | Client_data "" ->
            return (Transition(Server_data "",
                               Waiting_for_data mech))
        | Client_data resp -> begin
            try_lwt
              mech#data resp >>= function
                | Mech_continue chal ->
                    return (Transition(Server_data chal,
                                       Waiting_for_data mech))
                | Mech_ok ->
                    return (Transition(Server_ok guid,
                                       Waiting_for_begin))
                | Mech_reject ->
                    reject mechs
            with exn ->
              reject mechs
          end
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

  let authenticate ?(mechanisms=default_mechanisms) guid stream =
    let rec loop state count =
      lwt cmd = server_recv stream in
      transition guid mechanisms state cmd >>= function
        | Transition(cmd, state) ->
            let count =
              match cmd with
                | Server_rejected _ -> count + 1
                | _ -> count
            in
            (* Specification do not specify a limit for rejected, so
               we choose one arbitrary *)
            if count >= max_reject then
              auth_failure "too many reject"
            else
              lwt () = server_send stream cmd in
              loop state count
        | Accept ->
            return ()
        | Failure ->
            auth_failure "authentification failure"
    in
    stream.get_char () >>= function
      | '\000' ->
          loop Waiting_for_auth 0
      | _ ->
          auth_failure "initial null byte missing"
end
