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
open Lwt_chan

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
  method virtual init : client_mechanism_return
  method data (chall : data) = Client_mech_error("no data expected for this mechanism")
  method abort = ()
end

class virtual server_mechanism_handler = object
  method init : data option = None
  method virtual data : data -> server_mechanism_return
  method abort = ()
end

type client_mechanism = string * (unit -> client_mechanism_handler)
type server_mechanism = string * (unit -> server_mechanism_handler)

(***** Predefined client mechanisms *****)

class client_mech_external_handler = object
  inherit client_mechanism_handler
  method init = Client_mech_ok(string_of_int (Unix.getuid ()))
end

class client_mech_anonymous_handler = object
  inherit client_mechanism_handler
  method init = Client_mech_ok("obus " ^ OBus_info.version)
end

let client_mech_external = ("EXTERNAL", fun _ -> new client_mech_external_handler)
let client_mech_anonymous = ("ANONYMOUS", fun _ -> new client_mech_anonymous_handler)

let default_client_mechanisms = [client_mech_external;
                                 client_mech_anonymous]

(***** Predefined server mechanisms *****)

class server_mech_external_handler = object
  inherit server_mechanism_handler
  method data _ = Server_mech_ok
end

class server_mech_anonymous_handler = object
  inherit server_mechanism_handler
  method data _ = Server_mech_ok
end

let server_mech_external = ("EXTERNAL", fun _ -> new server_mech_external_handler)
let server_mech_anonymous = ("ANONYMOUS", fun _ -> new server_mech_anonymous_handler)

let default_server_mechanisms = [server_mech_external]

(***** Transport *****)

let send_line mode oc line =
  DEBUG("%s: sending: %S" mode line);
  (perform
     output_string oc line;
     output_string oc "\r\n";
     flush oc)

let rec recv_line buffer ic eol_state =
  (* We need a limit to avoid consuming all the ram... *)
  if Buffer.length buffer > max_line_length then
    auth_failure "line too long received (>%d)" max_line_length
  else
    (perform
       ch <-- input_char ic;
       let _ = Buffer.add_char buffer ch in
       match eol_state, ch with
         | 0, '\r' -> recv_line buffer ic 1
         | 1, '\n' -> return (Buffer.sub buffer 0 (Buffer.length buffer - 2))
         | _ -> recv_line buffer ic 0)

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

let rec recv mode command_parser (ic, oc) =
  (perform
     line <-- recv_line (Buffer.create 42) ic 0;
     let _ = DEBUG("%s: received: %S" mode line) in

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
              send_line mode oc ("ERROR \"" ^ msg ^ "\"");
              recv mode command_parser (ic, oc))
       | `Failure exn -> fail exn)

let hex_encode = Util.hex_encode
let hex_decode str =
  try
    Util.hex_decode str
  with
    | Invalid_argument _ -> failwith "invalid hex-encoded data"

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
          Failure
      | (name, f) :: mechs ->
          match available_mechs with
            | Some l when not (List.mem name l) ->
                aux mechs
            | _ ->
                let mech = f () in
                begin match mech#init with
                  | Client_mech_continue resp ->
                      Transition(Client_auth(Some (name, Some resp)),
                                 Waiting_for_data mech,
                                 mechs)
                  | Client_mech_ok resp ->
                      Transition(Client_auth(Some (name, Some resp)),
                                 Waiting_for_ok,
                                 mechs)
                  | Client_mech_error msg ->
                      aux mechs
                end
    in
    aux implemented_mechanisms

  let initial mechs = find_working_mech mechs None
  let next mechs available = find_working_mech mechs (Some available)

  let transition mechs state cmd = match state with
    | Waiting_for_data mech -> begin match cmd with
        | Server_data chall ->
            begin match mech#data chall with
              | Client_mech_continue resp ->
                  Transition(Client_data resp,
                             Waiting_for_data mech,
                             mechs)
              | Client_mech_ok resp ->
                  Transition(Client_data resp,
                             Waiting_for_ok,
                             mechs)
              | Client_mech_error msg ->
                  Transition(Client_error msg,
                             Waiting_for_data mech,
                             mechs)
            end
        | Server_rejected am ->
            mech#abort;
            next mechs am
        | Server_error _ ->
            mech#abort;
            Transition(Client_cancel,
                       Waiting_for_reject,
                       mechs)
        | Server_ok guid ->
            mech#abort;
            Success guid
(*        | _ ->
            Transition(Client_error "unexpected command",
                       Waiting_for_data mech,
                       mechs)*)
      end

    | Waiting_for_ok -> begin match cmd with
        | Server_ok guid -> Success guid
        | Server_rejected am -> next mechs am
        | Server_data _
        | Server_error _ -> Transition(Client_cancel,
                                       Waiting_for_reject,
                                       mechs)
(*        | _ ->
            Transition(Client_error "unexpected command",
                       Waiting_for_ok,
                       mechs)*)
      end

    | Waiting_for_reject -> begin match cmd with
        | Server_rejected am -> next mechs am
        | _ -> Failure
      end

  let exec mechs (ic, oc) =
    let rec loop = function
      | Transition(cmd, state, mechs) ->
          (perform
             client_send oc cmd;
             cmd <-- client_recv (ic, oc);
             loop (transition mechs state cmd))
      | Success guid ->
          (perform
             client_send oc Client_begin;
             return guid)
      | Failure ->
          auth_failure "authentification failure"
    in
    (perform
       output_char oc '\000';
       loop (initial mechs))
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
    Transition(Server_rejected (List.map fst mechs),
               Waiting_for_auth)

  let error msg =
    Transition(Server_error msg,
               Waiting_for_auth)

  let transition guid mechs state cmd = match state with
    | Waiting_for_auth -> begin match cmd with
        | Client_auth None ->
            reject mechs
        | Client_auth(Some(name, resp)) ->
            begin match Util.assoc name mechs with
              | None -> reject mechs
              | Some f ->
                  let mech = f () in
                  match mech#init, resp with
                    | None, None ->
                        Transition(Server_data "",
                                   Waiting_for_data mech)
                    | Some chall, None ->
                        Transition(Server_data chall,
                                   Waiting_for_data mech)
                    | Some chall, Some rest ->
                        reject mechs
                    | None, Some resp -> match mech#data resp with
                        | Server_mech_continue chall ->
                            Transition(Server_data chall,
                                       Waiting_for_data mech)
                        | Server_mech_ok ->
                            Transition(Server_ok guid,
                                       Waiting_for_begin)
                        | Server_mech_reject ->
                            reject mechs
            end
        | Client_begin -> Failure
        | Client_error msg -> reject mechs
        | _ -> error "AUTH command expected"
      end

    | Waiting_for_data mech -> begin match cmd with
        | Client_data "" ->
            Transition(Server_data "",
                       Waiting_for_data mech)
        | Client_data resp ->
            begin match mech#data resp with
              | Server_mech_continue chall ->
                  Transition(Server_data chall,
                             Waiting_for_data mech)
              | Server_mech_ok ->
                  Transition(Server_ok guid,
                             Waiting_for_data mech)
              | Server_mech_reject ->
                  reject mechs
            end
        | Client_begin -> mech#abort; Failure
        | Client_cancel -> mech#abort; reject mechs
        | Client_error _ -> mech#abort; reject mechs
        | _ -> mech#abort; error "DATA command expected"
      end

    | Waiting_for_begin -> begin match cmd with
        | Client_begin -> Accept
        | Client_cancel -> reject mechs
        | Client_error _ -> reject mechs
        | _ -> error "BEGIN command expected"
      end

  let exec mechs guid (ic, oc) =
    let rec loop state count =
      (perform
         cmd <-- server_recv (ic, oc);
         match transition guid mechs state cmd with
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
                    server_send oc cmd;
                    loop state count)
           | Accept ->
               return ()
           | Failure ->
               auth_failure "authentification failure")
    in
    (perform
       ch <-- input_char ic;
       if ch = '\000' then
         loop Waiting_for_auth 0
       else
         auth_failure "initial null byte missing")
end

let client_authenticate ?(mechanisms=default_client_mechanisms) = Client.exec mechanisms
let server_authenticate ?(mechanisms=default_server_mechanisms) = Server.exec mechanisms
