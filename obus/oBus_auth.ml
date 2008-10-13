(*
 * oBus_auth.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open Lwt_chan
open Auth_command

let failwith fmt = Printf.ksprintf (fun msg -> raise (Failure msg)) fmt

type data = string

type client_mechanism_return =
  | Client_mech_continue of data
  | Client_mech_ok of data
  | Client_mech_error of string

type server_mechanism_return =
  | Server_mech_continue of data
  | Server_mech_ok
  | Server_mech_reject

class virtual client_mechanism_handler = object
  method virtual init : client_mechanism_return
  method data (chall : data) = Client_mech_error("no data expected for this mechanism")
  method abort = ()
end

class virtual server_mechanism_handler = object
  method virtual init : data option -> server_mechanism_return
  method data (resp : data) = Server_mech_reject
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
  method init = function
    | None -> Server_mech_reject
    | Some id -> Server_mech_ok
end

class server_mech_anonymous_handler = object
  inherit server_mechanism_handler
  method init _ = Server_mech_ok
end

let server_mech_external = ("EXTERNAL", fun _ -> new server_mech_external_handler)
let server_mech_anonymous = ("ANONYMOUS", fun _ -> new server_mech_anonymous_handler)

let default_server_mechanisms = [server_mech_external]

(***** Transport *****)

let hexstring_of_data buf str =
  String.iter (fun c -> Printf.bprintf buf "%02x" (int_of_char c)) str

let send mode marshaler oc command =
  let buf = Buffer.create 42 in
  marshaler buf command;
  Buffer.add_string buf "\r\n";
  let line = Buffer.contents buf in
  DEBUG("%s/sending: %S" mode line);
  (perform
     output_string oc line;
     flush oc)

let rec recv mode lexer ic =
  (perform
     l <-- input_line ic;
     let _ = DEBUG("%s/received: %S" mode l) in
     try
       return (lexer (Lexing.from_string l))
     with
         _ -> failwith "parse failure")

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
        | Server_error ->
            mech#abort;
            Transition(Client_cancel,
                       Waiting_for_reject,
                       mechs)
        | Server_ok guid ->
            mech#abort;
            Success guid
        | _ ->
            Transition(Client_error "unexpected command",
                       Waiting_for_data mech,
                       mechs)
      end

    | Waiting_for_ok -> begin match cmd with
        | Server_ok guid -> Success guid
        | Server_rejected am -> next mechs am
        | Server_data _
        | Server_error -> Transition(Client_cancel,
                                     Waiting_for_reject,
                                     mechs)
        | _ ->
            Transition(Client_error "unexpected command",
                       Waiting_for_ok,
                       mechs)
      end

    | Waiting_for_reject -> begin match cmd with
        | Server_rejected am -> next mechs am
        | _ -> Failure
      end

  let marshaler buf = function
    | Client_auth None ->
        Buffer.add_string buf "AUTH"
    | Client_auth(Some(mechanism, data)) ->
        Buffer.add_string buf "AUTH ";
        Buffer.add_string buf mechanism;
        begin match data with
          | Some data ->
              Buffer.add_char buf ' ';
              hexstring_of_data buf data
          | None ->
              ()
        end
    | Client_cancel -> Buffer.add_string buf "CANCEL"
    | Client_begin -> Buffer.add_string buf "BEGIN"
    | Client_data(data) ->
        Buffer.add_string buf "DATA ";
        hexstring_of_data buf data
    | Client_error(message) ->
        Buffer.add_string buf "ERROR ";
        Buffer.add_string buf message

  let recv = recv "client" Auth_lexer.server_command
  let send = send "client" marshaler

  let exec mechs (ic, oc) =
    let rec loop = function
      | Transition(cmd, state, mechs) ->
          (perform
             send oc cmd;
             cmd <-- recv ic;
             loop (transition mechs state cmd))
      | Success guid ->
          (perform
             send oc Client_begin;
             return guid)
      | Failure ->
          failwith "authentification failure"
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

  let error =
    Transition(Server_error,
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
                  match mech#init resp with
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
        | _ -> error
      end

    | Waiting_for_data mech -> begin match cmd with
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
        | _ -> mech#abort; error
      end

    | Waiting_for_begin -> begin match cmd with
        | Client_begin -> Accept
        | Client_cancel -> reject mechs
        | Client_error _ -> reject mechs
        | _ -> error
      end

  let marshaler buf = function
    | Server_rejected mechs ->
        Buffer.add_string buf "REJECTED";
        List.iter (fun name ->
                     Buffer.add_char buf ' ';
                     Buffer.add_string buf name)
          mechs
    | Server_ok guid ->
        Buffer.add_string buf "OK ";
        Buffer.add_string buf (OBus_uuid.to_string guid)
    | Server_data data ->
        Buffer.add_string buf "DATA ";
        hexstring_of_data buf data
    | Server_error ->
        Buffer.add_string buf "ERROR"

  let recv = recv "server" Auth_lexer.client_command
  let send = send "server" marshaler

  let exec mechs guid (ic, oc) =
    let rec loop state count =
      (perform
         cmd <-- recv ic;
         match transition guid mechs state cmd with
           | Transition(cmd, state) ->
               let count = match cmd with
                 | Server_rejected _ -> count + 1
                 | _ -> count in
               (* Specification do not specify a limit for rejected,
                  so we choose one arbitrary *)
               if count >= 42 then
                 failwith "too many reject"
               else
                 (perform
                    send oc cmd;
                    loop state count)
           | Accept ->
               return ()
           | Failure ->
               failwith "authentification failure")
    in
    (perform
       ch <-- input_char ic;
       if ch = '\000' then
         loop Waiting_for_auth 0
       else
         failwith "initial null byte missing")
end

let client_authenticate ?(mechanisms=default_client_mechanisms) = Client.exec mechanisms
let server_authenticate ?(mechanisms=default_server_mechanisms) = Server.exec mechanisms
