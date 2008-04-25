(*
 * auth.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Unix

type data = string
type guid = string

type client_state =
  [ `Waiting_for_data
  | `Waiting_for_ok
  | `Waiting_for_reject ]

type client_command =
  [ `Auth of string * data
  | `Cancel
  | `Begin
  | `Data of data
  | `Error of string ]

type server_command =
    [ `Rejected of string list
    | `OK of guid
    | `Data of data
    | `Error of string ]

class type mechanism =
object
  method init : mechanism_return
  method data : data -> mechanism_return
  method shutdown : unit
end

class virtual immediate =
object
  method virtual init : mechanism_return
  method data _ = Error("no data expected for this mechanism")
  method shutdown = ()
end

(* Predefined mechanisms *)

class external_mech = object
  inherit immediate
  method init = OK(string_of_int (Unix.getuid ()))
end

type maker = string * (unit -> mechanism)
let makers = Protected.make [("EXTERNAL", fun () -> new external_mech)]
let safe f x = try f x with _ -> Error("mechanism error")
val register_mechanism m = Protected.update (fun l -> (safe m) :: l)

(* The protocol state machine for the client *)

type client_machine_state = client_state * mechanism * maker list
type client_machine_transition =
  | ClientTransition of (client_command * client_machine_state)
  | ClientFinal
  | ClientFailure of string

(* Transitions *)

let rec find_mechanism = function
  | [] -> None
  | (name, create_mech) :: l ->
      let mech = create_mech () in
        try
          match mech#init with
            | Continue(resp) -> Some(`Auth(name, resp), (`Waiting_for_data, mech, mechs))
            | OK(resp)       -> Some(`Auth(name, resp), (`Waiting_for_ok,   mech, mechs))
            | Error(_)       -> find_mechanism l
        with
            _ -> find_mechanism l

let client_transition (state, mech, mechs) cmd = match state, cmd with
  | `Waiting_for_data, `Data(data) ->
      ClientTransition(
        match mech#data data with
          | Continue(resp) -> `Data(resp), (`Waiting_for_data, mech, mechs)
          | OK(resp)       -> `Data(resp), (`Waiting_for_ok,   mech, mechs)
          | Error(msg)     -> `Error(msg), (`Waiting_for_data, mech, mechs))

  | _, `Rejected(supported_mechanisms) ->
      mech#shutdown;
      begin match find_mechanism
        (List.filter (fun (name, _) -> List.mem name supported_mechanisms) mechs)
      with
        | Some(x) -> ClientTransition(x)
        | None    -> ClientFailure "no working mechanism found"
      end

  | `Waiting_for_reject, _ -> ClientFailure "protocol error"

  | _, `OK(guid) -> ClientFinal

  | `Waiting_for_ok, `Data _
  | _, `Error _ -> ClientTransition(`Cancel, (`Waiting_for_reject, mech, mechs))

let client_machine_exec recv send =
  let rec aux state =
    try
      match
        let cmd = client_transition state (recv ()) with
        | ClientFinal -> send `Begin; true
        | ClientTransition(cmd, state) -> send cmd; aux state
        | ClientFailure(_) -> false
    with
      | Failure _ ->
          send (`Error "parsing error");
          aux state
  in aux

let hexstring_of_data buf str =
  List.iter (fun c -> Printf.bprintf buf "%02x" (int_of_char c)) str;

let marshal_client_command buf = function
  | `Auth(mechanism, data) ->
      Buffer.add_string buf "AUTH ";
      Buffer.add_string buf mechanism;
      Buffer.add_char buf ' ';
      hexstring_of_data buf data
  | `Cancel -> Buffer.add_string buf "CANCEL"
  | `Begin -> Buffer.add_string buf "BEGIN"
  | `Data(data) ->
      Buffer.add_string buf "DATA ";
      hexstring_of_data buf data
  | `Error(message) ->
      Buffer.add_string buf "ERROR ";
      Buffer.add_string buf message

let launch transport =
  let lexbuf = transport#lexbuf in

  let send command =
    let buf = Buffer.create 42 in
      marshal_client_command buf command;
      Buffer.add_string buf "\r\n";
      let line = Buffer.contents buf in
        transport#send line 0 (String.length line)

  and recv () =
    AuthLexer.command lexbuf
  in

    match find_mechanism (Protected.get makers) with
      | Some(cmd, state) ->
          sendline "\x00";
          send_command cmd;
          client_machine_exec recv_command send_command state
      | None ->
          ()
