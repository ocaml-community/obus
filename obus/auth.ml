(*
 * auth.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Unix

type auth_result =
  | Success
  | Failure of string

type client_state =
  [ `Waiting_for_data
  | `Waiting_for_ok
  | `Waiting_for_reject ]

type data = string

type client_command =
  [ `Auth of string * data
  | `Cancel
  | `Begin
  | `Data of data
  | `Error of string ]

type guid = string

type server_command =
    [ `Rejected of string list
    | `OK of data
    | `Data of data
    | `Error of string ]

(* Parsing *)

let separator_regex = Str.regexp " +"
let eol_delim = "\r\n"

exception ParseError of string

let int_of_hex = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' | 'a' -> 10
  | 'B' | 'b' -> 11
  | 'C' | 'c' -> 12
  | 'D' | 'd' -> 13
  | 'E' | 'e' -> 14
  | 'F' | 'f' -> 15
  | _ -> raise (ParseError "non hex-digit in data")

let hex_of_int = function
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'a'
  | 11 -> 'b'
  | 12 -> 'c'
  | 13 -> 'd'
  | 14 -> 'e'
  | 15 -> 'f'
  | _ -> '\x00'

let data_of_hexstring data =
  let len = String.length data / 2 in
  let s = String.make len '\x00' in
    for i = 0 to len - 1 do
      s.[i] <- char_of_int ((int_of_hex data.[i*2] lsl 4) lor
                              (int_of_hex data.[i*2+1]))
    done;
    s

let hexstring_of_data str =
  let len = String.length str in
  let s = String.make (len * 2) '\x00' in
    for i = 0 to len - 1 do
      let ascii_code = int_of_char str.[i] in
        s.[i*2] <- hex_of_int (ascii_code lsr 4);
        s.[i*2+1] <- hex_of_int (ascii_code land 0x0f);
    done;
    s

let parse_command line =
  (* Check for ascii-only *)
  String.iter
    (fun ch -> if ch = '\x00' or ch >= '\x80' then
       raise (ParseError "non-A`II character in command"))
    line;

  match Str.bounded_split separator_regex line 2 with
    | [command;argument] -> command, argument
    | [command] -> command, ""
    | _ -> line, "" (* never append *)

let server_command_of_string command argument = match command with
  | "REJECTED" -> `Rejected(Str.split separator_regex argument)
  | "OK" -> `OK(data_of_hexstring argument)
  | "DATA" -> `Data(data_of_hexstring argument)
  | "ERROR" -> `Error(argument)
  | _ -> raise (ParseError "unknown command")

let string_of_client_command = function
  | `Auth(mechanism, data) -> "AUTH " ^ mechanism ^ " " ^ (hexstring_of_data data)
  | `Cancel -> "CANCEL"
  | `Begin -> "BEGIN"
  | `Data(data) -> "DATA " ^ (hexstring_of_data data)
  | `Error(message) -> "ERROR " ^ message

(* Auth mechanisms *)

type auth_mechanism_return =
  | AMRContinue of data
  | AMROK of data
  | AMRError of string

class virtual auth_mechanism = object
  method virtual init : auth_mechanism_return
  method data : data -> auth_mechanism_return =
    fun _ -> AMRError("no data expected for this mechanism")
  method shutdown = ()
end

class auth_external = object
  inherit auth_mechanism
  method init = AMROK(string_of_int (Unix.getuid ()))
end

type auth_mechanism_definition = string * (unit -> auth_mechanism)

let mechanisms = [("EXTERNAL", fun () -> new auth_external)]

(* The protocol state machine for the client *)

type client_machine_state = client_state * auth_mechanism * auth_mechanism_definition list
type client_machine_transition =
  | ClientTransition of (client_command * client_machine_state)
  | ClientFinal
  | ClientFailure of string

(* Transitions *)

let rec find_mechanism = function
  | [] -> None
  | (name, create_mech)::mechs -> let mech = create_mech () in
      match mech#init with
        | AMRContinue(resp) -> Some(`Auth(name, resp), (`Waiting_for_data, mech, mechs))
        | AMROK(resp)       -> Some(`Auth(name, resp), (`Waiting_for_ok,   mech, mechs))
        | AMRError(_)       -> find_mechanism mechs

let client_transition (state, mech, mechs) cmd = match state, cmd with
  | `Waiting_for_data, `Data(data) ->
      ClientTransition(
        match mech#data data with
          | AMRContinue(resp) -> `Data(resp), (`Waiting_for_data, mech, mechs)
          | AMROK(resp)       -> `Data(resp), (`Waiting_for_ok,   mech, mechs)
          | AMRError(msg)     -> `Error(msg), (`Waiting_for_data, mech, mechs))

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
      match client_transition state (recv ()) with
        | ClientFinal -> send `Begin; Success
        | ClientTransition(cmd, state) -> send cmd; aux state
        | ClientFailure(msg) -> Failure(msg)
    with
      | ParseError msg ->
          send (`Error msg);
          aux state
  in aux

exception TransmitionError

let do_auth fd =
  try
    let sendline line =
      let len = String.length line in
      let rec aux pos =
        match write fd line pos (len-pos) with
          | 0 -> raise TransmitionError
          | n when n < len -> aux (pos + n)
          | _ -> ()
      in aux 0 in

    let recvline =
      let buffer_size = 1024 in
      let buffer = String.create buffer_size in
      let rec aux pos =
        match read fd buffer 0 (buffer_size-pos) with
          | 0 -> raise TransmitionError
          | n -> let e = pos + n in
              if buffer.[e-2] = '\r' & buffer.[e-1] = '\n'
              then String.sub buffer 0 (e - 2)
              else aux (pos + n)
      in (fun () -> aux 0) in

    let send_command command =
      sendline (string_of_client_command command ^ eol_delim)

    and recv_command () =
      let cmd, args = parse_command (recvline ()) in
        server_command_of_string cmd args in

      match find_mechanism mechanisms with
        | Some(cmd, state) ->
            sendline "\x00";
            send_command cmd;
            client_machine_exec recv_command send_command state
        | None ->
            Failure "no mechanism available"
  with
    | TransmitionError ->
        Failure "transmition error"
