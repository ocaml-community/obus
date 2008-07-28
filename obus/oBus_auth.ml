(*
 * oBus_auth.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

let ($) a b = a b
let (|>) a b x = b (a x)
let (>>) a b = a >>= (fun _ -> b)

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

type mechanism_return =
  | Continue of data
  | OK of data
  | Error of string

type mechanism_handlers = {
  mech_init : mechanism_return;
  mech_data : data -> mechanism_return;
  mech_shutdown : unit -> unit;
}
type mechanism = string * (unit -> mechanism_handlers)

let make ~init ?(data=fun _ -> Error("no data expected for this mechanism"))
    ?(shutdown=fun _ -> ()) () =
  { mech_init = init;
    mech_data = data;
    mech_shutdown = shutdown }

(* Predefined mechanisms *)

let mech_external = ("EXTERNAL",
                     fun () ->
                       make ~init:(OK (string_of_int (Unix.getuid ()))) ())

let default_mechanisms = [ mech_external ]

(* The protocol state machine for the client *)

type client_machine_state = client_state * mechanism_handlers * mechanism list
type client_machine_transition =
  | ClientTransition of (client_command * client_machine_state)
  | ClientFinal of guid
  | ClientFailure of string

(* Transitions *)

let rec find_mechanism = function
  | [] -> None
  | (name, create_mech) :: mechs ->
      try
        let mech = create_mech () in
          match mech.mech_init with
            | Continue(resp) -> Some(`Auth(name, resp), (`Waiting_for_data, mech, mechs))
            | OK(resp)       -> Some(`Auth(name, resp), (`Waiting_for_ok,   mech, mechs))
            | Error(_)       -> find_mechanism mechs
      with
          _ -> find_mechanism mechs

let client_transition (state, mech, mechs) cmd = match state, cmd with
  | `Waiting_for_data, `Data(data) ->
      ClientTransition(
        match mech.mech_data data with
          | Continue(resp) -> `Data(resp), (`Waiting_for_data, mech, mechs)
          | OK(resp)       -> `Data(resp), (`Waiting_for_ok,   mech, mechs)
          | Error(msg)     -> `Error(msg), (`Waiting_for_data, mech, mechs))

  | _, `Rejected(supported_mechanisms) ->
      mech.mech_shutdown ();
      begin match find_mechanism
        (List.filter (fun (name, _) -> List.mem name supported_mechanisms) mechs)
      with
        | Some(x) -> ClientTransition(x)
        | None    -> ClientFailure "no working mechanism found"
      end

  | `Waiting_for_reject, _ -> ClientFailure "protocol error"

  | _, `OK(guid) -> ClientFinal(guid)

  | `Waiting_for_ok, `Data _
  | _, `Error _ -> ClientTransition(`Cancel, (`Waiting_for_reject, mech, mechs))

let client_machine_exec recv send =
  let rec aux state =
    catch (fun  _ -> recv () >>= fun x -> return $ Some x)
      (function
         | Failure _ -> send (`Error "parsing error") >> return None
         | exn -> fail exn) >>= function
        | None -> aux state
        | Some cmd ->
            match client_transition state cmd with
              | ClientFinal(guid) -> send `Begin >> (return $ Some guid)
              | ClientTransition(cmd, state) -> send cmd >> aux state
              | ClientFailure(_) -> return None
  in aux

let hexstring_of_data buf str =
  String.iter (fun c -> Printf.bprintf buf "%02x" (int_of_char c)) str

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

let launch ?(mechanisms=default_mechanisms) transport =
  let ic = Lwt_chan.make_in_channel
    (if OBus_info.debug
     then
       (fun buf pos count ->
          OBus_transport.recv transport buf pos count
          >>= fun count ->
            DEBUG("received: %S" (String.sub buf 0 count));
            return count)
     else
       (fun buf pos count -> OBus_transport.recv transport buf pos count))
  in

  let send command =
    let buf = Buffer.create 42 in
      marshal_client_command buf command;
      Buffer.add_string buf "\r\n";
      let line = Buffer.contents buf in
      let len = String.length line in
        DEBUG("sending: %S" line);
        OBus_transport.send_exactly transport line 0 len

  and recv () =
    Lwt_chan.input_line ic >>=
      (fun l ->
         try
           return $ Auth_lexer.command (Lexing.from_string l)
         with
             exn -> Lwt.fail exn)
  in

    Lwt.finalize
      (fun _ ->match find_mechanism mechanisms with
         | Some(cmd, state) ->
             (perform
                OBus_transport.send_exactly transport "\x00" 0 1;
                send cmd;
                client_machine_exec recv send state);
         | None -> return None)
      (fun _ -> Lwt_chan.close_in ic)
