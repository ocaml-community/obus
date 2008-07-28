(*
 * oBus_bus.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_conv

include OBus_client.Make_service
  (struct
     let name = "org.freedesktop.DBus"
   end).Make_interface
  (struct
     let interface = "org.freedesktop.DBus"
     type t = connection
     let make = Proxy.make ~path:"/org/freedesktop/DBus"
     let to_proxy bus = Proxy.make bus ~path:"/org/freedesktop/DBus"
   end)

type name = string

let ob_name = ob_string

let hello = call "Hello" [: unit -> name :]

let name bus = Connection.get_name (hello bus) bus
let register_connection connection = ignore (name connection)

let connect addresses =
  let bus = Connection.of_addresses addresses ~shared:true in
    register_connection bus;
    bus

let session () = connect (Address.session ())
let system () = connect (Address.system ())

exception Name_has_owner = "Error.NameHasNoOwner"
exception Match_rule_not_found = "Error.MatchRuleNotFound";;

obus_bitwise request_name_flag [uint] =
  [ 1 -> `allow_replacement
  | 2 -> `replace_existing
  | 4 -> `do_not_queue ];;

obus_flag request_name_result [uint] =
  [ 1 -> `primary_owner
  | 2 -> `in_queue
  | 3 -> `exists
  | 4 -> `already_owner ]

let request_name = call "RequestName" [: string -> request_name_flag_list -> request_name_result :]

type start_service_flag;;
let ob_start_service_flag =
  OBus_conv.from_wire OBus_annot.duint
    (OBus_wire.Reader.failwith "not implemented")
    (fun _ -> OBus_wire.wuint 0)

obus_flag start_service_by_name_result [uint] =
    [ 1 -> `success
    | 2 -> `already_running ]

let start_service_by_name = call "StartServiceByName" [: string -> start_service_flag -> start_service_by_name_result :]
let name_has_owner = call "NameHasOwner" [: string --> bool :]
let list_names = call "ListNames" [: name list :]
let list_activables_names = call "ListActivablesNames" [: name list :]
let get_name_owner = call "GetNameOwner" [: name -> name :]
let list_queued_owners = call "ListQueuedOwners" [: name -> name list :]

type rule = string
let ob_rule = ob_string

let rule ?typ ?sender ?interface ?member ?path ?destination ?(args=[]) () =
  let buf = Buffer.create 42 in
  let first = ref true in
  let coma () =
    if !first
    then first := false
    else Buffer.add_char buf "," in
  let add key value =
    coma ();
    Printf.bprintf buf "%s='%s'" key value in
  let add_opt key = function
    | None -> ()
    | Some x -> add key x in
    begin match typ with
      | None -> ()
      | Some t ->
          add "type"
            (match t with
               | `let_call -> "let_call"
               | `let_return -> "let_return"
               | `error -> "error"
               | `signal -> "signal")
    end;
    add_opt "sender" sender;
    add_opt "interface" interface;
    add_opt "member" member;
    add_opt "path" path;
    add_opt "destination" destination;
    List.iter (fun (n, value) -> coma (); Printf.bprintf buf "arg%d='%s'" n value) args;
    Buffer.content buf

let add_match = call "AddMatch" [: rule -> unit :]
let remove_match = call "RemoveMatch" [: rule -> unit :]
let get_connection_unix_user = call "GetConnectionUnixUser" [: string -> int :]
let get_connection_unix_processid = call "GetConnectionUnixProcessId" [: string -> int :]
let get_connection_selinux_security_context = call "GetConnectionSelinuxSecurityContext" [: string -> byte_array :]
let reload_config = call "ReloadConfig" [: unit :]
let get_id = call "GetId" [: string :]

let name_owner_changed = signal "NameOwnerChanged" [: string -> string -> string -> unit :]
let name_lost = signal "NameLost" [: string -> unit :]
let name_acquired = signal "NameAcquired" [: string -> unit :]
