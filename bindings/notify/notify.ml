(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type
open OBus_value

include OBus_client.Make_constant
  (struct
     let name = "org.freedesktop.Notifications"
     let path = "/org/freedesktop/Notifications"
     let service = Some name
     let bus = OBus_bus.session
   end)

OBUS_record server_info = {
  server_name : string;
  server_vendor : string;
  server_version : string;
  server_spec_version : string;
}

OBUS_struct image = {
  img_width : int;
  img_height : int;
  img_rowstride : int;
  img_has_alpha : bool;
  img_bits_per_sample : int;
  img_channels : int;
  img_data : byte_array;
}

OBUS_flag urgency : uint8 =
  [ 0 -> `low
  | 1 -> `normal
  | 2 -> `critical ]

OBUS_flag closed_reason : uint =
    [ 1 -> `expired
    | 2 -> `closed_by_user
    | 3 -> `closed_explicitly
    | 4 -> `undefined ]

type id = {
  id_id : int32;
  mutable id_deleted : bool;
  id_actions : (string * (unit -> unit)) list;
  id_on_close : (unit -> unit) option;
  id_wakeup : (unit -> unit) option;
}

let ids = ref []

type hint =
  | Hint_image of image
  | Hint_variant of string * single

let thint = wrap_element (tdict_entry tstring tvariant)
  (fun (name, value) ->
     match name with
       | "image_data" -> begin match opt_cast_single timage value with
           | None -> Hint_variant(name, value)
           | Some img -> Hint_image img
         end
       | _ -> Hint_variant(name, value))
  (function
     | Hint_image img -> ("image_data", make_single timage img)
     | Hint_variant(name, value) -> (name, vvariant value))

let app_name = ref (Filename.basename Sys.argv.(0))
let desktop_entry = ref None

open Lwt

let call_func = function
  | Some f -> f ()
  | None -> ()

let get_server_information = call "GetServerInformation" << unit -> server_info >>
let get_capabilities = call "GetCapabilities" << unit -> string list >>
let close_notification id =
  if not id.id_deleted then begin
    id.id_deleted <- true;
    ids := List.remove_assoc id.id_id !ids;
    call_func id.id_wakeup;
    call "CloseNotification" << uint32 -> unit >> id.id_id
  end else return ()

let de = desktop_entry

let assoc x l =
  try Some(List.assoc x l) with
      Not_found -> None

let setup_closed_handler =
  lazy(on_signal ~global:false "NotificationClosed" << uint32 -> unit >>
        (fun n ->
           match assoc n !ids with
             | Some id ->
                 id.id_deleted <- true;
                 ids := List.remove_assoc n !ids;
                 call_func id.id_on_close;
                 call_func id.id_wakeup
             | None -> ()) >>= fun _ -> return ())

let setup_actions_handler =
  lazy(on_signal ~global:false "ActionInvoked" << uint32 -> string -> unit >>
        (fun n key ->
           match assoc n !ids with
             | Some id ->
                 id.id_deleted <- true;
                 ids := List.remove_assoc n !ids;
                 (match assoc key id.id_actions with
                    | Some f -> f ()
                    | None -> ());
                 call_func id.id_wakeup
             | None -> ()) >>= fun _ -> return ())

(* Survive to replacement/crash of the notification daemon *)
let setup_monitor_daemon =
  lazy(perform
         bus <-- Lazy.force OBus_bus.session;
         OBus_bus.on_service_status_change bus "org.freedesktop.Notifications"
           (fun _ ->
              List.iter (fun (_, id) -> call_func id.id_wakeup) !ids;
              ids := []);
         return ())

let notify ?(app_name= !app_name) ?desktop_entry
    ?replace ?(icon="") ?image ~summary ?(body="") ?(actions=[])
    ?urgency ?category ?sound_file ?suppress_sound ?pos ?(hints=[]) ?(timeout= -1) ?on_close ?wakeup () =

  (*** Creation of hints ***)
  let desktop_entry = match desktop_entry with
    | None -> !de
    | x -> x in
  let mkhint v f = match v with
    | Some x -> Some (f x)
    | None -> None in
  let mkvariant v name f = mkhint v (fun x -> Hint_variant(name, f x)) in
  let mkstring v name = mkvariant v name (make_single tstring) in
  let hints = Util.filter_map (fun x -> x)
    [mkstring desktop_entry "desktop_entry";
     mkhint image (fun img -> Hint_image img);
     mkvariant urgency "urgency" (make_single turgency);
     mkstring category "category";
     mkstring sound_file "sound-file";
     mkvariant suppress_sound "suppress-sound" (make_single tboolean);
     mkvariant pos "x" (fun (x, y) -> make_single tint x);
     mkvariant pos "y" (fun (x, y) -> make_single tint y)]
    @ List.map (fun (name, value) -> Hint_variant(name, value)) hints in

  (*** Handling of actions ***)
  let _, actions_list, actions_map =
    List.fold_right
      (fun (text, func) (acc, al, am) ->
         (* For each action, generate a key and associate it to the
            given function *)
         let key = Printf.sprintf "key%d" acc in
         (acc + 1, key :: text :: al, (key, func) :: am))
      actions (0, [], []) in

  perform
    (* Setup signal handlers only if needed *)

    if wakeup <> None || on_close <> None || actions <> []
    then
      (perform
         Lazy.force setup_closed_handler;
         Lazy.force setup_monitor_daemon)
    else return ();

    if actions <> []
    then Lazy.force setup_actions_handler
    else return ();

    (* Create the notification *)
    n <-- call "Notify" << string -> uint32 -> string -> string -> string -> string list -> hint list -> int -> uint32 >>
      app_name (match replace with
                  | Some id -> id.id_id
                  | None -> 0l)
      icon summary body actions_list hints timeout;

    let id = { id_id = n;
               id_deleted = false;
               id_actions = actions_map;
               id_on_close = on_close;
               id_wakeup = wakeup } in

    (* Register the handlers if needed *)
    let _ =
      if wakeup <> None || on_close <> None || actions <> []
      then ids := (n, id) :: !ids
    in

    return id
