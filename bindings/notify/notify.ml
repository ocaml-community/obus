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

let name = "org.freedesktop.Notifications"
let path = ["org"; "freedesktop"; "Notifications"]

include OBus_interface.Make(struct let name = "org.freedesktop.Notifications" end)

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

type id_desc = {
  id_id : int32;
  mutable id_deleted : bool;
  id_on_action : string -> unit;
  id_on_closed : unit -> unit;
}

type 'a id = id_desc * 'a Lwt.t

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

OBUS_method GetServerInformation : server_info
OBUS_method GetCapabilities : string list

OBUS_method Notify : string -> uint32 -> string -> string -> string -> string list -> hint list -> int -> uint32
OBUS_method CloseNotification : uint32 -> unit

(* Signals sent by the notification daemon are not braodcasted and it is an
   error to match on the bus for them because there is no way to distinguish
   between signals destined to us and signals destined to other application *)
OBUS_signal! NotificationClosed : uint32
OBUS_signal! ActionInvoked : uint32 * string

let assoc x l =
  try Some(List.assoc x l) with
      Not_found -> None

let init =
  lazy(perform
         bus <-- Lazy.force OBus_bus.session;
         let daemon = OBus_bus.make_proxy bus name path in

         (* Handle signals for closed notifications *)
         OBus_signal.connect daemon notification_closed
           (fun n ->
              match assoc n !ids with
                | Some id ->
                    id.id_deleted <- true;
                    ids := List.remove_assoc n !ids;
                    id.id_on_closed ()
                | None -> ());

         (* Handle signals for actions *)
         OBus_signal.connect daemon action_invoked
           (fun (n, key) ->
              match assoc n !ids with
                | Some id ->
                    id.id_deleted <- true;
                    ids := List.remove_assoc n !ids;
                    id.id_on_action key
                | None -> ());

         return daemon)

let get_server_information _ =
  Lazy.force init >>= get_server_information

let get_capabilities _ =
  Lazy.force init >>= get_capabilities

let close_notification (id, w) =
  if not id.id_deleted then begin
    id.id_deleted <- true;
    ids := List.remove_assoc id.id_id !ids;
    id.id_on_closed ();
    Lazy.force init >>= fun p -> close_notification p id.id_id
  end else return ()

let de = desktop_entry

(* Survive to replacement/crash of the notification daemon *)
(*let setup_monitor_daemon =
  lazy(perform
         bus <-- Lazy.force OBus_bus.session;
         OBus_bus.on_service_status_change bus "org.freedesktop.Notifications"
           (fun _ ->
              List.iter (fun (_, id) -> id.id_on_closed ()) !ids;
              ids := []);
         return ())*)

let result (id, w) = w

let notify ?(app_name= !app_name) ?desktop_entry
    ?replace ?(icon="") ?image ~summary ?(body="") ?(actions=[])
    ?urgency ?category ?sound_file ?suppress_sound ?pos ?(hints=[]) ?(timeout= -1) () =

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
      (fun (text, user_key) (acc, al, am) ->
         (* For each action, generate a key and associate it to the
            given function *)
         let key = Printf.sprintf "key%d" acc in
         (acc + 1, key :: text :: al, (key, user_key) :: am))
      actions (0, [], []) in

  perform
    (* Retreive the daemon *)
    daemon <-- Lazy.force init;

    (* Create the notification *)
    n <-- notify daemon
      app_name (match replace with
                  | Some (id, w) -> id.id_id
                  | None -> 0l)
      icon summary body actions_list hints timeout;

    let w = wait () in
    let id = { id_id = n;
               id_deleted = false;
               id_on_action = (fun action -> match assoc action actions_map with
                                 | Some k -> wakeup w k
                                 | None -> wakeup w `Closed);
               id_on_closed = (fun _ -> wakeup w `Closed) } in

    let _ = ids := (n, id) :: !ids in

    return (id, w)
