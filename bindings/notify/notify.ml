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

let server_name = "org.freedesktop.Notifications"
let server_path = ["org"; "freedesktop"; "Notifications"]

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

OBUS_type notification_server_id = uint32
    (* An notification id as returned by the server *)

OBUS_type notification_id = OBus_peer.t * notification_server_id
    (* We consider that an id is the pair of the id given by the
       daemon plus the peer of the daemon. This is to avoid the
       following problem:

       1 - the notification daemon start
       2 - we open a notification -> id = 1
       3 - a new notification daemon starts and replaces the first one
       4 - we open a notification -> id = 1 (ARGL!!)
    *)

type notification = {
  (* All information about an opened notification *)

  notif_id : notification_id;
  (* Id of the notification *)

  mutable notif_deleted : bool;
  (* Wether the notification as already been closed *)

  notif_action : string -> unit;
  (* Wakeup the waiting thread when an action is received *)

  notif_closed : unit -> unit;
  (* Wakeup the waiting thread with [`closed] when a notification is
     closed *)
}

type 'a id = notification * 'a Lwt.t
    (* The id given to the user: the notification description plus a
       thread waiting for the notification to be closed/clicked *)

let notifications : notification list ref = ref []
  (* All opened notifications *)

let default_action = "default"
  (* Default action for notifications *)

let find_notification id =
  let rec aux = function
    | [] -> None
    | n :: l ->
        if n.notif_id = id then
          Some n
        else
          aux l
  in
  aux !notifications

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

OBUS_method Notify : string -> uint32 -> string -> string -> string -> string list -> hint list -> int -> notification_id
OBUS_method CloseNotification : notification_server_id -> unit

OBUS_signal NotificationClosed : notification_id
OBUS_signal ActionInvoked : notification_id * string

let init =
  lazy(perform
         bus <-- Lazy.force OBus_bus.session;

         OBus_resolver.make (OBus_bus.connection bus) server_name
           ~on_change:begin function
             | Some name ->
                 let daemon = OBus_bus.make_peer bus name in

                 OBus_peer.wait_for_exit daemon >>= fun _ ->
                   (* Cancel all opened notification opened on this peer when it exit *)
                   notifications := List.filter
                     (fun notif ->
                        if fst notif.notif_id = daemon then begin
                          notif.notif_closed ();
                          false
                        end else
                          true) !notifications;
                   return ()

                 | None ->
                     return ()
           end;

         (* Create an anymous proxy for connecting signals, so we will
            receive signals comming from any daemon *)
         let anonymous_proxy = { OBus_proxy.peer = { OBus_peer.connection = OBus_bus.connection bus;
                                                     OBus_peer.name = None };
                                 OBus_proxy.path = server_path } in

         (* Handle signals for closed notifications *)
         OBus_signal.connect anonymous_proxy notification_closed
           (fun id ->
              match find_notification id with
                | Some notif ->
                    notif.notif_deleted <- true;
                    notifications := List.filter (fun n -> n.notif_id <> id) !notifications;
                    notif.notif_closed ();
                    return ()
                | None ->
                    return ());

         (* Handle signals for actions *)
         OBus_signal.connect anonymous_proxy action_invoked
           (fun (id, action) ->
              match find_notification id with
                | Some notif ->
                    notif.notif_deleted <- true;
                    notifications := List.filter (fun n -> n.notif_id <> id) !notifications;
                    notif.notif_action action;
                    return ()
                | None ->
                    return ());

         return (OBus_bus.make_proxy bus server_name server_path))

let get_server_information _ =
  Lazy.force init >>= get_server_information

let get_capabilities _ =
  Lazy.force init >>= get_capabilities

let close_notification (notif, w) =
  if not notif.notif_deleted then begin
    notif.notif_deleted <- true;
    notifications := List.filter (fun n -> n.notif_id <> notif.notif_id) !notifications;
    notif.notif_closed ();
    Lazy.force init >>= fun p -> close_notification p (snd notif.notif_id)
  end else return ()

let de = desktop_entry

let result (id, w) = w

let rec filter_map f = function
  | [] -> []
  | x :: l -> match f x with
      | Some x -> x :: filter_map f l
      | None -> filter_map f l

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
  let hints = filter_map (fun x -> x)
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
  let actions_map = (default_action, `default) :: actions_map in

  perform
    (* Retreive the daemon *)
    daemon <-- Lazy.force init;

    (* Create the notification *)
    id <-- notify daemon
      app_name (match replace with
                  | Some (notif, w) -> snd notif.notif_id
                  | None -> 0l)
      icon summary body actions_list hints timeout;

    let w = wait () in
    let notif = { notif_id = id;
                  notif_deleted = false;
                  notif_action = (fun action ->
                                    wakeup w (try
                                                List.assoc action actions_map
                                              with
                                                  Not_found -> `default));
                  notif_closed = (fun _ -> wakeup w `closed) } in

    let _ = notifications := notif :: !notifications in

    return (notif, w)
