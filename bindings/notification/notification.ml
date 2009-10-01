(*
 * notification.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_value
open OBus_type.Perv

let server_name = "org.freedesktop.Notifications"
let server_path = ["org"; "freedesktop"; "Notifications"]

module OBUS_INTERFACE = OBus_interface.Make(struct let name = "org.freedesktop.Notifications" end)

type server_info = {
  server_name : string;
  server_vendor : string;
  server_version : string;
  server_spec_version : string;
} with obus

type image = {
  img_width : int;
  img_height : int;
  img_rowstride : int;
  img_has_alpha : bool;
  img_bits_per_sample : int;
  img_channels : int;
  img_data : byte_array;
} with obus

let obus_image = obus_structure obus_image

type urgency = [ `low | `normal | `critical ]

let obus_urgency = OBus_type.mapping obus_uint8 [`low, 0; `normal, 1; `critical, 2]

type server_id = uint32
 with obus
     (* An notification id as returned by the server *)

type id = OBus_peer.t * server_id
 with obus
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

  notif_id : id;
  (* Id of the notification *)

  mutable notif_deleted : bool;
  (* Wether the notification as already been closed *)

  notif_action : string -> unit;
  (* Wakeup the waiting thread when an action is received *)

  notif_closed : unit -> unit;
  (* Wakeup the waiting thread with [`closed] when a notification is
     closed *)
}

class type ['a] t = object
  method result : 'a Lwt.t
  method close : unit Lwt.t
  method id : id
end

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

let obus_hints = OBus_type.map <:obus_type< (string, variant) dict >>
  (fun l ->
     List.map (fun (name, value) ->
                 match name with
                   | "image_data" -> begin match OBus_type.opt_cast_single obus_image value with
                       | None -> Hint_variant(name, value)
                       | Some img -> Hint_image img
                     end
                   | _ -> Hint_variant(name, value)) l)
  (fun l ->
     List.map (function
                 | Hint_image img -> ("image_data", OBus_type.make_single obus_image img)
                 | Hint_variant(name, value) -> (name, value)) l)

let app_name = ref (Filename.basename Sys.argv.(0))
let desktop_entry = ref None

open Lwt

OBUS_method GetServerInformation : server_info
OBUS_method GetCapabilities : string list

OBUS_method Notify : string -> uint32 -> string -> string -> string -> string list -> hints -> int -> id
OBUS_method CloseNotification : server_id -> unit

OBUS_signal NotificationClosed : id * uint
OBUS_signal ActionInvoked : id * string

let monitored_peers : OBus_peer.t list ref = ref []
  (* List of daemon which are monitored *)

let monitor_peer peer =
  if List.mem peer !monitored_peers then
    ()

  else begin
    monitored_peers := peer :: !monitored_peers;

    ignore begin
      OBus_peer.wait_for_exit peer >>= fun _ ->
        monitored_peers := List.filter (fun p -> p <> peer) !monitored_peers;

        (* Cancel all opened notification opened on this peer when it exit *)
        notifications := List.filter
          (fun notif ->
             if fst notif.notif_id = peer then begin
               notif.notif_closed ();
               false
             end else
               true) !notifications;

        return ()
    end
  end

let init_callbacks =
  lazy(lwt bus = Lazy.force OBus_bus.session in

       (* Create an anymous proxy for connecting signals, so we will
          receive signals comming from any daemon *)
       let anonymous_proxy = { OBus_proxy.peer = { OBus_peer.connection = bus;
                                                   OBus_peer.name = None };
                               OBus_proxy.path = server_path } in

       (* Handle signals for closed notifications *)
       let _ = Lwt_event.notify_p
         (fun (id, reason) ->
            match find_notification id with
              | Some notif ->
                  notif.notif_deleted <- true;
                  notifications := List.filter (fun n -> n.notif_id <> id) !notifications;
                  notif.notif_closed ();

                  return ()
              | None ->
                  return ()) (notification_closed anonymous_proxy)#event in

       (* Handle signals for actions *)
       let _ = Lwt_event.notify_p
         (fun (id, action) ->
            match find_notification id with
              | Some notif ->
                  notif.notif_deleted <- true;
                  notifications := List.filter (fun n -> n.notif_id <> id) !notifications;
                  notif.notif_action action;
                  return ()
              | None ->
                  return ()) (action_invoked anonymous_proxy)#event in

       return ())

let get_proxy =
  lazy(lwt bus = Lazy.force OBus_bus.session in
       return (OBus_proxy.make (OBus_peer.make bus server_name) server_path))

let get_server_information _ =
  Lazy.force get_proxy >>= get_server_information

let get_capabilities _ =
  Lazy.force get_proxy >>= get_capabilities

let rec filter_map f = function
  | [] -> []
  | x :: l -> match f x with
      | Some x -> x :: filter_map f l
      | None -> filter_map f l

let default_desktop_entry = desktop_entry

let notify ?(app_name= !app_name) ?desktop_entry
    ?replace ?(icon="") ?image ~summary ?(body="") ?(actions=[])
    ?urgency ?category ?sound_file ?suppress_sound ?pos ?(hints=[]) ?(timeout= -1) () =

  (*** Creation of hints ***)
  let desktop_entry = match desktop_entry with
    | None -> !default_desktop_entry
    | x -> x in
  let mkhint v f = match v with
    | Some x -> Some (f x)
    | None -> None in
  let mkvariant v name f = mkhint v (fun x -> Hint_variant(name, f x)) in
  let mkstring v name = mkvariant v name (fun x -> basic(String x)) in
  let hints = filter_map (fun x -> x)
    [mkstring desktop_entry "desktop_entry";
     mkhint image (fun img -> Hint_image img);
     mkvariant urgency "urgency" (OBus_type.make_single obus_urgency);
     mkstring category "category";
     mkstring sound_file "sound-file";
     mkvariant suppress_sound "suppress-sound" (fun x -> basic(Boolean x));
     mkvariant pos "x" (fun (x, y) -> basic(Int32(Int32.of_int x)));
     mkvariant pos "y" (fun (x, y) -> basic(Int32(Int32.of_int y)))]
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

  (* Setup callbacks *)
  lwt () = Lazy.force init_callbacks in

  (* Get the proxy *)
  lwt daemon = Lazy.force get_proxy in

  (* Create the notification *)
  lwt id = notify daemon
    app_name (match replace with
                | Some notif -> snd notif#id
                | None -> 0l)
    icon summary body actions_list hints timeout
  in

  let waiter, wakener = wait () in
  let notif = { notif_id = id;
                notif_deleted = false;
                notif_action = (fun action ->
                                  wakeup wakener (try
                                                    List.assoc action actions_map
                                                  with Not_found ->
                                                    `default));
                notif_closed = (fun _ -> wakeup wakener `closed) } in

  let _ = notifications := notif :: !notifications in

  (* Monitor the peer to be sure the notification is closed when the
     peer exit *)
  let _ = monitor_peer (fst id) in

  return (object
            method result = waiter
            method close =
              if not notif.notif_deleted then begin
                notif.notif_deleted <- true;
                notifications := List.filter (fun n -> n.notif_id <> notif.notif_id) !notifications;
                notif.notif_closed ();
                (* Call the method on the peer which have opened the
                   notification *)
                close_notification (OBus_proxy.make (fst notif.notif_id) server_path) (snd notif.notif_id)
              end else
                return ()
            method id = id
          end)
