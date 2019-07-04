(*
 * notification.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt_react
open Lwt
open OBus_value

let app_name = ref (Filename.basename Sys.argv.(0))
let desktop_entry = ref None

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type server_info = {
  server_name : string;
  server_vendor : string;
  server_version : string;
  server_spec_version : string;
}

type image = {
  img_width : int;
  img_height : int;
  img_rowstride : int;
  img_has_alpha : bool;
  img_bits_per_sample : int;
  img_channels : int;
  img_data : string;
}

type urgency = [ `Low | `Normal | `Critical ]

type id = int32
    (* An notification id *)

(* All informations about an opened notification *)
type notification = {
  mutable notif_deleted : bool;
  (* Wether the notification as already been closed *)

  notif_action : string -> unit;
  (* Wakeup the waiting thread when an action is received *)

  notif_closed : unit -> unit;
  (* Wakeup the waiting thread with [`Closed] when a notification is
     closed *)
}

type 'a t = {
  result : 'a Lwt.t;
  notification : notification;
  peer : OBus_peer.t;
  id : id;
}

module Peer_map = Map.Make(OBus_peer)
module Id_map = Map.Make(Int32)

let notifications : notification Id_map.t ref Peer_map.t ref = ref Peer_map.empty
  (* All opened notifications, by peer then id *)

let default_action = "default"
  (* Default action for notifications *)

(* +-----------------------------------------------------------------+
   | D-Bus methods and signals                                       |
   +-----------------------------------------------------------------+ *)

let server_name = "org.freedesktop.Notifications"
let server_path = ["org"; "freedesktop"; "Notifications"]

open Notification_interfaces.Org_freedesktop_Notifications

let proxy = lazy(
  let%lwt bus = OBus_bus.session () in
  return (OBus_proxy.make (OBus_peer.make bus server_name) server_path)
)

let get_server_information () =
  let%lwt proxy = Lazy.force proxy in
  let%lwt name, vendor, version, spec_version = OBus_method.call m_GetServerInformation proxy () in
  return {
    server_name = name;
    server_vendor = vendor;
    server_version = version;
    server_spec_version = spec_version;
  }

let get_capabilities () =
  let%lwt proxy = Lazy.force proxy in
  OBus_method.call m_GetCapabilities proxy ()

let notify proxy ~app_name ~id ~icon ~summary ~body ~actions ~hints ~timeout =
  let%lwt context, return_id = OBus_method.call_with_context m_Notify proxy (app_name, id, icon, summary, body, actions, hints, Int32.of_int timeout) in
  return (OBus_context.sender context, return_id)

let close_notification proxy id =
  OBus_method.call m_CloseNotification proxy id

let s_NotificationClosed =
  OBus_member.Signal.make
    ~interface:"org.freedesktop.Notifications"
    ~member:"NotificationClosed"
    ~args:(arg2
             (None, C.basic_uint32)
             (None, C.basic_uint32))
    ~annotations:[]

let notification_closed proxy =
  OBus_signal.make s_NotificationClosed proxy

let s_ActionInvoked =
  OBus_member.Signal.make
    ~interface:"org.freedesktop.Notifications"
    ~member:"ActionInvoked"
    ~args:(arg2
             (None, C.basic_uint32)
             (None, C.basic_string))
    ~annotations:[]

let action_invoked proxy =
  OBus_signal.make s_ActionInvoked proxy

(* +-----------------------------------------------------------------+
   | Notifications monitoring                                        |
   +-----------------------------------------------------------------+ *)

let monitor_peer peer =
  ignore begin
    let%lwt () = OBus_peer.wait_for_exit peer in
    let m = Peer_map.find peer !notifications in
    notifications := Peer_map.remove peer !notifications;
    (* Cancel all opened notification opened on this peer: *)
    Id_map.iter (fun id notif -> notif.notif_closed ()) !m;
    return ()
  end

let remove_notification peer id notif =
  notif.notif_deleted <- true;
  let r = Peer_map.find peer !notifications in
  r :=  Id_map.remove id !r

let init_callbacks = lazy(
  let%lwt bus = OBus_bus.session () in

  (* Create an anymous proxy for connecting signals, so we will
     receive signals comming from any daemon *)
  let anonymous_proxy = { OBus_proxy.peer = OBus_peer.anonymous bus;
                          OBus_proxy.path = server_path } in

  let%lwt event =
    OBus_signal.connect
      (OBus_signal.map_with_context
         (fun context (id, reason) -> (OBus_context.sender context, id, reason))
         (notification_closed anonymous_proxy))
  in

  (* Handle signals for closed notifications *)
  E.keep
    (E.map_p
       (fun (peer, id, reason) ->
          match try Some(Peer_map.find peer !notifications) with Not_found -> None with
            | None ->
                return ()
            | Some m ->
                match try Some(Id_map.find id !m) with Not_found -> None with
                  | None ->
                      return ()
                  | Some notif ->
                      remove_notification peer id notif;
                      notif.notif_closed ();
                      return ())
       event);

  let%lwt event =
    OBus_signal.connect
      (OBus_signal.map_with_context
         (fun context (id, action) -> (OBus_context.sender context, id, action))
         (action_invoked anonymous_proxy))
  in

  (* Handle signals for actions *)
  E.keep
    (E.map_p
       (fun (peer, id, action) ->
          match try Some(Peer_map.find peer !notifications) with Not_found -> None with
            | None ->
                return ()
            | Some m ->
                match try Some(Id_map.find id !m) with Not_found -> None with
                  | None ->
                      return ()
                  | Some notif ->
                      remove_notification peer id notif;
                      notif.notif_action action;
                      return ())
       event);

  return ()
)

(* +-----------------------------------------------------------------+
   | Operations on notifications                                     |
   +-----------------------------------------------------------------+ *)

let result n = n.result

let close n =
  let notif = n.notification in
  if not notif.notif_deleted then begin
    remove_notification n.peer n.id notif;
    notif.notif_closed ();
    (* Call the method on the peer which have opened the
       notification *)
    close_notification (OBus_proxy.make n.peer server_path) n.id
  end else
    return ()

(* +-----------------------------------------------------------------+
   | Openning notifications                                          |
   +-----------------------------------------------------------------+ *)

let rec filter_opt = function
  | [] -> []
  | Some x :: l ->
      x :: filter_opt l
  | None :: l ->
      filter_opt l

let default_desktop_entry = desktop_entry

let notify ?(app_name= !app_name) ?desktop_entry
    ?replace ?(icon="") ?image ~summary ?(body="") ?(actions=[])
    ?urgency ?category ?sound_file ?suppress_sound ?pos ?(hints=[]) ?(timeout= -1) () =

  let desktop_entry =
    match desktop_entry with
      | None -> !default_desktop_entry
      | x -> x
  in

  (*** Creation of hints ***)
  let make_hint name x f =
    match x with
      | Some x -> Some(name, f x)
      | None -> None
  in
  let hints =
    filter_opt
      [make_hint "desktop-entry" desktop_entry V.basic_string;
       make_hint "image_data" image
         (fun image ->
            V.structure
              [V.basic_int32 (Int32.of_int image.img_width);
               V.basic_int32 (Int32.of_int image.img_height);
               V.basic_int32 (Int32.of_int image.img_rowstride);
               V.basic_boolean image.img_has_alpha;
               V.basic_int32 (Int32.of_int image.img_bits_per_sample);
               V.basic_int32 (Int32.of_int image.img_channels);
               V.byte_array image.img_data]);
       make_hint "urgency" urgency
         (fun urgency ->
            V.basic_int32 (match urgency with
                             | `Low -> 0l
                             | `Normal -> 1l
                             | `Critical -> 2l));
       make_hint "category" category V.basic_string;
       make_hint "sound-file" sound_file V.basic_string;
       make_hint "suppress-sound" suppress_sound V.basic_boolean;
       make_hint "x" pos (fun (x, y) -> V.basic_int32(Int32.of_int x));
       make_hint "y" pos (fun (x, y) -> V.basic_int32(Int32.of_int y))]
    @ hints in

  (*** Handling of actions ***)
  let _, actions, actions_map =
    List.fold_right
      (fun (text, user_key) (acc, al, am) ->
         (* For each action, generate a key and associate it to the
            given function *)
         let key = Printf.sprintf "key%d" acc in
         (acc + 1, key :: text :: al, (key, user_key) :: am))
      actions (0, [], []) in
  let actions_map = (default_action, `Default) :: actions_map in

  (* Setup callbacks *)
  let%lwt () = Lazy.force init_callbacks in

  (* Get the proxy *)
  let%lwt daemon = Lazy.force proxy in

  (* Create the notification *)
  let%lwt peer, id =
    notify
      daemon
      ~app_name
      ~id:(match replace with
             | Some n -> n.id
             | None -> 0l)
      ~icon
      ~summary
      ~body
      ~actions
      ~hints
      ~timeout
  in

  let waiter, wakener = wait () in
  let notif = {
    notif_deleted = false;
    notif_action = (fun action ->
                      wakeup wakener (try
                                        List.assoc action actions_map
                                      with Not_found ->
                                        `Default));
    notif_closed = (fun () -> wakeup wakener `Closed);
  } in

  begin
    try
      let r = Peer_map.find peer !notifications in
      r := Id_map.add id notif !r
    with Not_found ->
      notifications :=
        Peer_map.add
          peer
          (ref (Id_map.add id notif Id_map.empty))
          !notifications;
      (* Monitor the peer to be sure the notification is closed when
         the peer exits *)
      monitor_peer peer
  end;

  return {
    result = waiter;
    notification = notif;
    peer = peer;
    id = id;
  }
