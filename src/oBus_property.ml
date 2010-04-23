(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(property)"

open Lwt
open OBus_private_connection

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type 'a name_map = 'a Map.Make(String).t
type notify_data = (unit context * OBus_value.V.single) name_map

type notifier = OBus_private_connection.notifier = {
  notifier_event : notify_data React.event;
  notifier_stop : unit -> unit;
}

type notify_mode = OBus_proxy.t -> OBus_name.interface -> notifier

type ('a, 'access) t = {
  cast : unit context -> OBus_value.V.single -> 'a;
  make : 'a -> OBus_value.V.single;
  member : OBus_name.member;
  proxy : OBus_proxy.t;
  notify_mode : notify_mode;
  property_group : OBus_private_connection.property_group;
}

type 'a r = ('a, [ `readable ]) t
type 'a w = ('a, [ `writable ]) t
type 'a rw = ('a, [ `readable | `writable ]) t

(* +-----------------------------------------------------------------+
   | Transformations                                                 |
   +-----------------------------------------------------------------+ *)

let map_rw f g property = {
  property with
    cast = (fun context x -> f (property.cast context x));
    make = (fun x -> property.make (g x));
}

let map_rw_with_context f g property = {
  property with
    cast = (fun context x -> f context (property.cast context x));
    make = (fun x -> property.make (g x));
}

let map_r f property = {
  property with
    cast = (fun context x -> f (property.cast context x));
    make = (fun x -> assert false);
}

let map_r_with_context f property = {
  property with
    cast = (fun context x -> f context (property.cast context x));
    make = (fun x -> assert false);
}

let map_w g property = {
  property with
    cast = (fun context x -> assert false);
    make = (fun x -> property.make (g x));
}

(* +-----------------------------------------------------------------+
   | Methods                                                         |
   +-----------------------------------------------------------------+ *)

let interface = "org.freedesktop.DBus.Properties"

let m_Get =
  OBus_member.Method.make
    ~interface
    ~member:"Get"
    ~i_args:(OBus_value.arg2 (None, OBus_value.C.basic_string) (None, OBus_value.C.basic_string))
    ~o_args:(OBus_value.arg1 (None, OBus_value.C.variant))

let m_Set =
  OBus_member.Method.make
    ~interface
    ~member:"Set"
    ~i_args:(OBus_value.arg3 (None, OBus_value.C.basic_string) (None, OBus_value.C.basic_string) (None, OBus_value.C.variant))
    ~o_args:OBus_value.arg0

let m_GetAll =
  OBus_member.Method.make
    ~interface
    ~member:"GetAll"
    ~i_args:(OBus_value.arg1 (None, OBus_value.C.basic_string))
    ~o_args:(OBus_value.arg1 (None, OBus_value.C.dict OBus_value.C.string OBus_value.C.variant))

(* +-----------------------------------------------------------------+
   | Reading all properties with their contexts                      |
   +-----------------------------------------------------------------+ *)

let get_all_no_cache proxy interface =
  lwt context, dict = OBus_method.call_with_context m_GetAll proxy interface in
  return (List.fold_left
            (fun acc (name, variant) -> String_map.add name (context, variant) acc)
            String_map.empty
            dict)

(* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ *)

let notify_none proxy interface =
  {
    notifier_event = React.E.never;
    notifier_stop = ignore;
  }

let notify_global member proxy interface =
  let signal = OBus_signal.connect (OBus_member.Signal.make ~interface ~member ~args:OBus_value.arg0) proxy in
  {
    notifier_event =
      Lwt_event.map_s
        (fun () -> get_all_no_cache proxy interface)
        (OBus_signal.event signal);
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_update member proxy interface =
  let signal =
    OBus_signal.connect
      (OBus_member.Signal.make
         ~interface
         ~member
         ~args:(OBus_value.arg1 (None, OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)))
      proxy
  in
  {
    notifier_event =
      React.E.map
        (fun (context, updates) ->
           List.fold_left
             (fun map (member, value) ->
                String_map.add member (context, value) map)
             String_map.empty updates)
        (OBus_signal.event_with_context signal);
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_egg_dbus proxy interface =
  let signal =
    OBus_signal.connect
      (OBus_member.Signal.make
         ~interface:"org.freedesktop.DBus.Properties"
         ~member:"EggDBusChanged"
         ~args:(OBus_value.arg2
                  (None, OBus_value.C.basic_string)
                  (None, OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)))
      proxy
  in
  (* Only monitor the properties with the same interface: *)
  OBus_signal.set_filters signal [(0, OBus_match.AF_string interface)];
  {
    notifier_event =
      React.E.map
        (fun (context, (iface, updates)) ->
           List.fold_left
             (fun map (member, value) ->
                String_map.add member (context, value) map)
             String_map.empty updates)
        (OBus_signal.event_with_context signal);
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_custom f = f

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let monitorable property = property.notify_mode != notify_none

let equal_context_value (context1, value1) (context2, value2) =
  context1 == context2 && value1 = value2

let hold_properties initial event =
  React.S.fold ~eq:(String_map.equal equal_context_value)
    (fun acc updates ->
       String_map.fold
         String_map.add
         updates acc)
    initial
    event

let rec updates property =
  let property_group = property.property_group in
  match property_group.property_group_state with
    | Property_group_simple ->
        let notifier =
          property.notify_mode
            property.proxy
            property_group.property_group_interface
        in
        property_group.property_group_state <- (
          Property_group_updates notifier
        );
        updates property
    | Property_group_cached thread ->
        let notifier =
          property.notify_mode
            property.proxy
            property_group.property_group_interface
        in
        property_group.property_group_state <- (
          Property_group_monitor((lwt initial = thread in
                                  return (hold_properties initial notifier.notifier_event)),
                                 notifier)
        );
        updates property
    | Property_group_updates notifier
    | Property_group_monitor(_, notifier) ->
        React.E.map
          (fun properties ->
             let context, value = String_map.find property.member properties in
             property.cast context value)
          notifier.notifier_event

let rec monitor property =
  let _ = updates property in
  let property_group = property.property_group in
  match property_group.property_group_state with
    | Property_group_simple
    | Property_group_cached _ ->
        (* We cannot be in theses states after a call to [updates] *)
        assert false
    | Property_group_updates notifier ->
        let thread =
          lwt initial =
            get_all_no_cache
              property.proxy
              property_group.property_group_interface
          in
          return (hold_properties initial notifier.notifier_event)
        in
        property_group.property_group_state <- (
          Property_group_monitor(thread, notifier)
        );
        monitor property
    | Property_group_monitor(thread, notifier) ->
        lwt signal = thread in
        return
          (React.S.map
             (fun properties ->
                let context, value = String_map.find property.member properties in
                property.cast context value)
             signal)

let unmonitor_property_group property_group =
  match property_group.property_group_state with
    | Property_group_simple | Property_group_cached _ ->
        ()
    | Property_group_updates notifier ->
        property_group.property_group_state <- Property_group_simple;
        notifier.notifier_stop ()
    | Property_group_monitor(thread, notifier) ->
        property_group.property_group_state <- Property_group_simple;
        notifier.notifier_stop ();
        match Lwt.state thread with
          | Sleep ->
              cancel thread
          | Return signal ->
              React.S.stop signal
          | Fail exn ->
              ()

let unmonitor property =
  unmonitor_property_group property.property_group

(* +-----------------------------------------------------------------+
   | Clean up                                                        |
   +-----------------------------------------------------------------+ *)

let cleanup_property_group property_group =
  property_group.property_group_ref_count <- property_group.property_group_ref_count - 1;
  if property_group.property_group_ref_count = 0 then begin
    let running = running_of_connection property_group.property_group_connection in
    running.running_properties <- Property_map.remove (property_group.property_group_owner, property_group.property_group_path, property_group.property_group_interface) running.running_properties;
    unmonitor_property_group property_group
  end

let cleanup_property property =
  cleanup_property_group property.property_group

(* +-----------------------------------------------------------------+
   | Caching                                                         |
   +-----------------------------------------------------------------+ *)

let load_cache proxy interface =
  let connection = OBus_proxy.connection proxy in
  let running = running_of_connection connection in
  let key = (OBus_proxy.name proxy, OBus_proxy.path proxy, interface) in
  let property_group =
    match try Some(Property_map.find key running.running_properties) with Not_found -> None with
      | None ->
          let property_group = {
            property_group_ref_count = 1;
            property_group_state = Property_group_simple;
            property_group_connection = connection;
            property_group_owner = OBus_proxy.name proxy;
            property_group_path = OBus_proxy.path proxy;
            property_group_interface = interface;
          } in
          running.running_properties <- Property_map.add key property_group running.running_properties;
          property_group
      | Some property_group ->
          property_group.property_group_ref_count <- property_group.property_group_ref_count + 1;
          property_group
  in
  let thread = get_all_no_cache proxy interface in
  let () =
    match property_group.property_group_state with
      | Property_group_simple ->
          property_group.property_group_state <- Property_group_cached thread
      | Property_group_updates notifier ->
          property_group.property_group_state <- (
            Property_group_monitor((lwt initial = thread in
                                    return (hold_properties initial notifier.notifier_event)),
                                   notifier)
          )
      | _ ->
          ()
  in
  ignore begin
    lwt _ = thread in
    (* Expire the cache before the next iteration of the main loop: *)
    lwt () = pause () in
    cleanup_property_group property_group;
    match property_group.property_group_state with
      | Property_group_cached _ ->
          property_group.property_group_state <- Property_group_simple;
          return ()
      | _ ->
          return ()
  end;
  thread

(* +-----------------------------------------------------------------+
   | Property reading/writing                                        |
   +-----------------------------------------------------------------+ *)

let get_with_context ?(cache=true) property =
  let property_group = property.property_group in
  match property_group.property_group_state with
    | Property_group_simple | Property_group_updates _ ->
        if cache then begin
          lwt properties = load_cache property.proxy property_group.property_group_interface in
          let context, value  = String_map.find property.member properties in
          return (context, property.cast context value)
        end else begin
          lwt context, value =
            OBus_method.call_with_context m_Get
              property.proxy
              (property_group.property_group_interface, property.member)
          in
          return (context, property.cast context value)
        end
    | Property_group_cached thread->
        lwt properties = thread in
        let context, value  = String_map.find property.member properties in
        return (context, property.cast context value)
    | Property_group_monitor(thread, notifier) ->
        lwt signal = thread in
        let context, value = String_map.find property.member (React.S.value signal) in
        return (context, property.cast context value)

let get ?cache property =
  get_with_context ?cache property >|= snd

let set property value =
  let property_group = property.property_group in
  OBus_method.call m_Set
    property.proxy
    (property_group.property_group_interface, property.member, property.make value)

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let make_backend proxy info ?(notify_mode=notify_none) ~cast ~make () =
  let connection = OBus_proxy.connection proxy in
  let running = running_of_connection connection in
  let key = (OBus_proxy.name proxy, OBus_proxy.path proxy, OBus_member.Property.interface info) in
  let property_group =
    match try Some(Property_map.find key running.running_properties) with Not_found -> None with
      | Some property_group ->
          property_group.property_group_ref_count <- property_group.property_group_ref_count + 1;
          property_group
      | None ->
          let property_group = {
            property_group_ref_count = 1;
            property_group_state = Property_group_simple;
            property_group_connection = connection;
            property_group_owner = OBus_proxy.name proxy;
            property_group_path = OBus_proxy.path proxy;
            property_group_interface = OBus_member.Property.interface info;
          } in
          running.running_properties <- Property_map.add key property_group running.running_properties;
          property_group
  in
  let property = {
    cast = cast;
    make = make;
    member = OBus_member.Property.member info;
    proxy = proxy;
    notify_mode = notify_mode;
    property_group = property_group;
  } in
  Gc.finalise cleanup_property property;
  property

let make info ?notify_mode proxy =
  let typ = OBus_member.Property.typ info in
  make_backend proxy info ?notify_mode
    ~cast:(fun context value -> OBus_value.C.cast_single typ value)
    ~make:(fun value -> OBus_value.C.make_single typ value)
    ()

(* +-----------------------------------------------------------------+
   | Reading all properties                                          |
   +-----------------------------------------------------------------+ *)

let get_all_with_context proxy ~interface =
  let connection = OBus_proxy.connection proxy in
  let running = running_of_connection connection in
  match
    try
      Some(Property_map.find
             (OBus_proxy.name proxy,
              OBus_proxy.path proxy,
              interface)
             running.running_properties)
    with Not_found ->
      None
  with
    | None | Some{ property_group_state = (Property_group_simple | Property_group_updates _) } ->
        load_cache proxy interface
    | Some{ property_group_state = Property_group_cached thread } ->
        thread
    | Some{ property_group_state = Property_group_monitor(thread, notifier) } ->
        lwt signal = thread in
        return (React.S.value signal)

let get_all proxy ~interface =
  get_all_with_context proxy interface >|= String_map.map snd
