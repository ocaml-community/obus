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

type properties = OBus_value.V.single String_map.t

type notifier = OBus_private_connection.notifier = {
  notifier_signal : (unit OBus_context.t * properties) React.signal;
  notifier_stop : unit -> unit;
}

type notify_mode = OBus_proxy.t -> OBus_name.interface -> notifier Lwt.t

type ('a, 'access) t = {
  cast : unit context -> properties -> 'a;
  make : 'a -> OBus_value.V.single;
  member : OBus_name.member;
  (* If [member = ""] then this a property group, otherwisse it is a
     single property *)
  proxy : OBus_proxy.t;
  notify_mode : notify_mode;
  property_group : OBus_private_connection.property_group Lazy.t;
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
  return (context,
          List.fold_left
            (fun acc (name, variant) -> String_map.add name variant acc)
            String_map.empty
            dict)

(* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ *)

let properties_equal (context1, values1) (context2, values2) =
  context1 == context2 && String_map.equal (=) values1 values2

let notify_none proxy interface =
  lwt initial = get_all_no_cache proxy interface in
  return {
    notifier_signal = React.S.const initial;
    notifier_stop = ignore;
  }

let notify_global member proxy interface =
  let signal =
    OBus_signal.connect
      (OBus_member.Signal.make
         ~interface
         ~member
         ~args:OBus_value.arg0)
      proxy
  in
  lwt initial = get_all_no_cache proxy interface in
  return {
    notifier_signal =
      React.S.hold
        ~eq:properties_equal
        initial
        (Lwt_event.map_s
           (fun () -> get_all_no_cache proxy interface)
           (OBus_signal.event signal));
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
  lwt initial = get_all_no_cache proxy interface in
  return {
    notifier_signal =
      React.S.fold
        ~eq:properties_equal
        (fun (old_context, properties) (context, updates) ->
           (context,
            List.fold_left
              (fun map (member, value) ->
                 String_map.add member value map)
              properties
              updates))
        initial
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
  lwt initial = get_all_no_cache proxy interface in
  return {
    notifier_signal =
      React.S.fold
        ~eq:properties_equal
        (fun (old_context, properties) (context, (iface_name, updates)) ->
           (context,
            List.fold_left
              (fun map (member, value) ->
                 String_map.add member value map)
              properties
              updates))
        initial
        (OBus_signal.event_with_context signal);
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_custom f = f

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let rec monitor property =
  let lazy property_group = property.property_group in
  match property_group.property_group_state with
    | Property_group_simple
    | Property_group_cached _ ->
        let thread =
          property.notify_mode
            property.proxy
            property_group.property_group_interface
        in
        property_group.property_group_state <- Property_group_monitor thread;
        monitor property
    | Property_group_monitor thread ->
        lwt notifier = thread in
        return
          (React.S.map
             (fun (context, properties) -> property.cast context properties)
             notifier.notifier_signal)

let monitor_with_stopper property =
  lwt signal = monitor property in
  (* TODO: implement the stop function *)
  return (signal, ignore)

(* +-----------------------------------------------------------------+
   | Clean up                                                        |
   +-----------------------------------------------------------------+ *)

let unmonitor_property_group property_group =
  match property_group.property_group_state with
    | Property_group_simple ->
        ()
    | Property_group_cached thread ->
        property_group.property_group_state <- Property_group_simple;
        cancel thread
    | Property_group_monitor thread ->
        property_group.property_group_state <- Property_group_simple;
        match Lwt.state thread with
          | Sleep ->
              cancel thread
          | Return notifier ->
              React.S.stop notifier.notifier_signal;
              notifier.notifier_stop ()
          | Fail exn ->
              ()

let cleanup_property_group property_group =
  property_group.property_group_ref_count <- property_group.property_group_ref_count - 1;
  if property_group.property_group_ref_count = 0 then begin
    let running = running_of_connection property_group.property_group_connection in
    running.running_properties <- (
      Property_map.remove
        (property_group.property_group_owner,
         property_group.property_group_path,
         property_group.property_group_interface)
        running.running_properties
    );
    unmonitor_property_group property_group
  end

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
  property_group.property_group_state <- Property_group_cached thread;
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
  let lazy property_group = property.property_group in
  match property_group.property_group_state with
    | Property_group_simple ->
        if property.member <> "" && not cache then begin
          lwt context, value =
            OBus_method.call_with_context m_Get
              property.proxy
              (property_group.property_group_interface, property.member)
          in
          return (context, property.cast context (String_map.add property.member value String_map.empty))
        end else begin
          lwt context, properties = load_cache property.proxy property_group.property_group_interface in
          return (context, property.cast context properties)
        end
    | Property_group_cached thread->
        lwt context, properties = thread in
        return (context, property.cast context properties)
    | Property_group_monitor thread ->
        lwt notifier = thread in
        let context, properties = React.S.value notifier.notifier_signal in
        return (context, property.cast context properties)

let get ?cache property =
  get_with_context ?cache property >|= snd

let set property value =
  let lazy property_group = property.property_group in
  OBus_method.call m_Set
    property.proxy
    (property_group.property_group_interface, property.member, property.make value)

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let make_property_group proxy interface =
  let connection = OBus_proxy.connection proxy in
  let running = running_of_connection connection in
  let key = (OBus_proxy.name proxy, OBus_proxy.path proxy, interface) in
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
          property_group_interface = interface;
        } in
        running.running_properties <- Property_map.add key property_group running.running_properties;
        property_group
  in
  Gc.finalise cleanup_property_group property_group;
  property_group

let make info ?(notify_mode=notify_none) proxy =
  let typ = OBus_member.Property.typ info and member = OBus_member.Property.member info in
  {
    cast = (fun context properties -> OBus_value.C.cast_single typ (String_map.find member properties));
    make = (fun value -> OBus_value.C.make_single typ value);
    member = member;
    proxy = proxy;
    notify_mode = notify_mode;
    property_group = (let interface = OBus_member.Property.interface info in
                      lazy(make_property_group proxy interface));
  }

let make_group proxy ?(notify_mode=notify_none) interface =
  {
    cast = (fun context properties -> properties);
    make = (fun value -> assert false);
    member = "";
    proxy = proxy;
    notify_mode = notify_mode;
    property_group = lazy(make_property_group proxy interface);
  }

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
    | None | Some{ property_group_state = Property_group_simple } ->
        load_cache proxy interface
    | Some{ property_group_state = Property_group_cached thread } ->
        thread
    | Some{ property_group_state = Property_group_monitor thread } ->
        lwt notifier = thread in
        return (React.S.value notifier.notifier_signal)

let get_all proxy ~interface =
  get_all_with_context proxy interface >|= snd

(* +-----------------------------------------------------------------+
   | Reading properties from a set of properties                     |
   +-----------------------------------------------------------------+ *)

let find property context properties =
  property.cast context properties
