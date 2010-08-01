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
open OBus_interfaces.Org_freedesktop_DBus_Properties

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type properties = OBus_value.V.single String_map.t

type ('a, 'access) t = {
  p_cast : void message_context -> properties -> 'a;
  p_make : 'a -> OBus_value.V.single;
  p_member : OBus_name.member;
  (* If [member = ""] then this a property group, otherwisse it is a
     single property *)
  p_proxy : OBus_proxy.t;
  p_group : OBus_private_connection.property_group Lazy.t;
}

type 'a r = ('a, [ `readable ]) t
type 'a w = ('a, [ `writable ]) t
type 'a rw = ('a, [ `readable | `writable ]) t

(* +-----------------------------------------------------------------+
   | Transformations                                                 |
   +-----------------------------------------------------------------+ *)

let map_rw f g property = {
  property with
    p_cast = (fun context x -> f (property.p_cast context x));
    p_make = (fun x -> property.p_make (g x));
}

let map_rw_with_context f g property = {
  property with
    p_cast = (fun context x -> f context (property.p_cast context x));
    p_make = (fun x -> property.p_make (g x));
}

let map_r f property = {
  property with
    p_cast = (fun context x -> f (property.p_cast context x));
    p_make = (fun x -> assert false);
}

let map_r_with_context f property = {
  property with
    p_cast = (fun context x -> f context (property.p_cast context x));
    p_make = (fun x -> assert false);
}

let map_w g property = {
  property with
    p_cast = (fun context x -> assert false);
    p_make = (fun x -> property.p_make (g x));
}

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
   | Clean up                                                        |
   +-----------------------------------------------------------------+ *)

let unmonitor_property_group property_group =
  match property_group.pg_watcher with
    | None ->
        return ()
    | Some pgw ->
        property_group.pg_watcher <- None;
        cancel (React.S.value pgw.pgw_cache);
        pgw.pgw_stop ()

let cleanup_property_group property_group =
  property_group.pg_ref_count <- property_group.pg_ref_count - 1;
  if property_group.pg_ref_count = 0 then begin
    let running = property_group.pg_connection#get in
    running.rc_properties <- (
      Property_map.remove
        (property_group.pg_owner,
         property_group.pg_path,
         property_group.pg_interface)
        running.rc_properties
    );
    unmonitor_property_group property_group
  end else
    return ()

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let properties_equal (context1, values1) (context2, values2) =
  context1 == context2 && String_map.equal (=) values1 values2

let rec monitor property =
  let lazy property_group = property.p_group in
  match property_group.pg_watcher with
    | Some pgw ->
        lwt context, properties = React.S.value pgw.pgw_cache in
        return
          (Lwt_signal.map_s
             (fun thread ->
                lwt context, properties = thread in
                return (property.p_cast context properties))
             (property.p_cast context properties)
             pgw.pgw_cache)
    | None ->
        lwt resolver, owner =
          match property_group.pg_owner with
            | Some name ->
                lwt resolver = OBus_resolver.make property_group.pg_connection name in
                return (Some resolver, OBus_resolver.owner resolver)
            | None ->
                return (None, React.S.const None)
        in
        property_group.pg_watcher <- Some(
          let signal = OBus_signal.connect s_PropertiesChanged property.p_proxy in
          (* Monitor only properties of the given interface *)
          OBus_signal.set_filters signal [(0, OBus_match.AF_string property_group.pg_interface)];
          let action, send_action = React.E.create () in
          let properties_signal =
            React.S.fold ~eq:(==)
              (fun acc action ->
                 lwt old_context, properties = acc in
                 match action with
                   | Update(context, (interface, updates, invalidates)) ->
                       if invalidates <> [] then
                         get_all_no_cache property.p_proxy property_group.pg_interface
                       else
                         return (context, List.fold_left (fun map (member, value) -> String_map.add member value map) properties updates)
                   | Invalidate ->
                       get_all_no_cache property.p_proxy property_group.pg_interface)
              (get_all_no_cache property.p_proxy property_group.pg_interface)
              (React.E.select
                 [React.E.map (fun (ctx, props) -> Update(ctx, props)) (OBus_signal.event_with_context signal);
                  action;
                  React.E.fmap
                    (function
                       | Some _ -> Some Invalidate
                       | None -> None)
                    (React.S.changes owner)])
          in
          { pgw_cache = properties_signal;
            pgw_send = send_action;
            pgw_stop = (
              fun () ->
                OBus_signal.disconnect signal;
                match resolver with
                  | Some resolver ->
                      OBus_resolver.disable resolver
                  | None ->
                      return ()
            ) }
        );
        monitor property

let monitor_with_stopper property =
  lwt signal = monitor property in
  let stop = lazy(
    React.S.stop signal;
    cleanup_property_group (Lazy.force property.p_group)
  ) in
  return (signal, fun () -> Lazy.force stop)

let monitor_custom property ~event ~stop =
  let lazy property_group = property.p_group in
  match property_group.pg_watcher with
    | Some _ ->
        monitor property
    | None ->
        property_group.pg_watcher <- Some(
          let action, send_action = React.E.create () in
          let properties_signal =
            React.S.fold ~eq:(==)
              (fun acc action -> acc >> get_all_no_cache property.p_proxy property_group.pg_interface)
              (get_all_no_cache property.p_proxy property_group.pg_interface)
              (React.E.select [React.E.map (fun _ -> Invalidate) event; action])
          in
          { pgw_cache = properties_signal;
            pgw_send = send_action;
            pgw_stop = stop }
        );
        monitor property

let monitor_custom_with_stopper property ~event ~stop =
  lwt signal = monitor_custom property ~event ~stop in
  let stop = lazy(
    React.S.stop signal;
    cleanup_property_group (Lazy.force property.p_group)
  ) in
  return (signal, fun () -> Lazy.force stop)

let invalidate property =
  let lazy property_group = property.p_group in
  match property_group.pg_watcher with
    | Some pgw ->
        pgw.pgw_send Invalidate
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Property reading/writing                                        |
   +-----------------------------------------------------------------+ *)

let get_with_context property =
  let lazy property_group = property.p_group in
  match property_group.pg_watcher with
    | None ->
        if property.p_member <> "" then begin
          lwt context, value =
            OBus_method.call_with_context m_Get
              property.p_proxy
              (property_group.pg_interface, property.p_member)
          in
          return (context, property.p_cast context (String_map.add property.p_member value String_map.empty))
        end else begin
          lwt context, properties = get_all_no_cache property.p_proxy property_group.pg_interface in
          return (context, property.p_cast context properties)
        end
    | Some pgw ->
        lwt context, properties = React.S.value pgw.pgw_cache in
        return (context, property.p_cast context properties)

let get property =
  get_with_context property >|= snd

let set property value =
  let lazy property_group = property.p_group in
  OBus_method.call m_Set
    property.p_proxy
    (property_group.pg_interface, property.p_member, property.p_make value)

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let make_property_group proxy interface =
  let connection = OBus_proxy.connection proxy in
  let running = connection#get in
  let key = (OBus_proxy.name proxy, OBus_proxy.path proxy, interface) in
  let property_group =
  match try Some(Property_map.find key running.rc_properties) with Not_found -> None with
    | Some property_group ->
        property_group.pg_ref_count <- property_group.pg_ref_count + 1;
        property_group
    | None ->
        let property_group = {
          pg_ref_count = 1;
          pg_watcher = None;
          pg_connection = connection;
          pg_owner = OBus_proxy.name proxy;
          pg_path = OBus_proxy.path proxy;
          pg_interface = interface;
        } in
        running.rc_properties <- Property_map.add key property_group running.rc_properties;
        property_group
  in
  Lwt_gc.finalise cleanup_property_group property_group;
  property_group

let make info proxy =
  let typ = OBus_member.Property.typ info and member = OBus_member.Property.member info in
  {
    p_cast = (fun context properties -> OBus_value.C.cast_single typ (String_map.find member properties));
    p_make = (fun value -> OBus_value.C.make_single typ value);
    p_member = member;
    p_proxy = proxy;
    p_group = (let interface = OBus_member.Property.interface info in
               lazy(make_property_group proxy interface));
  }

let make_group proxy interface =
  {
    p_cast = (fun context properties -> properties);
    p_make = (fun value -> assert false);
    p_member = "";
    p_proxy = proxy;
    p_group = lazy(make_property_group proxy interface);
  }

(* +-----------------------------------------------------------------+
   | Reading all properties                                          |
   +-----------------------------------------------------------------+ *)

let get_all_with_context proxy ~interface =
  let connection = OBus_proxy.connection proxy in
  let running = connection#get in
  match
    try
      Some(Property_map.find
             (OBus_proxy.name proxy,
              OBus_proxy.path proxy,
              interface)
             running.rc_properties)
    with Not_found ->
      None
  with
    | None | Some{ pg_watcher = None } ->
        get_all_no_cache proxy interface
    | Some{ pg_watcher = Some{ pgw_cache = signal;
                               pgw_send = send;
                               pgw_stop = stop } } ->
        React.S.value signal

let get_all proxy ~interface =
  get_all_with_context proxy interface >|= snd

let invalidate_all proxy ~interface =
  let connection = OBus_proxy.connection proxy in
  let running = connection#get in
  match
    try
      Some(Property_map.find
             (OBus_proxy.name proxy,
              OBus_proxy.path proxy,
              interface)
             running.rc_properties)
    with Not_found ->
      None
  with
    | None | Some{ pg_watcher = None } ->
        ()
    | Some{ pg_watcher = Some{ pgw_cache = thread;
                               pgw_send = send;
                               pgw_stop = stop } } ->
        send Invalidate

(* +-----------------------------------------------------------------+
   | Reading properties from a set of properties                     |
   +-----------------------------------------------------------------+ *)

let find property context properties =
  property.p_cast context properties
