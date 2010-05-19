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

type ('a, 'access) t = {
  cast : void context -> properties -> 'a;
  make : 'a -> OBus_value.V.single;
  member : OBus_name.member;
  (* If [member = ""] then this a property group, otherwisse it is a
     single property *)
  proxy : OBus_proxy.t;
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

let s_PropertiesChanged =
  OBus_member.Signal.make
    ~interface
    ~member:"PropertiesChanged"
    ~args:(OBus_value.arg3
             (None, OBus_value.C.basic_string)
             (None, OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)
             (None, OBus_value.C.array OBus_value.C.basic_string))

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
  match property_group.property_group_monitor with
    | None ->
        ()
    | Some(signal, send, stop) ->
        property_group.property_group_monitor <- None;
        cancel (React.S.value signal);
        stop ()

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
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let properties_equal (context1, values1) (context2, values2) =
  context1 == context2 && String_map.equal (=) values1 values2

let rec monitor property =
  let lazy property_group = property.property_group in
  match property_group.property_group_monitor with
    | Some(signal, send, stop) ->
        lwt context, properties = React.S.value signal in
        return
          (Lwt_signal.map_s
             (fun thread ->
                lwt context, properties = thread in
                return (property.cast context properties))
             (property.cast context properties)
             signal)
    | None ->
        property_group.property_group_monitor <- Some(
          let signal = OBus_signal.connect s_PropertiesChanged property.proxy in
          (* Monitor only properties of the given interface *)
          OBus_signal.set_filters signal [(0, OBus_match.AF_string property_group.property_group_interface)];
          let action, send_action = React.E.create () in
          let properties_signal =
            React.S.fold ~eq:(==)
              (fun acc action ->
                 lwt old_context, properties = acc in
                 match action with
                   | Update(context, (interface, updates, invalidates)) ->
                       if invalidates <> [] then
                         get_all_no_cache property.proxy property_group.property_group_interface
                       else
                         return (context, List.fold_left (fun map (member, value) -> String_map.add member value map) properties updates)
                   | Invalidate ->
                       get_all_no_cache property.proxy property_group.property_group_interface)
              (get_all_no_cache property.proxy property_group.property_group_interface)
              (React.E.select
                 [React.E.map (fun (ctx, props) -> Update(ctx, props)) (OBus_signal.event_with_context signal);
                  action])
          in
          (properties_signal, send_action, fun () -> OBus_signal.disconnect signal)
        );
        monitor property

let monitor_with_stopper property =
  lwt signal = monitor property in
  let stop = lazy(
    React.S.stop signal;
    cleanup_property_group (Lazy.force property.property_group)
  ) in
  return (signal, fun () -> Lazy.force stop))

let invalidate property =
  let lazy property_group = property.property_group in
  match property_group.property_group_monitor with
    | Some(signal, send, stop) ->
        send Invalidate
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Property reading/writing                                        |
   +-----------------------------------------------------------------+ *)

let get_with_context property =
  let lazy property_group = property.property_group in
  match property_group.property_group_monitor with
    | None ->
        if property.member <> "" then begin
          lwt context, value =
            OBus_method.call_with_context m_Get
              property.proxy
              (property_group.property_group_interface, property.member)
          in
          return (context, property.cast context (String_map.add property.member value String_map.empty))
        end else begin
          lwt context, properties = get_all_no_cache property.proxy property_group.property_group_interface in
          return (context, property.cast context properties)
        end
    | Some(signal, send, stop) ->
        lwt context, properties = React.S.value signal in
        return (context, property.cast context properties)

let get property =
  get_with_context property >|= snd

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
          property_group_monitor = None;
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

let make info proxy =
  let typ = OBus_member.Property.typ info and member = OBus_member.Property.member info in
  {
    cast = (fun context properties -> OBus_value.C.cast_single typ (String_map.find member properties));
    make = (fun value -> OBus_value.C.make_single typ value);
    member = member;
    proxy = proxy;
    property_group = (let interface = OBus_member.Property.interface info in
                      lazy(make_property_group proxy interface));
  }

let make_group proxy interface =
  {
    cast = (fun context properties -> properties);
    make = (fun value -> assert false);
    member = "";
    proxy = proxy;
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
    | None | Some{ property_group_monitor = None } ->
        get_all_no_cache proxy interface
    | Some{ property_group_monitor = Some(signal, send, stop) } ->
        React.S.value signal

let get_all proxy ~interface =
  get_all_with_context proxy interface >|= snd

let invalidate_all proxy ~interface =
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
    | None | Some{ property_group_monitor = None } ->
        ()
    | Some{ property_group_monitor = Some(thread, send, stop) } ->
        send Invalidate

(* +-----------------------------------------------------------------+
   | Reading properties from a set of properties                     |
   +-----------------------------------------------------------------+ *)

let find property context properties =
  property.cast context properties
