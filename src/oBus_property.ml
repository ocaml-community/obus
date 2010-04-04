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
open OBus_pervasives

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type 'a access = Readable | Writable | Readable_writable

let readable = Readable
let writable = Writable
let readable_writable = Readable_writable

type notify_data = OBus_private_connection.notify_data

type notifier = OBus_private_connection.notifier = {
  notifier_signal : notify_data React.signal;
  notifier_stop : unit -> unit;
}

type notify_mode = OBus_connection.t -> OBus_name.bus option -> OBus_path.t -> OBus_name.interface -> notifier Lwt.t

type ('a, 'access) t = {
  cast : OBus_type.context -> OBus_value.single -> 'a;
  make : 'a -> OBus_value.single;
  member : OBus_name.member;
  notify : notify_mode;
  core : OBus_private_connection.property;
  (* The ``core'' property. It is used by the connection *)
}

type 'a r = ('a, [ `readable ]) t
type 'a w = ('a, [ `writable ]) t
type 'a rw = ('a, [ `readable | `writable ]) t

(* +-----------------------------------------------------------------+
   | Property reading/writing                                        |
   +-----------------------------------------------------------------+ *)

let get property =
  let core = property.core in
  match core.property_state with
    | Property_simple ->
        lwt variant, context =
          OBus_method.call
            ~connection:core.property_connection
            ?destination:core.property_owner
            ~path:core.property_path
            ~interface:"org.freedesktop.DBus.Properties"
            ~member:"Get"
            <:obus_func< string -> string -> variant * context >>
            core.property_interface
            property.member
        in
        return (property.cast context variant)
    | Property_monitor notifier ->
        lwt notifier = notifier in
        let context, value = String_map.find property.member (React.S.value notifier.notifier_signal) in
        return (property.cast context value)

let set property value =
  let core = property.core in
  OBus_method.call
    ~connection:core.property_connection
    ?destination:core.property_owner
    ~path:core.property_path
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"Set"
    <:obus_func< string -> string -> variant -> unit >>
    core.property_interface
    property.member
    (property.make value)

(* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ *)

let obus_properties = OBus_type.map_with_context <:obus_type< (string, variant) dict >>
  (fun context list ->
     List.fold_left
       (fun acc (name, variant) -> String_map.add name (context, variant) acc)
       String_map.empty
       list)
  (fun map  ->
     String_map.fold (fun name (context, variant) acc -> (name, variant) :: acc) map [])

let get_all_no_cache connection owner path interface =
  OBus_method.call
    ~connection:connection
    ?destination:owner
    ~path
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"GetAll"
    <:obus_func< string -> properties >>
    interface

let notify_none connection owner path interface =
  fail (Failure "OBus_property.monitor: this property can not be monitored")

let equal_context_value (context1, value1) (context2, value2) =
  context1 == context2 && value1 = value2

let notify_global name connection owner path interface =
  let signal =
    OBus_signal.connect
      ~connection
      ?sender:owner
      ~path
      ~interface
      ~member:name
      <:obus_type< unit >>
  in
  lwt initial = get_all_no_cache connection owner path interface in
  return {
    notifier_signal =
      React.S.hold ~eq:(String_map.equal equal_context_value)
        initial
        (Lwt_event.map_s
           (fun () -> get_all_no_cache connection owner path interface)
           (OBus_signal.event signal));
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_update name connection owner path interface =
  let signal =
    OBus_signal.connect
      ~connection
      ?sender:owner
      ~path
      ~interface
      ~member:name
      <:obus_type< properties >>
  in
  lwt initial = get_all_no_cache connection owner path interface in
  return {
    notifier_signal =
      React.S.fold ~eq:(String_map.equal equal_context_value)
        (fun properties updates -> String_map.fold String_map.add updates properties)
        initial
        (OBus_signal.event signal);
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_egg_dbus connection owner path interface =
  let signal =
    OBus_signal.connect
      ~connection
      ?sender:owner
      ~path
      ~interface:"org.freedesktop.DBus.Properties"
      ~member:"EggDBusChanged"
      <:obus_type< string * properties >>
  in
  (* Only monitor the properties with the same interface: *)
  OBus_signal.set_filters signal [(0, OBus_match.AF_string interface)];
  lwt initial = get_all_no_cache connection owner path interface in
  return {
    notifier_signal =
      React.S.fold ~eq:(String_map.equal equal_context_value)
        (fun properties (iface, updates) -> String_map.fold String_map.add updates properties)
        initial
        (OBus_signal.event signal);
    notifier_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_custom f = f

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let monitorable property = property.notify != notify_none

let rec monitor property =
  let core = property.core in
  match core.property_state with
    | Property_monitor notifier ->
        lwt notifier = notifier in
        return (React.S.map
                  (fun properties ->
                     let context, value = String_map.find property.member properties in
                     property.cast context value)
                  notifier.notifier_signal)
    | Property_simple ->
        core.property_state <- Property_monitor(property.notify core.property_connection core.property_owner core.property_path core.property_interface);
        monitor property

let unmonitor property =
  let core = property.core in
  match core.property_state with
    | Property_simple ->
        ()
    | Property_monitor notifier ->
        core.property_state <- Property_simple;
        match Lwt.state notifier with
          | Sleep ->
              cancel notifier
          | Return notifier ->
              React.S.stop notifier.notifier_signal;
              notifier.notifier_stop ()
          | Fail exn ->
              ()

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let cleanup property =
  let core = property.core in
  let running = running_of_connection core.property_connection in
  core.property_ref_count <- core.property_ref_count - 1;
  if core.property_ref_count = 0 then begin
    running.running_properties <- Property_map.remove (core.property_owner, core.property_path, core.property_interface) running.running_properties;
    unmonitor property
  end

let make_backend ~connection ?owner ~path ~interface ~member ~access ?(notify=notify_none) ~cast ~make () =
  let running = running_of_connection connection in
  let core =
    match try Some(Property_map.find (owner, path, interface) running.running_properties) with Not_found -> None with
      | Some core ->
          core.property_ref_count <- core.property_ref_count + 1;
          core
      | None ->
          let core = {
            property_ref_count = 1;
            property_state = Property_simple;
            property_connection = connection;
            property_owner = owner;
            property_path = path;
            property_interface = interface;
          } in
          running.running_properties <- Property_map.add (owner, path, interface) core running.running_properties;
          core
  in
  let property = {
    cast = cast;
    make = make;
    member = member;
    notify = notify;
    core = core;
  } in
  Gc.finalise cleanup property;
  property

let make ~connection ?owner ~path ~interface ~member ~access ?notify typ =
  make_backend ~connection ?owner ~path ~interface ~member ~access ?notify
    ~cast:(fun context value -> OBus_type.cast_single typ ~context value)
    ~make:(fun value -> OBus_type.make_single typ value)
    ()

let dyn_make ~connection ?owner ~path ~interface ~member ~access ?notify () =
  make_backend ~connection ?owner ~path ~interface ~member ~access ?notify
    ~cast:(fun context value -> value)
    ~make:(fun value -> value)
    ()

(* +-----------------------------------------------------------------+
   | Reading all properties                                          |
   +-----------------------------------------------------------------+ *)

let get_all ~connection ?owner ~path ~interface () =
  let running = running_of_connection connection in
  match try Some(Property_map.find (owner, path, interface) running.running_properties) with Not_found -> None with
    | None | Some{ property_state = Property_simple } ->
        lwt dict =
          OBus_method.call
            ~connection:connection
            ?destination:owner
            ~path
            ~interface:"org.freedesktop.DBus.Properties"
            ~member:"GetAll"
            <:obus_func< string -> (string, variant) dict >>
            interface
        in
        return (List.fold_left
                  (fun map (key, value) -> String_map.add key value map)
                  String_map.empty
                  dict)
    | Some{ property_state = Property_monitor notifier } ->
        lwt notifier = notifier in
        return (String_map.map snd (React.S.value notifier.notifier_signal))
