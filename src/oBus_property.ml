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
   | Reading all properties with their contexts                      |
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

(* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ *)

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
    | Property_monitor thread ->
        lwt notifier = thread in
        return (React.S.map
                  (fun properties ->
                     let context, value = String_map.find property.member properties in
                     property.cast context value)
                  notifier.notifier_signal)
    | Property_cached _  | Property_simple ->
        core.property_state <- Property_monitor(property.notify core.property_connection core.property_owner core.property_path core.property_interface);
        monitor property

let unmonitor_core core =
  match core.property_state with
    | Property_simple | Property_cached _ ->
        ()
    | Property_monitor thread ->
        core.property_state <- Property_simple;
        match Lwt.state thread with
          | Sleep ->
              cancel thread
          | Return notifier ->
              React.S.stop notifier.notifier_signal;
              notifier.notifier_stop ()
          | Fail exn ->
              ()

let unmonitor property =
  unmonitor_core property.core

(* +-----------------------------------------------------------------+
   | Clean up                                                        |
   +-----------------------------------------------------------------+ *)

let cleanup_core core =
  core.property_ref_count <- core.property_ref_count - 1;
  if core.property_ref_count = 0 then begin
    let running = running_of_connection core.property_connection in
    running.running_properties <- Property_map.remove (core.property_owner, core.property_path, core.property_interface) running.running_properties;
    unmonitor_core core
  end

let cleanup_property property =
  cleanup_core property.core

(* +-----------------------------------------------------------------+
   | Caching                                                         |
   +-----------------------------------------------------------------+ *)

let load_cache connection owner path interface =
  let running = running_of_connection connection in
  let core =
    match try Some(Property_map.find (owner, path, interface) running.running_properties) with Not_found -> None with
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
      | Some core ->
          core.property_ref_count <- core.property_ref_count + 1;
          core
  in
  let thread = get_all_no_cache connection owner path interface in
  core.property_state <- Property_cached thread;
  ignore begin
    lwt _ = thread in
    (* Expire the cache before the next iteration of the main loop: *)
    lwt () = pause () in
    cleanup_core core;
    match core.property_state with
      | Property_cached _ ->
          core.property_state <- Property_simple;
          return ()
      | _ ->
          return ()
  end;
  thread

(* +-----------------------------------------------------------------+
   | Property reading/writing                                        |
   +-----------------------------------------------------------------+ *)

let get ?(cache=false) property =
  let core = property.core in
  match core.property_state with
    | Property_simple ->
        if cache then begin
          lwt properties =
            load_cache
              core.property_connection
              core.property_owner
              core.property_path
              core.property_interface
          in
          let context, value  = String_map.find property.member properties in
          return (property.cast context value)
        end else begin
          lwt context, value =
            OBus_method.call
              ~connection:core.property_connection
              ?destination:core.property_owner
              ~path:core.property_path
              ~interface:"org.freedesktop.DBus.Properties"
              ~member:"Get"
            <:obus_func< string -> string -> context * variant >>
            core.property_interface
            property.member
          in
          return (property.cast context value)
        end
    | Property_cached thread->
        lwt properties = thread in
        let context, value  = String_map.find property.member properties in
        return (property.cast context value)
    | Property_monitor thread ->
        lwt notifier = thread in
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
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

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
  Gc.finalise cleanup_property property;
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
        lwt properties = load_cache connection owner path interface in
        return (String_map.map snd properties)
    | Some{ property_state = Property_cached thread } ->
        lwt properties = thread in
        return (String_map.map snd properties)
    | Some{ property_state = Property_monitor thread } ->
        lwt notifier = thread in
        return (String_map.map snd (React.S.value notifier.notifier_signal))
