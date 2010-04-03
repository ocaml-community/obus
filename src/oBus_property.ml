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
open OBus_private
open OBus_pervasives

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type 'a access = Readable | Writable | Readable_writable

let readable = Readable
let writable = Writable
let readable_writable = Readable_writable

type notify_data = OBus_private.notify_data

type notifier = OBus_private.notifier = {
  notify_signal : notify_data React.signal;
  notify_stop : unit -> unit;
}

type notify_mode = OBus_connection.t -> OBus_name.bus option -> OBus_path.t -> OBus_name.interface -> notifier Lwt.t

type ('a, 'access) t = {
  cast : OBus_type.context -> OBus_value.single -> 'a;
  make : 'a -> OBus_value.single;
  member : OBus_name.member;
  notify : notify_mode;
  core : OBus_private.property;
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
  match core.prop_state with
    | Prop_simple ->
        lwt variant, context =
          OBus_method.call
            ~connection:core.prop_connection
            ?destination:core.prop_owner
            ~path:core.prop_path
            ~interface:"org.freedesktop.DBus.Properties"
            ~member:"Get"
            <:obus_func< string -> string -> variant * context >>
            core.prop_interface
            property.member
        in
        return (property.cast context variant)
    | Prop_monitor notifier ->
        lwt notifier = notifier in
        let context, value = StringMap.find property.member (React.S.value notifier.notify_signal) in
        return (property.cast context value)

let set property value =
  let core = property.core in
  OBus_method.call
    ~connection:core.prop_connection
    ?destination:core.prop_owner
    ~path:core.prop_path
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"Set"
    <:obus_func< string -> string -> variant -> unit >>
    core.prop_interface
    property.member
    (property.make value)

(* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ *)

let obus_properties = OBus_type.map_with_context <:obus_type< (string, variant) dict >>
  (fun context list ->
     List.fold_left
       (fun acc (name, variant) -> StringMap.add name (context, variant) acc)
       StringMap.empty
       list)
  (fun map  ->
     StringMap.fold (fun name (context, variant) acc -> (name, variant) :: acc) map [])

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
    notify_signal =
      React.S.hold ~eq:(StringMap.equal equal_context_value)
        initial
        (Lwt_event.map_s
           (fun () -> get_all_no_cache connection owner path interface)
           (OBus_signal.event signal));
    notify_stop =
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
    notify_signal =
      React.S.fold ~eq:(StringMap.equal equal_context_value)
        (fun properties updates -> StringMap.fold StringMap.add updates properties)
        initial
        (OBus_signal.event signal);
    notify_stop =
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
    notify_signal =
      React.S.fold ~eq:(StringMap.equal equal_context_value)
        (fun properties (iface, updates) -> StringMap.fold StringMap.add updates properties)
        initial
        (OBus_signal.event signal);
    notify_stop =
      (fun () -> OBus_signal.disconnect signal);
  }

let notify_custom f = f

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let monitorable property = property.notify != notify_none

let rec monitor property =
  let core = property.core in
  match core.prop_state with
    | Prop_monitor notifier ->
        lwt notifier = notifier in
        return (React.S.map
                  (fun properties ->
                     let context, value = StringMap.find property.member properties in
                     property.cast context value)
                  notifier.notify_signal)
    | Prop_simple ->
        core.prop_state <- Prop_monitor(property.notify core.prop_connection core.prop_owner core.prop_path core.prop_interface);
        monitor property

let unmonitor property =
  let core = property.core in
  match core.prop_state with
    | Prop_simple ->
        ()
    | Prop_monitor notifier ->
        core.prop_state <- Prop_simple;
        match Lwt.state notifier with
          | Sleep ->
              cancel notifier
          | Return notifier ->
              React.S.stop notifier.notify_signal;
              notifier.notify_stop ()
          | Fail exn ->
              ()

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let cleanup property =
  let core = property.core in
  let connection = unpack_connection core.prop_connection in
  core.prop_ref_count <- core.prop_ref_count - 1;
  if core.prop_ref_count = 0 then begin
    connection.properties <- PropertyMap.remove (core.prop_owner, core.prop_path, core.prop_interface) connection.properties;
    unmonitor property
  end

let make_backend ~connection ?owner ~path ~interface ~member ~access ?(notify=notify_none) ~cast ~make () =
  let connection = unpack_connection connection in
  let core =
    match try Some(PropertyMap.find (owner, path, interface) connection.properties) with Not_found -> None with
      | Some core ->
          core.prop_ref_count <- core.prop_ref_count + 1;
          core
      | None ->
          let core = {
            prop_ref_count = 1;
            prop_state = Prop_simple;
            prop_connection = connection.packed;
            prop_owner = owner;
            prop_path = path;
            prop_interface = interface;
          } in
          connection.properties <- PropertyMap.add (owner, path, interface) core connection.properties;
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
  let connection = unpack_connection connection in
  match try Some(PropertyMap.find (owner, path, interface) connection.properties) with Not_found -> None with
    | None | Some{ prop_state = Prop_simple } ->
        lwt dict =
          OBus_method.call
            ~connection:connection.packed
            ?destination:owner
            ~path
            ~interface:"org.freedesktop.DBus.Properties"
            ~member:"GetAll"
            <:obus_func< string -> (string, variant) dict >>
            interface
        in
        return (List.fold_left
                  (fun map (key, value) -> StringMap.add key value map)
                  StringMap.empty
                  dict)
    | Some{ prop_state = Prop_monitor notifier } ->
        lwt notifier = notifier in
        return (StringMap.map snd (React.S.value notifier.notify_signal))
