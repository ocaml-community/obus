(*
 * oBus_object.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_private_connection
open OBus_message

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Connection_set = Set.Make(OBus_connection)
module Member_map = String_map

type emits_signal_changed =
  | Esc_not_specified
  | Esc_false
  | Esc_true
  | Esc_invalidates

type 'a method_info = {
  method_name : OBus_name.member;
  method_type : OBus_value.signature;
  method_handler : OBus_value.V.sequence OBus_context.t -> 'a t -> OBus_message.t -> [ `Replied | `No_reply ] Lwt.t;
  method_introspect : unit -> OBus_introspect.member;
}

and 'a signal_info = {
  signal_introspect : unit -> OBus_introspect.member;
}

and 'a property_info = {
  property_name : OBus_name.member;
  property_type : OBus_value.T.single;
  property_signal : ('a -> OBus_value.V.single React.signal) option;
  property_setter : (unit OBus_context.t -> 'a -> OBus_value.V.single -> unit Lwt.t) option;
  property_emits_changed_signal : emits_signal_changed;
  property_introspect : unit -> OBus_introspect.member;
}

and 'a property_instance = {
  property_instance_signal : OBus_value.V.single React.signal;
  property_instance_monitor : unit React.event;
  (* Event which send notifications when the contents of the property
     changes *)
}

(* An interface descriptor *)
and 'a interface = {
  interface_name : OBus_name.interface;
  (* The name of the interface *)

  interface_methods : 'a method_info array;
  (* Array of methods, for dispatching method calls and introspection *)

  interface_signals : 'a signal_info array;
  (* Array of signals, for introspection *)

  interface_properties : 'a property_info array;
  (* Array of for properties, for reading/writing properties and introspection *)

  interface_emits_changed_signal : emits_signal_changed;
  (* Default mode for sending notification of properties changes *)
}

(* D-Bus object informations *)
and 'a t = {
  path : OBus_path.t;
  (* The path of the object *)

  mutable data : 'a option;
  (* Data attached to the object *)

  mutable exports : Connection_set.t;
  (* Set of connection on which the object is exported *)

  owner : OBus_peer.t option;
  (* The optionnal owner of the object *)

  mutable interfaces : 'a interface array;
  (* Interfaces implemented by this object *)

  mutable properties : 'a property_instance option array array;
  (* All property instances of the object *)

  mutable changed : OBus_value.V.single option Member_map.t array;
  (* Properties that changed since the last upadte, organised by
     interface *)

  properties_changed : (OBus_name.interface -> (OBus_name.member * OBus_value.V.single option) list -> unit Lwt.t) ref;
  (* Function called when proeprties change. It may emit a
     notification signal. The default one use
     [org.freedesktop.DBus.Properties.PropertiesChanged] *)
}

(* +-----------------------------------------------------------------+
   | Object parameters                                               |
   +-----------------------------------------------------------------+ *)

let path obj = obj.path
let owner obj = obj.owner
let exports obj = obj.exports

let introspect obj =
  Array.fold_right
    (fun interface acc ->
       (interface.interface_name,
        Array.fold_right
          (fun method_ acc -> method_.method_introspect () :: acc)
          interface.interface_methods
          (Array.fold_right
             (fun signal acc -> signal.signal_introspect () :: acc)
             interface.interface_signals
             (Array.fold_right
                (fun property acc -> property.property_introspect () :: acc)
                interface.interface_properties
                [])),
        []) :: acc)
    obj.interfaces []

let on_properties_changed obj = obj.properties_changed

(* +-----------------------------------------------------------------+
   | Binary search                                                   |
   +-----------------------------------------------------------------+ *)

let binary_search compare key array =
  let rec loop a b =
    if a = b then
      -1
    else begin
      let middle = (a + b) / 2 in
      let cmp = compare key (Array.unsafe_get array middle) in
      if cmp = 0 then
        middle
      else if cmp < 0 then
        loop a middle
      else
        loop (middle + 1) b
    end
  in
  loop 0 (Array.length array)

let compare_interface name interface =
  String.compare name interface.interface_name

let compare_property name property =
  String.compare name property.property_name

let compare_method (name, typ) method_ =
  match String.compare name method_.method_name with
    | 0 ->
        compare typ method_.method_type
    | n ->
        n

(* +-----------------------------------------------------------------+
   | Exportation                                                     |
   +-----------------------------------------------------------------+ *)

let unknown_method context message =
  OBus_method.fail
    context
    OBus_error.Unknown_method
    (unknown_method_message message)

let handle_call obj context message =
  match message with
    | { typ = Method_call(path, Some interface, member) } -> begin
        match binary_search compare_interface interface obj.interfaces with
          | -1 ->
              unknown_method context message
          | index ->
              let interface = obj.interfaces.(index) in
              match
                binary_search
                  compare_method
                  (member, OBus_value.V.type_of_sequence message.body)
                  interface.interface_methods
              with
                | -1 ->
                    unknown_method context message
                | index ->
                    interface.interface_methods.(index).method_handler context obj message
      end
    | { typ = Method_call(path, None, member) } ->
        let key = (member, OBus_value.V.type_of_sequence message.body) in
        let rec loop i =
          if i = Array.length obj.interfaces then
            unknown_method context message
          else
            match binary_search compare_method key obj.interfaces.(i).interface_methods with
              | -1 ->
                  loop (i + 1)
              | index ->
                  obj.interfaces.(i).interface_methods.(index).method_handler context obj message
        in
        loop 0
    | _ ->
        invalid_arg "OBus_object.Make.handle_call"

let export connection obj =
  if obj.data = None then
    failwith "OBus_object.export: cannot export an object without data attahed"
  else begin
    let running = running_of_connection connection in
    if not (Connection_set.mem connection obj.exports) then begin
      running.running_static_objects <- Object_map.add obj.path {
        static_object_handle = handle_call obj;
        static_object_connection_closed = (fun connection -> obj.exports <- Connection_set.remove connection obj.exports);
      } running.running_static_objects;
      obj.exports <- Connection_set.add connection obj.exports
    end
  end

let remove_by_path connection path =
  match connection#get with
    | Crashed _ ->
        ()
    | Running running ->
        running.running_dynamic_objects <- Object_map.remove path running.running_dynamic_objects;
        match try Some(Object_map.find path running.running_static_objects) with Not_found -> None with
          | Some static_object ->
              running.running_static_objects <- Object_map.remove path running.running_static_objects;
              static_object.static_object_connection_closed connection
          | None ->
              ()

let remove connection obj =
  if Connection_set.mem connection obj.exports then begin
    obj.exports <- Connection_set.remove connection obj.exports;
    match connection#get with
      | Crashed _ ->
          ()
      | Running running ->
          running.running_static_objects <- Object_map.remove obj.path running.running_static_objects
  end

let destroy obj =
  Connection_set.iter
    (fun connection -> match connection#get with
       | Crashed exn ->
           ()
       | Running running ->
           running.running_static_objects <- Object_map.remove obj.path running.running_static_objects)
    obj.exports;
  obj.exports <- Connection_set.empty

let dynamic ~connection ~prefix ~handler =
  let running = running_of_connection connection in
  (* Remove any dynamic node declared with the same prefix: *)
  let create context path =
    handler context path >>= function
      | `Object obj ->
          return (`Object{
                    static_object_handle = handle_call obj;
                    static_object_connection_closed = ignore;
                  })
      | `Replied ->
          return `Replied
      | `No_reply ->
          return `No_reply
      | `Not_found ->
          return `Not_found
  in
  running.running_dynamic_objects <- Object_map.add prefix create running.running_dynamic_objects

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

let emit obj ~interface ~member ?peer typ x =
  let body = OBus_value.C.make_sequence typ x in
  match peer, obj.owner with
    | Some { OBus_peer.connection = connection; OBus_peer.name = destination }, _
    | _, Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
        OBus_connection.send_message connection {
          flags = { no_reply_expected = true; no_auto_start = true };
          serial = 0l;
          typ = OBus_message.Signal(obj.path, interface, member);
          destination = destination;
          sender = None;
          body = body;
        }
    | None, None ->
        let signal = {
          flags = { no_reply_expected = true; no_auto_start = true };
          serial = 0l;
          typ = OBus_message.Signal(obj.path, interface, member);
          destination = None;
          sender = None;
          body = body;
        } in
        join (Connection_set.fold
                (fun connection l -> OBus_connection.send_message connection signal :: l)
                obj.exports [])

(* +-----------------------------------------------------------------+
   | Property change notifications                                   |
   +-----------------------------------------------------------------+ *)

let s_PropertiesChanged =
  OBus_member.Signal.make
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"PropertiesChanged"
    ~args:(OBus_value.arg3
             (None, OBus_value.C.basic_string)
             (None, OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)
             (None, OBus_value.C.array OBus_value.C.basic_string))
    ~annotations:[]

let notify_properties_change obj interface_name changed index =
  (* Sleep a bit, so multiple changes are sent only one time. *)
  lwt () = pause () in
  let members = changed.(index) in
  changed.(index) <- Member_map.empty;
  try_lwt
    !(obj.properties_changed)
      interface_name
      (Member_map.fold (fun name value_opt acc -> (name, value_opt) :: acc) members [])
  with exn ->
    Lwt_log.error ~exn ~section "properties_changed callback failed with"

let handle_property_change obj index member value_opt =
  let empty = Member_map.is_empty obj.changed.(index) in
  obj.changed.(index) <- Member_map.add member value_opt obj.changed.(index);
  if empty then ignore (notify_properties_change obj obj.interfaces.(index).interface_name obj.changed index)

let handle_property_change_true obj interface_index member value =
  handle_property_change obj interface_index member (Some value)

let handle_property_change_invalidates obj interface_index member value =
  handle_property_change obj interface_index member None

(* +-----------------------------------------------------------------+
   | Property maps geenration                                        |
   +-----------------------------------------------------------------+ *)

(* Generate the [properties] and the [methods] fields from the
   [interfaces] field: *)
let generate obj =
  (* Stop monitoring of previous properties *)
  Array.iter
    (fun instances ->
       Array.iter
         (function
            | Some instance -> React.E.stop instance.property_instance_monitor
            | None -> ())
         instances)
    obj.properties;
  let count = Array.length obj.interfaces in
  obj.properties <- Array.make count [||];
  obj.changed <- Array.make count Member_map.empty;
  for i = 0 to count - 1 do
    let properties = obj.interfaces.(i).interface_properties in
    let count' = Array.length properties in
    let instances = Array.make count' None in
    obj.properties.(i) <- instances;
    for j = 0 to count' - 1 do
      match properties.(j).property_signal with
        | Some make ->
            let signal = make (match obj.data with Some data -> data | None -> assert false) in
            instances.(j) <- (
              Some({
                     property_instance_signal = signal;
                     property_instance_monitor =
                       (match properties.(j).property_emits_changed_signal, obj.interfaces.(i).interface_emits_changed_signal with
                          | Esc_false, _ | Esc_not_specified, (Esc_not_specified | Esc_false) ->
                              React.E.never
                          | Esc_true, _ | Esc_not_specified, Esc_true ->
                              React.E.map (handle_property_change_true obj i properties.(j).property_name) (React.S.changes signal)
                          | Esc_invalidates, _ | Esc_not_specified, Esc_invalidates ->
                              React.E.map (handle_property_change_invalidates obj i properties.(j).property_name) (React.S.changes signal))
                   })
            )
        | None ->
            ()
    done
  done

(* +-----------------------------------------------------------------+
   | Member informations                                             |
   +-----------------------------------------------------------------+ *)

let _method_info info f =
  let handler context obj message =
    let context =
      OBus_context.map
        (fun x ->
           OBus_value.C.make_sequence
             (OBus_value.arg_types
                (OBus_member.Method.o_args info))
             x)
        context
    in
    let args =
      (* This will not fail since we already tested types in the
         binary search for the method *)
      OBus_value.C.cast_sequence
        (OBus_value.arg_types (OBus_member.Method.i_args info))
        (OBus_message.body message)
    in
    f context obj args
  in
  {
    method_name = OBus_member.Method.member info;
    method_type = OBus_value.C.type_sequence (OBus_value.arg_types (OBus_member.Method.i_args info));
    method_handler = handler;
    method_introspect = (fun () -> OBus_member.Method.introspect info);
  }

let method_info info f =
  _method_info info
    (fun context obj ->
       match obj.data with
         | Some data -> f context data
         | None -> assert false)

let signal_info info = {
  signal_introspect = (fun () -> OBus_member.Signal.introspect info);
}

let get_emits_changed_signal annotations =
  try
    match List.assoc OBus_introspect.emits_changed_signal annotations with
      | "true" -> Esc_true
      | "false" -> Esc_false
      | "invalidates" -> Esc_invalidates
      | value ->
          ignore (Lwt_log.warning_f "invalid value(%S) for annotation %S, using default(\"true\")" value OBus_introspect.emits_changed_signal);
          Esc_true
  with Not_found ->
    Esc_true

let property_info info signal setter =
  let typ = OBus_member.Property.typ info in
  let signal = match signal with
    | None ->
        None
    | Some f ->
        Some(fun data -> React.S.map (OBus_value.C.make_single typ) (f data))
  in
  let setter = match setter with
    | None ->
        None
    | Some f ->
        Some(fun context data value -> f context data (OBus_value.C.cast_single typ value))
  in
  {
    property_name = OBus_member.Property.member info;
    property_type = OBus_value.C.type_single (OBus_member.Property.typ info);
    property_signal = signal;
    property_setter = setter;
    property_emits_changed_signal = get_emits_changed_signal (OBus_member.Property.annotations info);
    property_introspect = (fun () -> OBus_member.Property.introspect info);
  }

let property_r_info info signal = property_info info (Some signal) None
let property_w_info info setter = property_info info None (Some setter)
let property_rw_info info signal setter = property_info info (Some signal) (Some setter)

(* +-----------------------------------------------------------------+
   | Interfaces creation                                             |
   +-----------------------------------------------------------------+ *)

let make_interface_unsafe name annotations methods signals properties = {
  interface_name = name;
  interface_emits_changed_signal = get_emits_changed_signal annotations;
  interface_methods = methods;
  interface_signals = signals;
  interface_properties = properties;
}

let compare_methods m1 m2 =
  match String.compare m1.method_name m2.method_name with
    | 0 -> Pervasives.compare m1.method_type m2.method_type
    | n -> n

let compare_properties p1 p2 =
  match String.compare p1.property_name p2.property_name with
    | 0 -> Pervasives.compare p1.property_type p2.property_type
    | n -> n

let make_interface ~name ?(annotations=[]) ?(methods=[]) ?(signals=[]) ?(properties=[]) () =
  let methods = Array.of_list methods
  and signals = Array.of_list signals
  and properties = Array.of_list properties in
  Array.sort compare_methods  methods;
  Array.sort compare_properties properties;
  make_interface_unsafe name annotations methods signals properties

let compare_interfaces i1 i2 =
  String.compare i1.interface_name i2.interface_name

let rec uniq = function
  | iface :: iface' :: rest when iface.interface_name = iface'.interface_name ->
      uniq (iface :: rest)
  | iface :: rest ->
      iface :: uniq rest
  | [] ->
      []

let add_interfaces obj interfaces =
  obj.interfaces <- Array.of_list (uniq (List.stable_sort compare_interfaces (interfaces @ Array.to_list obj.interfaces)));
  generate obj

let remove_interfaces_by_names obj names =
  obj.interfaces <- Array.of_list (List.filter (fun iface -> not (List.mem iface.interface_name names)) (Array.to_list obj.interfaces));
  generate obj

let remove_interfaces obj interfaces =
  remove_interfaces_by_names obj (List.map (fun iface -> iface.interface_name) interfaces)

(* +-----------------------------------------------------------------+
   | Common interfaces                                               |
   +-----------------------------------------------------------------+ *)

let introspectable () =
  let interface_name = "org.freedesktop.DBus.Introspectable" in
  make_interface_unsafe interface_name [] [|
    _method_info
      (OBus_member.Method.make
         interface_name
         "Introspect"
         OBus_value.arg0
         (OBus_value.arg1
            (Some "result", OBus_value.C.basic_string))
         [])
      (fun context obj () ->
         let document =
           (introspect obj,
            match context.context_connection#get with
              | Crashed _ ->
                  []
              | Running running ->
                  children running obj.path)
         in
         let buf = Buffer.create 42 in
         OBus_introspect.output (Xmlm.make_output ~nl:true ~indent:(Some 2) (`Buffer buf)) document;
         OBus_method.return context (Buffer.contents buf))
  |] [||] [||]

let properties () =
  let interface_name = "org.freedesktop.DBus.Properties" in
  make_interface_unsafe interface_name [] [|
    _method_info
      (OBus_member.Method.make
         interface_name
         "Get"
         (OBus_value.arg2
            (Some "interface", OBus_value.C.basic_string)
            (Some "member", OBus_value.C.basic_string))
         (OBus_value.arg1
            (Some "value", OBus_value.C.variant))
         [])
      (fun context obj (interface, member) ->
         match binary_search compare_interface interface obj.interfaces with
           | -1 ->
               Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "no such interface: %S" interface
           | i ->
               match binary_search compare_property member obj.interfaces.(i).interface_properties with
                 | -1 ->
                     Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "no such property: %S on interface %S" member interface
                 | j ->
                     match obj.properties.(i).(j) with
                       | Some{ property_instance_signal = signal } ->
                           OBus_method.return context (React.S.value signal)
                       | None ->
                           Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "property %S on interface %S is not readable" member interface);
    _method_info
      (OBus_member.Method.make
         interface_name
         "GetAll"
         (OBus_value.arg1
            (Some "interface", OBus_value.C.basic_string))
         (OBus_value.arg1
            (Some "values", OBus_value.C.dict OBus_value.C.string OBus_value.C.variant))
         [])
      (fun context obj interface ->
         match binary_search compare_interface interface obj.interfaces with
           | -1 ->
               Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "no such interface: %S" interface
           | i ->
               let count = Array.length obj.properties.(i) in
               let rec loop j acc =
                 if j = count then
                   acc
                 else
                   match obj.properties.(i).(j) with
                     | Some{ property_instance_signal = signal } ->
                         loop (j + 1) ((obj.interfaces.(i).interface_properties.(j).property_name,
                                        React.S.value signal) :: acc)
                     | None ->
                         loop (j + 1) acc
               in
               OBus_method.return context (loop 0 []));
    _method_info
      (OBus_member.Method.make
         interface_name
         "Set"
         (OBus_value.arg3
            (Some "interface", OBus_value.C.basic_string)
            (Some "member", OBus_value.C.basic_string)
            (Some "value", OBus_value.C.variant))
         OBus_value.arg0
         [])
      (fun context obj (interface, member, value) ->
         match binary_search compare_interface interface obj.interfaces with
           | -1 ->
               Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "no such interface: %S" interface
           | i ->
               match binary_search compare_property member obj.interfaces.(i).interface_properties with
                 | -1 ->
                     Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "no such property: %S on interface %S" member interface
                 | j ->
                     match obj.interfaces.(i).interface_properties.(j).property_setter with
                       | Some f -> begin
                           match obj.data with
                             | Some data ->
                                 lwt () = f context data value in
                                 OBus_method.return context ()
                             | None ->
                                 assert false
                         end
                       | None ->
                           Printf.ksprintf (OBus_method.fail context OBus_error.Failed) "property %S on interface %S is not writable" member interface);
  |]
    [|signal_info s_PropertiesChanged|]
    [||]

(* +-----------------------------------------------------------------+
   | Constructors                                                    |
   +-----------------------------------------------------------------+ *)

let properties_changed obj interface values =
  emit obj
    ~interface:s_PropertiesChanged.OBus_member.Signal.interface
    ~member:s_PropertiesChanged.OBus_member.Signal.member
    (OBus_value.arg_types s_PropertiesChanged.OBus_member.Signal.args)
    (interface,
     OBus_util.filter_map
       (function
          | (name, Some value) -> Some(name, value)
          | (name, None) -> None)
       values,
     OBus_util.filter_map
       (function
          | (name, Some value) -> None
          | (name, None) -> Some name)
       values)

let make ?owner ?(common=true) ?(interfaces=[]) path =
  let interfaces = if common then introspectable () :: properties () :: interfaces else interfaces in
  let obj = {
    path = path;
    exports = Connection_set.empty;
    owner = owner;
    data = None;
    properties = [||];
    interfaces = [||];
    changed = [||];
    properties_changed = ref (fun name values -> assert false);
  } in
  obj.properties_changed := (fun name values -> properties_changed obj name values);
  add_interfaces obj interfaces;
  obj

let attach obj data =
  match obj.data with
    | Some _ ->
        failwith "OBus_object.attach: object already contains attached"
    | None ->
        obj.data <- Some data;
        generate obj;
        match obj.owner with
          | None ->
              ()
          | Some peer ->
              export (OBus_peer.connection peer) obj;
              ignore (lwt () = OBus_peer.wait_for_exit peer in
                      destroy obj;
                      return ())

let get obj =
  match obj.data with
    | Some data -> data
    | None -> failwith "OBus_object.get: no data attached"
