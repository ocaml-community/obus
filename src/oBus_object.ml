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
module Interface_map = String_map
module Member_map = String_map

type 'a member_type =
  | Method of OBus_value.signature * (OBus_connection.t -> OBus_message.t -> 'a t -> unit Lwt.t)
  | Signal
  | Property of ('a -> OBus_value.V.single React.signal) option *
      (unit OBus_context.t -> 'a -> OBus_value.V.single -> unit Lwt.t) option

and 'a member = {
  member_name : OBus_name.member;
  member_type : 'a member_type;
  member_introspection : OBus_introspect.member;
}

and 'a method_info = {
  method_name : OBus_name.member;
  method_type : OBus_value.signature;
  method_handler : OBus_connection.t -> OBus_message.t -> 'a t -> unit Lwt.t;
}

and 'a property_info = {
  property_name : OBus_name.member;
  property_signal : ('a -> OBus_value.V.single React.signal) option;
  property_setter : (unit OBus_context.t -> 'a -> OBus_value.V.single -> unit Lwt.t) option;
}

and 'a property_instance = {
  property_instance_name : OBus_name.member;
  property_instance_signal : OBus_value.V.single React.signal option;
  property_instance_setter : (unit OBus_context.t -> 'a -> OBus_value.V.single -> unit Lwt.t) option;
  property_instance_monitor : unit React.event;
  (* Event which send notifications when the contents of the property
     changes *)
}

(* An interface descriptor *)
and 'a interface = {
  interface_name : OBus_name.interface;
  (* The name of the interface *)

  interface_introspect : OBus_introspect.interface;
  (* For introspection *)

  interface_methods : 'a method_info array;
  (* For dispatching method calls *)

  interface_properties : 'a property_info array;
  (* List for properties, for reading/writing properties *)

  interface_notify_mode : 'a notify_mode;
  (* Notification mode for this interface *)
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

  mutable interfaces : 'a interface Interface_map.t;
  (* Interfaces implemented by this object *)

  mutable methods : (OBus_name.interface * 'a method_info array) array;
  (* All methods of the object *)

  mutable properties : (OBus_name.interface * 'a property_instance array) array;
  (* All properties of the object *)

  mutable changed : OBus_value.V.single Member_map.t Interface_map.t;
  (* Properties that changed since the last upadte *)
}

and 'a notify_mode = {
  notify_emit : 'a t -> OBus_name.interface -> OBus_value.V.single Member_map.t -> unit Lwt.t;
  (* The function which send the notification *)

  notify_signature : OBus_introspect.member list;
  (* Additional members for the interface *)
}

(* +-----------------------------------------------------------------+
   | Object parameters                                               |
   +-----------------------------------------------------------------+ *)

let path obj = obj.path
let owner obj = obj.owner
let exports obj = obj.exports
let introspect obj =
  Interface_map.fold
    (fun name interface acc -> interface.interface_introspect :: acc)
    obj.interfaces []

(* +-----------------------------------------------------------------+
   | Binary search                                                   |
   +-----------------------------------------------------------------+ *)

let binary_search compare key array =
  let rec loop a b =
    if a = b then
      None
    else begin
      let middle = (a + b) / 2 in
      let element = Array.unsafe_get array middle in
      let cmp = compare key element in
      if cmp = 0 then
        Some element
      else if cmp < 0 then
        loop a middle
      else
        loop (middle + 1) b
    end
  in
  loop 0 (Array.length array)

let compare_interface interface_name (interface_name', _) =
  String.compare interface_name interface_name'

let compare_property name prop =
  String.compare name prop.property_instance_name

let compare_method (name, typ) meth =
  match String.compare name meth.method_name with
    | 0 ->
        compare typ meth.method_type
    | n ->
        n

(* +-----------------------------------------------------------------+
   | Exportation                                                     |
   +-----------------------------------------------------------------+ *)

let unknown_method connection message =
  OBus_method.fail
    (make_context connection message)
    OBus_error.Unknown_method
    (unknown_method_message message)

let handle_call obj connection message =
  match message with
    | { typ = Method_call(path, Some interface, member) } -> begin
        match binary_search compare_interface interface obj.methods with
          | None ->
              unknown_method connection message
          | Some(interface, methods) ->
              match
                binary_search
                  compare_method
                  (member, OBus_value.V.type_of_sequence message.body)
                  methods
              with
                | None ->
                    unknown_method connection message
                | Some meth ->
                    meth.method_handler connection message obj
      end
    | { typ = Method_call(path, None, member) } -> begin
        match
          Interface_map.fold
            (fun name interface found ->
               match found with
                 | Some _ ->
                     found
                 | None ->
                     binary_search
                       compare_method
                       (member, OBus_value.V.type_of_sequence message.body)
                       interface.interface_methods)
            obj.interfaces None
        with
          | Some meth ->
              meth.method_handler connection message obj
          | None ->
              unknown_method connection message
      end
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
  let create path =
    lwt obj = handler path in
    return {
      static_object_handle = handle_call obj;
      static_object_connection_closed = ignore;
    }
  in
  running.running_dynamic_objects <- Object_map.add prefix create running.running_dynamic_objects

(* +-----------------------------------------------------------------+
   | Property change notifications                                   |
   +-----------------------------------------------------------------+ *)

(* The function which send the notifications *)
let handle_property_change obj interface_name member_name value =
  let empty = Interface_map.is_empty obj.changed in
  obj.changed <- (
    Interface_map.add interface_name
      (Member_map.add member_name value
         (try Interface_map.find interface_name obj.changed with Not_found -> Member_map.empty))
      obj.changed
  );
  if empty then ignore begin
    (* Sleep a bit, so multiple changes are sent only one time. *)
    lwt () = pause () in
    let changed = obj.changed in
    obj.changed <- Interface_map.empty;
    Interface_map.iter
      (fun name properties ->
         match try Some(Interface_map.find name obj.interfaces) with Not_found -> None with
           | Some interface ->
               ignore (interface.interface_notify_mode.notify_emit obj name properties)
           | None ->
               ())
      changed;
    return ()
  end

(* +-----------------------------------------------------------------+
   | Property maps geenration                                        |
   +-----------------------------------------------------------------+ *)

let dummy = ("", [||])

(* Generate the [properties] and the [methods] fields from the
   [interfaces] field: *)
let generate obj =
  (* Stop monitoring of previous properties *)
  Array.iter
    (fun (interface, properties) ->
       Array.iter
         (fun property -> React.E.stop property.property_instance_monitor)
         properties)
    obj.properties;
  let count = Interface_map.fold (fun _ _ count -> succ count) obj.interfaces 0 in
  let properties = Array.make count dummy and methods = Array.make count dummy in
  ignore (
    Interface_map.fold
      (fun name interface i ->
         Array.unsafe_set methods i (name, interface.interface_methods);
         Array.unsafe_set properties i
           (name,
            Array.map
              (fun property ->
                 let signal, monitor =
                   match property.property_signal with
                     | Some f ->
                         let signal =
                           match obj.data with
                             | Some data -> f data
                             | None -> assert false
                         in
                         (Some signal,
                          React.E.map
                            (handle_property_change obj interface.interface_name property.property_name)
                            (React.S.changes signal))
                     | None ->
                         (None, React.E.never)
                 in
                 {
                   property_instance_name = property.property_name;
                   property_instance_signal = signal;
                   property_instance_setter = property.property_setter;
                   property_instance_monitor = monitor;
                 })
              interface.interface_properties);
         i + 1)
      obj.interfaces 0
  );
  obj.methods <- methods;
  obj.properties <- properties

(* +-----------------------------------------------------------------+
   | Member informations                                             |
   +-----------------------------------------------------------------+ *)

exception Done

let make_args arguments =
  List.map2
    (fun name typ ->
       (name, typ))
    (OBus_value.arg_names arguments)
    (OBus_value.C.type_sequence (OBus_value.arg_types arguments))

let _method_info info f =
  let handler connection message obj =
    let context =
      OBus_context.make_with_arguments
        ~connection
        ~message
        ~arguments:(OBus_member.Method.o_args info)
    in
    try_lwt
      let args =
        (* This will not fail since we already tested types in the
           binary search for the method *)
        OBus_value.C.cast_sequence
          (OBus_value.arg_types (OBus_member.Method.i_args info))
          (OBus_message.body message)
      in
      lwt result = f context obj args in
      OBus_method.return context result
    with
      | Done ->
          return ()
      | OBus_error.DBus(key, name, error_message) ->
          OBus_method.fail_by_name context name error_message
      | exn ->
          OBus_method.fail context OBus_error.OCaml (Printexc.to_string exn)
  in
  {
    member_name = OBus_member.Method.member info;
    member_type = Method(OBus_value.C.type_sequence
                           (OBus_value.arg_types
                              (OBus_member.Method.i_args info)),
                         handler);
    member_introspection = OBus_introspect.Method(OBus_member.Method.member info,
                                                  make_args (OBus_member.Method.i_args info),
                                                  make_args (OBus_member.Method.o_args info),
                                                  []);
  }

let method_info info f =
  _method_info info
    (fun context obj ->
       match obj.data with
         | Some data -> f context data
         | None -> assert false)

let signal_info info = {
  member_name = OBus_member.Signal.member info;
  member_type = Signal;
  member_introspection = OBus_introspect.Signal(OBus_member.Signal.member info,
                                                make_args (OBus_member.Signal.args info),
                                                []);
}

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
    member_name = OBus_member.Property.member info;
    member_type = Property(signal, setter);
    member_introspection = (
      OBus_introspect.Property
        (OBus_member.Property.member info,
         OBus_value.C.type_single typ,
         (match OBus_member.Property.access info with
            | OBus_member.Property.Readable ->
                OBus_introspect.Read
            | OBus_member.Property.Writable ->
                OBus_introspect.Write
            | OBus_member.Property.Readable_writable ->
                OBus_introspect.Read_write),
         [])
    );
  }

let property_r_info info signal = property_info info (Some signal) None
let property_w_info info setter = property_info info None (Some setter)
let property_rw_info info signal setter = property_info info (Some signal) (Some setter)

(* +-----------------------------------------------------------------+
   | Interfaces creation                                             |
   +-----------------------------------------------------------------+ *)

let notify_none = {
  notify_emit = (fun _ _ _ -> return ());
  notify_signature = [];
}

let make_interface_unsafe ?(notify_mode=notify_none) name methods signals properties = {
  interface_name = name;
  interface_introspect =
    (name,
     Array.fold_right
       (fun member acc -> member.member_introspection :: acc)
       methods
       (Array.fold_right
          (fun member acc -> member.member_introspection :: acc)
          signals
          (Array.fold_right
             (fun member acc -> member.member_introspection :: acc)
             properties
             [])),
     []);
  interface_methods =
    Array.map
      (function
         | { member_name = name;
             member_type = Method(typ, handler) } -> {
             method_name = name;
             method_type = typ;
             method_handler = handler;
           }
         | _ ->
             assert false)
      methods;
  interface_properties =
    Array.map
      (function
         | { member_name = name;
             member_type = Property(signal, setter) } -> {
             property_name = name;
             property_signal = signal;
             property_setter = setter;
           }
         | _ ->
             assert false)
      properties;
  interface_notify_mode = notify_mode;
}

let compare_member a b =
  match String.compare a.member_name b.member_name with
    | 0 ->
        (match a.member_type, b.member_type with
           | Method(typ_a, _), Method(typ_b, _) -> Pervasives.compare typ_a typ_b
           | _ -> 0)
    | n ->
        n

let make_interface ?notify_mode name members =
  let methods =
    Array.of_list
      (List.filter
         (function
            | { member_type = Method _ } -> true
            | _ -> false)
         members)
  and signals =
    Array.of_list
      (List.filter
         (function
            | { member_type = Signal } -> true
            | _ -> false)
         members)
  and properties =
    Array.of_list
      (List.filter
         (function
            | { member_type = Property _ } -> true
            | _ -> false)
         members)
  in
  Array.sort compare_member methods;
  Array.sort compare_member signals;
  Array.sort compare_member properties;
  make_interface_unsafe ?notify_mode name methods signals properties

let add_interface obj interface =
  obj.interfaces <- Interface_map.add interface.interface_name interface obj.interfaces;
  generate obj

let remove_interface_by_name obj name =
  obj.interfaces <- Interface_map.remove name obj.interfaces;
  generate obj

let remove_interface obj interface =
  remove_interface_by_name obj interface.interface_name

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
   | Common interfaces                                               |
   +-----------------------------------------------------------------+ *)

let introspectable () =
  let interface_name = "org.freedesktop.DBus.Introspectable" in
  make_interface_unsafe interface_name [|
    _method_info
      (OBus_member.Method.make
         interface_name
         "Introspect"
         OBus_value.arg0
         (OBus_value.arg1
            (Some "resul", OBus_value.C.basic_string)))
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
         return (Buffer.contents buf))
  |] [||] [||]

let properties () =
  let interface_name = "org.freedesktop.DBus.Properties" in
  make_interface_unsafe interface_name [|
    _method_info
      (OBus_member.Method.make
         interface_name
         "Get"
         (OBus_value.arg2
            (Some "interface", OBus_value.C.basic_string)
            (Some "member", OBus_value.C.basic_string))
         (OBus_value.arg1
            (Some "value", OBus_value.C.variant)))
      (fun context obj (interface, member) ->
         match binary_search compare_interface interface obj.properties with
           | None ->
               Printf.ksprintf (OBus_error.fail OBus_error.Failed) "no such interface: %S" interface
           | Some(interface, properties) ->
               match binary_search compare_property member properties with
                 | Some{ property_instance_signal = Some s } ->
                     return (React.S.value s)
                 | Some{ property_instance_signal = None } ->
                     Printf.ksprintf (OBus_error.fail OBus_error.Failed) "property %S on interface %S is not readable" member interface
                 | None ->
                     Printf.ksprintf (OBus_error.fail OBus_error.Failed) "no such property: %S on interface %S" member interface);

    _method_info
      (OBus_member.Method.make
         interface_name
         "GetAll"
         (OBus_value.arg1
            (Some "interface", OBus_value.C.basic_string))
         (OBus_value.arg1
            (Some "values", OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)))
      (fun context obj interface ->
         match binary_search compare_interface interface obj.properties with
           | Some(interface, properties) ->
               return (Array.fold_left
                         (fun acc property ->
                            match property.property_instance_signal with
                              | Some s -> (property.property_instance_name, React.S.value s) :: acc
                              | None -> acc)
                         []
                         properties)
           | None ->
               return []);
    _method_info
      (OBus_member.Method.make
         interface_name
         "Set"
         (OBus_value.arg3
            (Some "interface", OBus_value.C.basic_string)
            (Some "member", OBus_value.C.basic_string)
            (Some "value", OBus_value.C.variant))
         OBus_value.arg0)
      (fun context obj (interface, member, value) ->
         match binary_search compare_interface interface obj.properties with
           | None ->
               Printf.ksprintf (OBus_error.fail OBus_error.Failed) "no such interface: %S" interface
           | Some(interface, properties) ->
               match binary_search compare_property member properties with
                 | Some{ property_instance_setter = Some f } -> begin
                     match obj.data with
                       | Some data -> f context data value
                       | None -> assert false
                   end
                 | Some{ property_instance_setter = None } ->
                     Printf.ksprintf (OBus_error.fail OBus_error.Failed) "property %S on interface %S is not writable" member interface
                 | None ->
                     Printf.ksprintf (OBus_error.fail OBus_error.Failed) "no such property: %S on interface %S" member interface);
  |] [||] [||]


(* +-----------------------------------------------------------------+
   | Constructors                                                    |
   +-----------------------------------------------------------------+ *)

let make ?owner ?(common=true) ?(interfaces=[]) path =
  let interfaces = if common then introspectable () :: properties () :: interfaces else interfaces in
  let obj = {
    path = path;
    exports = Connection_set.empty;
    owner = owner;
    data = None;
    methods = [||];
    properties = [||];
    interfaces =
      List.fold_left
        (fun acc iface -> Interface_map.add iface.interface_name iface acc)
        Interface_map.empty
        interfaces;
    changed = Interface_map.empty;
  } in
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

(* +-----------------------------------------------------------------+
   | Notification modes                                              |
   +-----------------------------------------------------------------+ *)

let notify_custom ~emit ~signature = {
  notify_emit = emit;
  notify_signature = signature;
}

let notify_global name = {
  notify_emit = (fun obj interface members ->
                   emit obj ~interface ~member:name OBus_value.C.seq0 ());
  notify_signature = [OBus_introspect.Signal(name, [], [])];
}

let notify_update name = {
  notify_emit = (fun obj interface members ->
                   emit obj ~interface ~member:name
                     (OBus_value.C.seq1
                        (OBus_value.C.dict OBus_value.C.string OBus_value.C.variant))
                     (Member_map.fold (fun name value acc -> (name, value) :: acc) members []));
  notify_signature = [OBus_introspect.Signal(name, [(None, OBus_value.T.Dict(OBus_value.T.String, OBus_value.T.Variant))], [])];
}
