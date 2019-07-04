(*
 * oBus_object.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt_react

let section = Lwt_log.Section.make "obus(object)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Connection_set = Set.Make(OBus_connection)
module String_set = Set.Make(String)
module String_map = Map.Make(String)
module Path_map = Map.Make(OBus_path)

module type Method_info = sig
  type obj
  type i_type
  type o_type
  val info : (i_type, o_type) OBus_member.Method.t
  val handler : obj -> i_type -> o_type Lwt.t
end

module type Signal_info = sig
  type obj
  type typ
  val info : typ OBus_member.Signal.t
end

module type Property_info = sig
  type obj
  type typ
  type access
  val info : (typ, access) OBus_member.Property.t
  val set : (obj -> typ -> unit Lwt.t) option
  val signal : (obj -> typ signal) option
end

module type Property_instance = sig
  type typ
  type access
  val info : (typ, access) OBus_member.Property.t

  val signal : typ signal
    (* The signal holding the current value of the property *)

  val monitor : unit event
    (* Event which send notifications when the contents of the
       property changes *)
end

type property_instance = (module Property_instance)

(* An interface descriptor *)
type 'a interface = {
  i_name : OBus_name.interface;
  (* The name of the interface *)

  i_methods : 'a method_info array;
  (* Array of methods, for dispatching method calls and introspection *)

  i_signals : 'a signal_info array;
  (* Array of signals, for introspection *)

  i_properties : 'a property_info array;
  (* Array of for properties, for reading/writing properties and introspection *)

  i_annotations : OBus_introspect.annotation list;
  (* List of annotations of the interfaces. They are used for
     introspection *)
}

(* D-Bus object informations *)
and 'a t = {
  path : OBus_path.t;
  (* The path of the object *)

  mutable data : 'a option;
  (* Data attached to the object *)

  exports : Connection_set.t signal;
  set_exports : Connection_set.t -> unit;
  (* Set of connection on which the object is exported *)

  owner : OBus_peer.t option;
  (* The optionnal owner of the object *)

  mutable interfaces : 'a interface array;
  (* Interfaces implemented by this object *)

  mutable properties : property_instance option array array;
  (* All property instances of the object *)

  mutable changed : OBus_value.V.single option String_map.t array;
  (* Properties that changed since the last upadte, organised by
     interface *)

  properties_changed : (OBus_name.interface -> (OBus_name.member * OBus_value.V.single option) list -> unit Lwt.t) ref;
  (* Function called when proeprties change. It may emit a
     notification signal. The default one use
     [org.freedesktop.DBus.Properties.PropertiesChanged] *)
}

and 'a method_info = (module Method_info with type obj = 'a t)
and 'a signal_info = (module Signal_info with type obj = 'a t)
and 'a property_info = (module Property_info with type obj = 'a t)

(* Signature for static objects *)
module type Static = sig
  type data
    (* Type of data attached to the obejct *)

  val obj : data t
    (* The object itself *)
end

type static = (module Static)

(* Signature for dynamic object *)
module type Dynamic = sig
  type data
    (* Type of data attached to obejcts *)

  val get : OBus_context.t -> OBus_path.t -> data t Lwt.t
end

type dynamic = (module Dynamic)

(* Informations stored in connections *)
type info = {
  mutable statics : static Path_map.t;
  (* Static objects exported on the connection *)

  mutable dynamics : dynamic Path_map.t;
  (* Dynamic objects exported on the connection *)

  mutable watcher : unit event;
  (* Event which cleanup things when the connection goes down *)
}

(* +-----------------------------------------------------------------+
   | Object parameters                                               |
   +-----------------------------------------------------------------+ *)

let path obj = obj.path
let owner obj = obj.owner
let exports obj = obj.exports

let introspect_args args =
  List.map2
    (fun name_opt typ -> (name_opt, typ))
    (OBus_value.arg_names args)
    (OBus_value.C.type_sequence (OBus_value.arg_types args))

let introspect_method (type d) info =
  let module M = (val info : Method_info with type obj = d t) in
  OBus_member.Method.introspect M.info

let introspect_signal (type d) info =
  let module S = (val info : Signal_info with type obj = d t) in
  OBus_member.Signal.introspect S.info

let introspect_property (type d) info =
  let module P = (val info : Property_info with type obj = d t) in
  OBus_member.Property.introspect P.info

let introspect obj =
  Array.fold_right
    (fun interface acc ->
       let members = [] in
       let members = Array.fold_right (fun member acc -> introspect_property member :: acc) interface.i_properties members in
       let members = Array.fold_right (fun member acc -> introspect_signal member :: acc) interface.i_signals members in
       let members = Array.fold_right (fun member acc -> introspect_method member :: acc) interface.i_methods members in
       (interface.i_name, members, interface.i_annotations) :: acc)
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
  String.compare name interface.i_name

let compare_property (type d) name property =
  let module P = (val property : Property_info with type obj = d t) in
  String.compare name P.info.OBus_member.Property.member

let compare_method (type d) name method_ =
  let module M = (val method_ : Method_info with type obj = d t) in
  String.compare name M.info.OBus_member.Method.member

(* +-----------------------------------------------------------------+
   | Dispatching                                                     |
   +-----------------------------------------------------------------+ *)

let unknown_method interface member arguments =
  Lwt.fail
    (OBus_error.Unknown_method
       (Printf.sprintf
          "Method %S with signature %S on interface %S does not exist"
          member
          (OBus_value.string_of_signature (OBus_value.V.type_of_sequence arguments))
          interface))

(* Executes a method *)
let execute (type d) method_info context obj arguments =
  let module M = (val method_info : Method_info with type obj = d t) in
  let arguments =
    try
      OBus_value.C.cast_sequence
        (OBus_value.arg_types (OBus_member.Method.i_args M.info))
        arguments
    with OBus_value.C.Signature_mismatch ->
      raise
        (OBus_error.Failed
           (Printf.sprintf
              "invalid signature(%S) for method %S on interface %S, must be %S"
              (OBus_value.string_of_signature
                 (OBus_value.V.type_of_sequence arguments))
              (OBus_member.Method.member M.info)
              (OBus_member.Method.interface M.info)
              (OBus_value.string_of_signature
                 (OBus_value.C.type_sequence
                    (OBus_value.arg_types
                       (OBus_member.Method.i_args M.info))))))
  in
  Lwt.with_value OBus_context.key (Some context)
    (fun () ->
       let%lwt reply = M.handler obj arguments in
       Lwt.return (OBus_value.C.make_sequence (OBus_value.arg_types (OBus_member.Method.o_args M.info)) reply))

(* Dispatch a method call to the implementation of the method *)
let dispatch context obj interface member arguments =
  if interface = "" then
    let rec loop i =
      if i = Array.length obj.interfaces then
        unknown_method interface member arguments
      else
        match binary_search compare_method member obj.interfaces.(i).i_methods with
          | -1 ->
              loop (i + 1)
          | index ->
              execute obj.interfaces.(i).i_methods.(index) context obj arguments
    in
    loop 0
  else
    match binary_search compare_interface interface obj.interfaces with
      | -1 ->
          unknown_method interface member arguments
      | index ->
          let interface = obj.interfaces.(index) in
          match binary_search compare_method member interface.i_methods with
            | -1 ->
                unknown_method interface.i_name member arguments
            | index ->
                execute interface.i_methods.(index) context obj arguments

(* Search a dynamic node prefix of [path] in [map]: *)
let search_dynamic path map =
  Path_map.fold
    (fun prefix dynamic acc ->
       match acc with
         | Some _ ->
             acc
         | None ->
             match OBus_path.after prefix path with
               | Some path ->
                   Some(path, dynamic)
               | None ->
                   None)
    map None

let send_reply context value =
  try%lwt
    let open OBus_message in
    OBus_connection.send_message (OBus_context.connection context) {
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = Method_return(OBus_context.serial context);
      destination = OBus_peer.name (OBus_context.sender context);
      sender = "";
      body = value;
    }
  with exn ->
    Lwt_log.warning ~section ~exn "failed to send reply to method call"

let send_error context exn =
  let name, message = OBus_error.cast exn in
  try%lwt
    let open OBus_message in
    OBus_connection.send_message (OBus_context.connection context) {
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = Error(OBus_context.serial context, name);
      destination = OBus_peer.name (OBus_context.sender context);
      sender = "";
      body = [OBus_value.V.basic_string message];
    }
  with exn ->
    Lwt_log.warning ~section ~exn "failed to send error in reply to method call"

(* Returns the list of children of a node *)
let children info prefix =
  String_set.elements
    (Path_map.fold
       (fun path obj acc -> match OBus_path.after prefix path with
          | Some(element :: _) -> String_set.add element acc
          | _ -> acc)
       info.statics
       String_set.empty)

exception No_such_object

(* Handle method call messages *)
let handle_message connection info message =
  match message with
    | { OBus_message.typ = OBus_message.Method_call(path, interface, member) } ->
        ignore begin
          let context = OBus_context.make connection message in
          try%lwt
            let%lwt reply =
              (* First, we search the object in static objects *)
              match try Some(Path_map.find path info.statics) with Not_found -> None with
                | Some static ->
                    let module M = (val static : Static) in
                    dispatch context M.obj interface member (OBus_message.body message)
                | None ->
                    (* Then we search in dynamic objects *)
                    match search_dynamic path info.dynamics with
                      | None ->
                          Lwt.fail No_such_object
                      | Some(path, dynamic) ->
                          let module M = (val dynamic : Dynamic) in
                          let%lwt result =
                            try%lwt
                              let%lwt obj = M.get context path in
                              Lwt.return (`Success obj)
                            with exn ->
                              Lwt.return (`Failure exn)
                          in
                          match result with
                            | `Success obj ->
                                dispatch context obj interface member (OBus_message.body message)
                            | `Failure Not_found ->
                                Lwt.fail No_such_object
                            | `Failure exn ->
                                let%lwt () = Lwt_log.error ~section ~exn "dynamic object handler failed with" in
                                Lwt.fail No_such_object
            in
            send_reply context reply
          with
            | No_such_object -> begin
                (* Handle introspection for missing intermediate object:

                   for example if we have only one exported object
                   with path "/a/b/c", we need to add introspection
                   support for virtual objects with path "/", "/a",
                   "/a/b", "/a/b/c". *)
                match interface, member, OBus_message.body message with
                  | ("" | "org.freedesktop.DBus.Introspectable"), "Introspect", [] ->
                      let buffer = Buffer.create 1024 in
                      OBus_introspect.output
                        (Xmlm.make_output ~nl:true ~indent:(Some 2) (`Buffer buffer))
                        ([], children info path);
                      send_reply context [OBus_value.V.basic_string (Buffer.contents buffer)]
                  | _ ->
                      send_error context (OBus_error.Unknown_object (Printf.sprintf "Object %S does not exists" (OBus_path.to_string path)))
              end
            | exn ->
                let%lwt () =
                  if OBus_error.name exn = OBus_error.ocaml then
                    (* It is a bad thing to raise an error that is not
                       mapped to a D-Bus error, so we alert the
                       user: *)
                    Lwt_log.error_f ~section ~exn
                      "method call handler for method %S on interface %S failed with"
                      member interface
                  else
                    Lwt.return ()
                in
                send_error context exn
        end;
        Some message

    | _ ->
        Some message

(* +-----------------------------------------------------------------+
   | Exportation                                                     |
   +-----------------------------------------------------------------+ *)

let key = OBus_connection.new_key ()

let cleanup connection info =
  E.stop info.watcher;
  Path_map.iter
    (fun path static ->
       let module M = (val static : Static) in
       M.obj.set_exports (Connection_set.remove connection (S.value M.obj.exports)))
    info.statics

let get_info connection =
  match OBus_connection.get connection key with
    | Some info ->
        info
    | None ->
        let info = {
          statics = Path_map.empty;
          dynamics = Path_map.empty;
          watcher = E.never;
        } in
        OBus_connection.set connection key (Some info);
        let _ = Lwt_sequence.add_r (handle_message connection info) (OBus_connection.incoming_filters connection) in
        info.watcher <- (
          E.map
            (fun state -> cleanup connection info)
            (E.once
               (S.changes
                  (OBus_connection.active connection)))
        );
        info

let remove connection obj =
  let exports = S.value obj.exports in
  if Connection_set.mem connection exports then begin
    if S.value (OBus_connection.active connection) then begin
      match OBus_connection.get connection key with
        | Some info ->
            info.statics <- Path_map.remove obj.path info.statics
        | None ->
            ()
    end;
    obj.set_exports (Connection_set.remove connection exports);
  end

let remove_by_path connection path =
  if S.value (OBus_connection.active connection) then
    match OBus_connection.get connection key with
      | None ->
          ()
      | Some info ->
          info.dynamics <- Path_map.remove path info.dynamics;
          match try Some(Path_map.find path info.statics) with Not_found -> None with
            | Some static ->
                let module M = (val static : Static) in
                remove connection M.obj
            | None ->
                ()

let export (type d) connection obj =
  if obj.data = None then
    failwith "OBus_object.export: cannot export an object without data attached"
  else
    let exports = S.value obj.exports in
    if not (Connection_set.mem connection exports) then begin
      let info = get_info connection in
      let () =
        (* Remove any object registered under the same path: *)
        match try Some(Path_map.find obj.path info.statics) with Not_found -> None with
          | Some static ->
              let module M = (val static : Static) in
              remove connection M.obj
          | None ->
              ()
      in
      let module M = struct
        type data = d
        let obj = obj
      end in
      info.statics <- Path_map.add obj.path (module M : Static) info.statics;
      obj.set_exports (Connection_set.add connection exports)
    end

let destroy obj =
  Connection_set.iter (fun connection -> remove connection obj) (S.value obj.exports)

let dynamic (type d) ~connection ~prefix ~handler =
  let info = get_info connection in
  let module M = struct
    type data = d
    let get = handler
  end in
  info.dynamics <- Path_map.add prefix (module M : Dynamic) info.dynamics

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

let emit obj ~interface ~member ?peer typ x =
  let module M = OBus_message in
  let body = OBus_value.C.make_sequence typ x in
  match peer, obj.owner with
    | Some { OBus_peer.connection; OBus_peer.name }, _
    | _, Some { OBus_peer.connection; OBus_peer.name } ->
        OBus_connection.send_message connection {
          M.flags = { M.no_reply_expected = true; M.no_auto_start = true };
          M.serial = 0l;
          M.typ = OBus_message.Signal(obj.path, interface, member);
          M.destination = name;
          M.sender = "";
          M.body = body;
        }
    | None, None ->
        let signal = {
          M.flags = { M.no_reply_expected = true; M.no_auto_start = true };
          M.serial = 0l;
          M.typ = OBus_message.Signal(obj.path, interface, member);
          M.destination = "";
          M.sender = "";
          M.body = body;
        } in
        Lwt.join (Connection_set.fold
                    (fun connection l -> OBus_connection.send_message connection signal :: l)
                    (S.value obj.exports) [])

(* +-----------------------------------------------------------------+
   | Property change notifications                                   |
   +-----------------------------------------------------------------+ *)

let notify_properties_change (type d) obj interface_name changed index =
  (* Sleep a bit, so multiple changes are sent only one time. *)
  let%lwt () = Lwt.pause () in
  let members = changed.(index) in
  changed.(index) <- String_map.empty;
  try%lwt
    !(obj.properties_changed)
      interface_name
      (String_map.fold (fun name value_opt acc -> (name, value_opt) :: acc) members [])
  with exn ->
    Lwt_log.error ~exn ~section "properties_changed callback failed with"

let handle_property_change obj index info value_opt =
  let empty = String_map.is_empty obj.changed.(index) in
  obj.changed.(index) <- String_map.add (OBus_member.Property.member info) value_opt obj.changed.(index);
  if empty then ignore (notify_properties_change obj (OBus_member.Property.interface info) obj.changed index)

let handle_property_change_true (type d) (type v) obj interface_index prop value =
  let module P = (val prop : Property_info with type obj = d t and type typ = v) in
  let value = OBus_value.C.make_single (OBus_member.Property.typ P.info) value in
  handle_property_change obj interface_index P.info (Some value)

let handle_property_change_invalidates (type d) (type v) obj interface_index prop value =
  let module P = (val prop : Property_info with type obj = d t and type typ = v) in
  handle_property_change obj interface_index P.info None

(* +-----------------------------------------------------------------+
   | Property maps genrations                                        |
   +-----------------------------------------------------------------+ *)

(* Notification mode for a property *)
type emits_signal_changed =
  | Esc_default
      (* Use the default value, which may be defined in the
         interface *)
  | Esc_false
      (* Do not notify property changes *)
  | Esc_true
      (* Notify property changes, and send the new contents in the
         notification *)
  | Esc_invalidates
      (* Only send the property name in changes' notifications *)

let get_emits_changed_signal annotations =
  try
    match List.assoc OBus_introspect.emits_changed_signal annotations with
      | "true" -> Esc_true
      | "false" -> Esc_false
      | "invalidates" -> Esc_invalidates
      | value ->
          ignore (Lwt_log.warning_f "invalid value(%S) for annotation %S. Using default(\"true\")" value OBus_introspect.emits_changed_signal);
          Esc_true
  with Not_found ->
    Esc_default

(* Generate the [properties] field from the [interfaces] field: *)
let generate (type d) obj =
  (* Stop monitoring of previous properties *)
  Array.iter
    (fun instances ->
       Array.iter
         (function
            | Some instance ->
                let module M = (val instance : Property_instance) in
                S.stop M.signal;
                E.stop M.monitor
            | None -> ())
         instances)
    obj.properties;
  let count = Array.length obj.interfaces in
  obj.properties <- Array.make count [||];
  obj.changed <- Array.make count String_map.empty;
  for i = 0 to count - 1 do
    let properties = obj.interfaces.(i).i_properties in
    let count' = Array.length properties in
    let instances = Array.make count' None in
    obj.properties.(i) <- instances;
    for j = 0 to count' - 1 do
      let module P = (val properties.(j) : Property_info with type obj = d t) in
      match P.signal with
        | Some make ->
            let module I = struct
              type typ = P.typ
              type access = P.access
              let info = P.info
              let signal = make obj
              let monitor =
                let esc_prop = get_emits_changed_signal (OBus_member.Property.annotations P.info)
                and esc_intf = get_emits_changed_signal obj.interfaces.(i).i_annotations in
                let info = (module P : Property_info with type obj = d t and type typ = P.typ) in
                match esc_prop, esc_intf with
                  | Esc_false, _ | Esc_default, Esc_false ->
                      E.never
                  | Esc_true, _ | Esc_default, (Esc_default | Esc_true) ->
                      E.map (handle_property_change_true obj i info) (S.changes signal)
                  | Esc_invalidates, _ | Esc_default, Esc_invalidates ->
                      E.map (handle_property_change_invalidates obj i info) (S.changes signal)
            end in
            instances.(j) <- (Some(module I : Property_instance))
        | None ->
            ()
    done
  done

(* +-----------------------------------------------------------------+
   | Member informations                                             |
   +-----------------------------------------------------------------+ *)

let method_info (type d) (type i) (type o) info f =
  let module M = struct
    type obj = d t
    type i_type = i
    type o_type = o
    let info = info
    let handler = f
  end in
  (module M : Method_info with type obj = d t)

let signal_info (type d) (type i) info =
  let module M = struct
    type obj = d t
    type typ = i
    let info = info
  end in
  (module M : Signal_info with type obj = d t)

let property_r_info (type d) (type i) (type a) info signal =
  let module M = struct
    type obj = d t
    type typ = i
    type access = a
    let info = info
    let set = None
    let signal = Some signal
  end in
  (module M : Property_info with type obj = d t)

let property_w_info (type d) (type i) (type a) info set =
  let module M = struct
    type obj = d t
    type typ = i
    type access = a
    let info = info
    let set = Some set
    let signal = None
  end in
  (module M : Property_info with type obj = d t)

let property_rw_info (type d) (type i) (type a) info signal set =
  let module M = struct
    type obj = d t
    type typ = i
    type access = a
    let info = info
    let set = Some set
    let signal = Some signal
  end in
  (module M : Property_info with type obj = d t)

(* +-----------------------------------------------------------------+
   | Interfaces creation                                             |
   +-----------------------------------------------------------------+ *)

let make_interface_unsafe name annotations methods signals properties = {
  i_name = name;
  i_methods = methods;
  i_signals = signals;
  i_properties = properties;
  i_annotations = annotations;
}

let compare_methods (type d) m1 m2 =
  let module M1 = (val m1 : Method_info with type obj = d t) in
  let module M2 = (val m2 : Method_info with type obj = d t) in
  String.compare (OBus_member.Method.member M1.info) (OBus_member.Method.member M2.info)

let compare_signals (type d) s1 s2 =
  let module S1 = (val s1 : Signal_info with type obj = d t) in
  let module S2 = (val s2 : Signal_info with type obj = d t) in
  String.compare (OBus_member.Signal.member S1.info) (OBus_member.Signal.member S2.info)

let compare_properties (type d) p1 p2 =
  let module P1 = (val p1 : Property_info with type obj = d t) in
  let module P2 = (val p2 : Property_info with type obj = d t) in
  String.compare (OBus_member.Property.member P1.info) (OBus_member.Property.member P2.info)

let make_interface ~name ?(annotations=[]) ?(methods=[]) ?(signals=[]) ?(properties=[]) () =
  let methods = Array.of_list methods
  and signals = Array.of_list signals
  and properties = Array.of_list properties in
  Array.sort compare_methods methods;
  Array.sort compare_signals signals;
  Array.sort compare_properties properties;
  make_interface_unsafe name annotations methods signals properties

let process_interfaces interfaces =
  let rec uniq = function
    | iface :: iface' :: rest when iface.i_name = iface'.i_name ->
        uniq (iface :: rest)
    | iface :: rest ->
        iface :: uniq rest
    | [] ->
        []
  and compare i1 i2 =
    String.compare i1.i_name i2.i_name
  in
  Array.of_list (uniq (List.stable_sort compare interfaces))

let add_interfaces obj interfaces =
  obj.interfaces <- process_interfaces (interfaces @ Array.to_list obj.interfaces);
  generate obj

let remove_interfaces_by_names obj names =
  obj.interfaces <- Array.of_list (List.filter (fun iface -> not (List.mem iface.i_name names)) (Array.to_list obj.interfaces));
  generate obj

let remove_interfaces obj interfaces =
  remove_interfaces_by_names obj (List.map (fun iface -> iface.i_name) interfaces)

(* +-----------------------------------------------------------------+
   | Common interfaces                                               |
   +-----------------------------------------------------------------+ *)

open OBus_member

let introspectable (type d) () =
  let interface = "org.freedesktop.DBus.Introspectable" in
  make_interface_unsafe interface []
    [|
      (let module M = struct
         type obj = d t
         type i_type = unit
         type o_type = string

         let info = {
           Method.interface = interface;
           Method.member = "Introspect";
           Method.i_args = OBus_value.arg0;
           Method.o_args = OBus_value.arg1 (Some "result", OBus_value.C.basic_string);
           Method.annotations = [];
         }

         let handler obj () =
           let context = OBus_context.get () in
           let info = get_info (OBus_context.connection context) in
           let buf = Buffer.create 42 in
           OBus_introspect.output
             (Xmlm.make_output ~nl:true ~indent:(Some 2) (`Buffer buf))
             (introspect obj, children info obj.path);
           Lwt.return (Buffer.contents buf)
       end in
       (module M : Method_info with type obj = d t));
    |]
    [||]
    [||]

let properties (type d) () =
  let interface = "org.freedesktop.DBus.Properties" in
  make_interface_unsafe interface []
    [|
      (let module M = struct
         type obj = d t
         type i_type = string * string
         type o_type = OBus_value.V.single

         let info = {
           Method.interface = interface;
           Method.member = "Get";
           Method.i_args =
             OBus_value.arg2
               (Some "interface", OBus_value.C.basic_string)
               (Some "member", OBus_value.C.basic_string);
           Method.o_args =
             OBus_value.arg1
               (Some "value", OBus_value.C.variant);
           Method.annotations = [];
         }

         let handler obj (interface, member) =
           match binary_search compare_interface interface obj.interfaces with
             | -1 ->
                 Lwt.fail (OBus_error.Unknown_interface(Printf.sprintf "Interface %S does not exists" interface))
             | i ->
                 match binary_search compare_property member obj.interfaces.(i).i_properties with
                   | -1 ->
                       Lwt.fail (OBus_error.Unknown_property(Printf.sprintf "Property %S on interface %S does not exists" member interface))
                   | j ->
                       match obj.properties.(i).(j) with
                         | Some instance ->
                             let module I = (val instance : Property_instance) in
                             Lwt.return (OBus_value.C.make_single (Property.typ I.info) (S.value I.signal))
                         | None ->
                             Lwt.fail (OBus_error.Failed(Printf.sprintf "Property %S on interface %S is not readable" member interface))
       end in
       (module M : Method_info with type obj = d t));

      (let module M = struct
         type obj = d t
         type i_type = string
         type o_type = (string * OBus_value.V.single) list

         let info = {
           Method.interface = interface;
           Method.member = "GetAll";
           Method.i_args =
             OBus_value.arg1
               (Some "interface", OBus_value.C.basic_string);
           Method.o_args =
             OBus_value.arg1
               (Some "values", OBus_value.C.dict OBus_value.C.string OBus_value.C.variant);
           Method.annotations = [];
         }

         let handler obj interface =
           match binary_search compare_interface interface obj.interfaces with
             | -1 ->
                 Lwt.fail (OBus_error.Unknown_interface(Printf.sprintf "Interface %S does not exists" interface))
             | i ->
                 let count = Array.length obj.properties.(i) in
                 let rec loop j acc =
                   if j = count then
                     acc
                   else
                     match obj.properties.(i).(j) with
                       | Some instance ->
                           let module I = (val instance : Property_instance) in
                           loop (j + 1)
                             ((Property.member I.info,
                               OBus_value.C.make_single (Property.typ I.info) (S.value I.signal)) :: acc)
                       | None ->
                           loop (j + 1) acc
                 in
                 Lwt.return (loop 0 [])
       end in
       (module M : Method_info with type obj = d t));

      (let module M = struct
         type obj = d t
         type i_type = string * string * OBus_value.V.single
         type o_type = unit

         let info = {
           Method.interface = interface;
           Method.member = "Set";
           Method.i_args =
             OBus_value.arg3
               (Some "interface", OBus_value.C.basic_string)
               (Some "member", OBus_value.C.basic_string)
               (Some "value", OBus_value.C.variant);
           Method.o_args =
             OBus_value.arg0;
           Method.annotations = [];
         }

         let handler obj (interface, member, value) =
           match binary_search compare_interface interface obj.interfaces with
             | -1 ->
                 Lwt.fail (OBus_error.Unknown_interface(Printf.sprintf "Interface %S does not exists" interface))
             | i ->
                 match binary_search compare_property member obj.interfaces.(i).i_properties with
                   | -1 ->
                       Lwt.fail (OBus_error.Unknown_property(Printf.sprintf "Property %S on interface %S does not exists" member interface))
                   | j ->
                       let module P = (val obj.interfaces.(i).i_properties.(j) : Property_info with type obj = d t) in
                       match P.set with
                         | Some f -> begin
                             match try `Success(OBus_value.C.cast_single (Property.typ P.info) value) with exn -> `Failure exn with
                               | `Success value ->
                                   f obj value
                               | `Failure OBus_value.C.Signature_mismatch ->
                                   Lwt.fail
                                     (OBus_error.Failed
                                        (Printf.sprintf
                                           "invalid type(%S) for property %S on interface %S, should be %S"
                                           (OBus_value.string_of_signature
                                              [OBus_value.V.type_of_single value])
                                           member
                                           interface
                                           (OBus_value.string_of_signature
                                              [OBus_value.C.type_single
                                                 (Property.typ P.info)])))
                               | `Failure exn ->
                                   Lwt.fail exn
                           end
                         | None ->
                             Lwt.fail (OBus_error.Property_read_only(Printf.sprintf "property %S on interface %S is not writable" member interface))
       end in
       (module M : Method_info with type obj = d t));
    |]
    [|
      (let module S = struct
         type obj = d t
         type typ = string * (string * OBus_value.V.single) list * string list
         let info = {
           Signal.interface = interface;
           Signal.member = "PropertiesChanged";
           Signal.args =
             OBus_value.arg3
               (Some "interface", OBus_value.C.basic_string)
               (Some "updates", OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)
               (Some "invalidates", OBus_value.C.array OBus_value.C.basic_string);
           Signal.annotations = [];
         }
       end in
       (module S : Signal_info with type obj = d t));
    |]
    [||]

(* +-----------------------------------------------------------------+
   | Constructors                                                    |
   +-----------------------------------------------------------------+ *)

let properties_changed obj interface values =
  emit obj
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"PropertiesChanged"
    (OBus_value.C.seq3
       OBus_value.C.basic_string
       (OBus_value.C.dict OBus_value.C.string OBus_value.C.variant)
       (OBus_value.C.array OBus_value.C.basic_string))
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
  let exports, set_exports = S.create ~eq:Connection_set.equal Connection_set.empty in
  let obj = {
    path = path;
    exports = exports;
    set_exports = set_exports;
    owner = owner;
    data = None;
    properties = [||];
    interfaces = process_interfaces interfaces;
    changed = [||];
    properties_changed = ref (fun name values -> assert false);
  } in
  obj.properties_changed := (fun name values -> properties_changed obj name values);
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
              ignore (let%lwt () = OBus_peer.wait_for_exit peer in
                      destroy obj;
                      Lwt.return ())

let get obj =
  match obj.data with
    | Some data -> data
    | None -> failwith "OBus_object.get: no data attached"
