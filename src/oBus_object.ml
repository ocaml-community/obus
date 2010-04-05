(*
 * oBus_object.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open Lwt
open OBus_message
open OBus_introspect
open OBus_value
open OBus_private_connection
open OBus_connection
open OBus_pervasives

(* Numebrs used to generate new unique object paths: *)
let object_unique_id = ref(0, 0)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Connection_set = Set.Make(OBus_connection)
module Interface_map = String_map
module Member_map = String_map

type notify_mode = {
  notify_send : OBus_connection.t -> OBus_name.bus option -> OBus_path.t -> OBus_name.interface -> OBus_value.single Member_map.t -> unit Lwt.t;
  (* The function which send the notification *)

  notify_signature : OBus_introspect.member list;
  (* Additional members for the interface *)
}

(* A method descriptor *)
type method_desc = {
  method_name : OBus_name.member;
  method_handler : OBus_pack.t -> OBus_connection.t -> OBus_message.t -> unit Lwt.t;
}

(* A proeprty descriptor *)
type property_desc = {
  property_name : OBus_name.member;
  property_signal : (OBus_pack.t -> OBus_value.single React.signal) option;
  property_setter : (OBus_pack.t -> OBus_value.single -> unit Lwt.t) option;
}

(* Result of an property applied to an object *)
type property_instance = {
  property_instance_name : OBus_name.member;
  property_instance_signal : (OBus_value.single React.signal) option;
  property_instance_setter : (OBus_pack.t -> OBus_value.single -> unit Lwt.t) option;
  property_instance_monitor : unit React.event;
}

(* An interface descriptor *)
type interface_desc = {
  interface_name : OBus_name.interface;
  (* The name of the interface *)

  interface_introspect : OBus_introspect.interface;
  (* For introspection *)

  interface_methods : method_desc array;
  (* For dispatching method calls *)

  interface_properties : property_desc array;
  (* List for properties, for reading/writing properties *)

  interface_notify : notify_mode;
  (* Notification mode for this interface *)
}

(* D-Bus object informations *)
type info = {
  mutable pack : OBus_pack.t;
  (* The pack containing the user object assiociated for this
     descriptor *)

  path : OBus_path.t;
  (* The path of the object *)

  exports : Connection_set.t React.signal;
  (* Set of connection on which the object is exported *)

  set_exports : Connection_set.t -> unit;
  (* Setter for [exports] *)

  owner : OBus_peer.t option;
  (* The optionnal object owner *)

  mutable interfaces : interface_desc Interface_map.t;
  (* Interfaces implemented byu the object *)

  mutable methods : (OBus_name.interface * method_desc array) array;
  (* All methods of the object *)

  mutable properties : (OBus_name.interface * property_instance array) array;
  (* All properties of the object *)

  mutable changed : OBus_value.single Member_map.t Interface_map.t;
  (* Properties that changed since the last upadte *)
}

type t = info
    (* The doefault type for object is directly D-Bus informations *)

(* +-----------------------------------------------------------------+
   | Binary search                                                   |
   +-----------------------------------------------------------------+ *)

let binary_search get key array =
  let rec loop a b =
    if a = b then
      None
    else begin
      let middle = (a + b) / 2 in
      let element = Array.unsafe_get array middle in
      let cmp = String.compare key (get element) in
      if cmp = 0 then
        Some element
      else if cmp < 0 then
        loop a middle
      else
        loop (middle + 1) b
    end
  in
  loop 0 (Array.length array)

(* +-----------------------------------------------------------------+
   | Property change notifications                                   |
   +-----------------------------------------------------------------+ *)

let notify_none = {
  notify_send = (fun _ _ _ _ _ -> return ());
  notify_signature = [];
}

(* The function which send the notification *)
let handle_property_change info interface_name member_name value =
  let empty = Interface_map.is_empty info.changed in
  info.changed <- (
    Interface_map.add interface_name
      (Member_map.add member_name value
         (try Interface_map.find interface_name info.changed with Not_found -> Member_map.empty))
      info.changed
  );
  if empty then ignore begin
    (* Sleep a bit, so multiple changes are sent only one time. *)
    lwt () = pause () in
    let changed = info.changed in
    info.changed <- Interface_map.empty;
    match info.owner with
      | Some peer ->
          Interface_map.iter
            (fun name properties ->
               match try Some(Interface_map.find name info.interfaces) with Not_found -> None with
                 | Some interface ->
                     ignore (interface.interface_notify.notify_send
                               (OBus_peer.connection peer)
                               (OBus_peer.name peer)
                               info.path
                               name
                               properties)
                 | None ->
                     ())
            changed;
          return ()
      | None ->
          Connection_set.iter
            (fun connection ->
               Interface_map.iter
                 (fun name properties ->
                    match try Some(Interface_map.find name info.interfaces) with Not_found -> None with
                      | Some interface ->
                          ignore (interface.interface_notify.notify_send
                                    connection
                                    None
                                    info.path
                                    name
                                    properties)
                      | None ->
                          ())
                 changed)
            (React.S.value info.exports);
          return ()
  end

(* +-----------------------------------------------------------------+
   | Method and property maps geenration                             |
   +-----------------------------------------------------------------+ *)

let dummy = ("", [||])

(* Generate the [methods] and [properties] fields from the
   [interfaces] field: *)
let generate info =
  (* Stop monitoring of previous properties *)
  Array.iter
    (fun (interface, properties) ->
       Array.iter
         (fun property -> React.E.stop property.property_instance_monitor)
         properties)
    info.properties;
  let count = Interface_map.fold (fun _ _ count -> succ count) info.interfaces 0 in
  let properties = Array.make count dummy and methods = Array.make count dummy in
  ignore (
    Interface_map.fold
      (fun name interface i ->
         Array.unsafe_set properties i
           (name,
            Array.map
              (fun property ->
                 let signal, monitor =
                   match property.property_signal with
                     | Some f ->
                         let signal = f info.pack in
                         (Some signal,
                          React.E.map
                            (handle_property_change info interface.interface_name property.property_name)
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
         Array.unsafe_set methods i (name, interface.interface_methods);
         i + 1)
      info.interfaces 0
  );
  info.properties <- properties;
  info.methods <- methods

(* +-----------------------------------------------------------------+
   | Interface                                                       |
   +-----------------------------------------------------------------+ *)

let dummy_method = { method_name = ""; method_handler = (fun pack connection message -> return ()) }
let dummy_property = { property_name = ""; property_signal = None; property_setter = None }

module Interface =
struct
  (* An interface builder: *)
  type 'obj builder = {
    name : OBus_name.interface;
    (* Name of the interface *)

    unpack : OBus_pack.t -> 'obj;
    (* Unpack a packed object *)

    get : 'obj -> info;
    (* Retreive object informations *)

    notify : notify_mode;
    (* Property change noficiation mode *)

    mutable members : OBus_introspect.member Member_map.t;
    (* Members of the interface, for introspection *)

    mutable methods : method_desc Member_map.t;
    (* Descriptoins of all methods of the interface *)

    mutable properties : property_desc Member_map.t;
    (* Descriptions of all properties of the interface *)
  }

  (* State of an interface *)
  type 'obj state =
    | Building of 'obj builder
        (* The interface is being built *)
    | Finished of ('obj -> t) * interface_desc
        (* Building done *)

  type 'obj t = 'obj state ref
      (* Type of an interface *)

  let make name unpack get notify =
    ref (Building{
           name = name;
           unpack = unpack;
           get = get;
           notify = notify;
           members =
             List.fold_left
               (fun map member ->
                  Member_map.add
                    (match member with
                       | Method(name, _, _, _)
                       | Signal(name, _, _)
                       | Property(name, _, _, _) -> name)
                    member map)
               Member_map.empty
               notify.notify_signature;
           methods = Member_map.empty;
           properties = Member_map.empty;
         })

  let array_of_member_map map dummy =
    let count = Member_map.fold (fun key value n -> n + 1) map 0 in
    let array = Array.make count dummy in
    ignore (Member_map.fold
              (fun key value i ->
                 Array.unsafe_set array i value;
                 i + 1)
              map 0);
    array

  let finished iface = match !iface with
    | Building builder ->
        let interface_desc = {
          interface_name = builder.name;
          interface_introspect = (builder.name, List.rev (Member_map.fold (fun name definition acc -> definition :: acc) builder.members []), []);
          interface_methods = array_of_member_map builder.methods dummy_method;
          interface_properties = array_of_member_map builder.properties dummy_property;
          interface_notify = builder.notify;
        } in
        iface := Finished(builder.get, interface_desc);
        interface_desc
    | Finished(_, interface_desc) ->
        interface_desc

  let close iface = ignore (finished iface)

  let name iface = match !iface with
    | Building{ name = name }
    | Finished(_, { interface_name = name }) -> name

  let introspect iface = (finished iface).interface_introspect

  let get_builder caller iface = match !iface with
    | Building builder ->
        builder
    | Finished(_, { interface_name = name }) ->
        ksprintf failwith "OBus_object.Interface.%s: The interface %S cannot register any new member" caller name

  let with_names typs = List.map (fun t -> (None, t)) typs

  type 'a result =
    | Value of 'a
    | Error of exn

  let method_call iface member typ f =
    let builder = get_builder "method_call" iface
    and isig = OBus_type.isignature typ
    and osig = OBus_type.osignature typ in
    builder.members <- Member_map.add member (Method(member, with_names isig, with_names osig, [])) builder.members;
    let unpack = builder.unpack in
    let handler pack connection message =
      let context = (connection, message) in
      try_bind
        (fun () ->
           match try Value(OBus_type.cast_func typ ~context message.body) with exn -> Error exn with
             | Value apply ->
                 apply (f (unpack pack))
             | Error(OBus_type.Cast_failure(func, reason)) ->
                 let body_sig = type_of_sequence message.body in
                 if body_sig <> isig then
                   ksprintf failwith
                     "invalid signature for method %S of interface %S: '%s', should be '%s'"
                     member
                     builder.name
                     (string_of_signature (type_of_sequence message.body))
                     (string_of_signature isig)
                 else
                   ksprintf failwith "%s.%s: failed to cast method-call contents: %s" builder.name member reason
             | Error exn ->
                 fail exn)
        (OBus_method.return ~context (OBus_type.func_reply typ))
        (OBus_method.fail ~context)
    in
    builder.methods <- Member_map.add member { method_name = member; method_handler = handler } builder.methods

  let signal iface member typ =
    let builder = get_builder "signal" iface in
    builder.members <- Member_map.add member (Signal(member, with_names (OBus_type.type_sequence typ), [])) builder.members

  let emit iface member typ obj ?peer value =
    let interface_name, info = match !iface with
      | Building{ name = name; get = get }
      | Finished(get, { interface_name = name }) -> (name, get obj)
    and body = OBus_type.make_sequence typ value in
    match peer, info.owner with
      | Some { OBus_peer.connection = connection; OBus_peer.name = destination }, _
      | _, Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
          OBus_signal.dyn_emit ~connection ?destination ~interface:interface_name ~member ~path:info.path body
      | None, None ->
          join (Connection_set.fold
                  (fun connection l -> OBus_signal.dyn_emit ~connection ~interface:interface_name ~member ~path:info.path body :: l)
                  (React.S.value info.exports)
                  [])

  let property iface member typ signal setter mode =
    let builder = get_builder "property" iface and ty = OBus_type.type_single typ in
    builder.members <- Member_map.add member (Property(member, ty, mode, [])) builder.members;
    let unpack = builder.unpack in
    builder.properties <- Member_map.add member {
      property_name = member;
      property_signal = (match signal with
                           | None ->
                               None
                           | Some f ->
                               Some(fun pack -> React.S.map (OBus_type.make_single typ) (f (unpack pack))));
      property_setter = (match setter with
                           | None ->
                               None
                           | Some f ->
                               Some(fun pack value ->
                                      match try Value(OBus_type.cast_single typ value) with exn -> Error exn with
                                        | Value x ->
                                            f (unpack pack) x
                                        | Error(OBus_type.Cast_failure(func, reason)) ->
                                            let value_type = type_of_single value in
                                            if value_type <> ty then
                                              ksprintf failwith "invalid type for property %S of interface %S: '%s', should be '%s'"
                                                member
                                                builder. name
                                                (string_of_signature [value_type])
                                                (string_of_signature [ty])
                                            else
                                              ksprintf failwith "%s.%s: failed to cast property contents: %s" builder.name member reason
                                        | Error exn ->
                                            fail exn));
    } builder.properties

  let property_r iface member typ signal = property iface member typ (Some signal) None Read
  let property_w iface member typ setter = property iface member typ None (Some setter) Write
  let property_rw iface member typ signal setter = property iface member typ (Some signal) (Some setter) Read_write
end

(* +-----------------------------------------------------------------+
   | Objects signature                                               |
   +-----------------------------------------------------------------+ *)

module type S = sig
  type obj with obus(basic)
  val make_interface : ?notify : notify_mode -> OBus_name.interface -> obj Interface.t
  val add_interface : obj -> obj Interface.t -> unit
  val remove_interface : obj -> obj Interface.t -> unit
  val remove_interface_by_name : obj -> OBus_name.interface -> unit
  val introspectable : obj Interface.t
  val properties : obj Interface.t
  val make : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : obj Interface.t list -> OBus_path.t -> (t -> obj) -> obj
  val make' : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : obj Interface.t list -> (t -> obj) -> obj
  val path : obj -> OBus_path.t
  val owner : obj -> OBus_peer.t option
  val exports : obj -> Set.Make(OBus_connection).t React.signal
  val introspect : obj -> OBus_introspect.interface list
  val export : OBus_connection.t -> obj -> unit
  val remove : OBus_connection.t -> obj -> unit
  val destroy : obj -> unit
  val dynamic : connection : OBus_connection.t -> prefix : OBus_path.t -> handler : (OBus_path.t -> obj Lwt.t) -> unit
  val emit : obj ->
    interface : OBus_name.interface ->
    member : OBus_name.member ->
    ('a, _) OBus_type.cl_sequence ->
    ?peer : OBus_peer.t -> 'a -> unit Lwt.t
end

module type Custom = sig
  type obj
  val cast : obj -> t
end

(* +-----------------------------------------------------------------+
   | Objects implementation                                          |
   +-----------------------------------------------------------------+ *)

let default_destroy info =
  Connection_set.iter
    (fun connection -> match connection#get with
       | Crashed exn ->
           ()
       | Running running ->
           running.running_static_objects <- Object_map.remove info.path running.running_static_objects)
    (React.S.value info.exports);
  info.set_exports Connection_set.empty

module Make(Object : Custom) : S with type obj = Object.obj =
struct
  module Pack = OBus_pack.Make(struct type t = Object.obj end)

  (* +---------------------------------------------------------------+
     | Type and type combinator                                      |
     +---------------------------------------------------------------+ *)

  type obj = Object.obj

  let obus_obj = OBus_type.map_with_context obus_path
    (fun (connection, context) path ->
       match connection#get with
         | Crashed _ ->
             raise (OBus_type.Cast_failure("OBus_object.Make.obus_obj", "connection crashed"))
         | Running running ->
             match try Some(Object_map.find path running.running_static_objects) with Not_found -> None with
               | Some  { static_object_object = pack } -> begin
                   try
                     Pack.unpack pack
                   with Invalid_argument _ ->
                     raise (OBus_type.Cast_failure("OBus_object.Make.obus_obj",
                                                   sprintf
                                                     "unexpected type for object with path %S"
                                                     (OBus_path.to_string path)))
                 end
               | None ->
                   raise (OBus_type.Cast_failure("OBus_object.Make.obus_obj",
                                                 sprintf
                                                   "cannot find object with path %S"
                                                   (OBus_path.to_string path))))
    (fun obj -> (Object.cast obj).path)

  (* +---------------------------------------------------------------+
     | Parameters                                                    |
     +---------------------------------------------------------------+ *)

  let path obj = (Object.cast obj).path
  let owner obj = (Object.cast obj).owner
  let exports obj = (Object.cast obj).exports
  let introspect obj =
    List.rev
      (Interface_map.fold
         (fun name iface acc -> iface.interface_introspect :: acc)
         (Object.cast obj).interfaces
         [])

  (* +---------------------------------------------------------------+
     | Export/remove                                                 |
     +---------------------------------------------------------------+ *)

  let handle_call pack connection message =
    let info = Object.cast (Pack.unpack pack) in
    match message with
      | { typ = Method_call(path, Some interface, member) } -> begin
          match binary_search fst interface info.methods with
            | None ->
                OBus_method.fail (connection, message) (unknown_method_exn message)
            | Some(interface, methods) ->
                match binary_search (fun meth -> meth.method_name) member methods with
                  | None ->
                      OBus_method.fail  (connection, message) (unknown_method_exn message)
                  | Some meth ->
                      meth.method_handler pack connection message
        end
      | { typ = Method_call(path, None, member) } -> begin
          let count = Array.length info.methods in
          let rec loop i =
            if i = count then
              OBus_method.fail  (connection, message) (unknown_method_exn message)
            else
              match binary_search (fun meth -> meth.method_name) member (snd info.methods.(i)) with
                | Some meth ->
                    meth.method_handler pack connection message
                | None ->
                    loop (i + 1)
          in
          loop 0
        end
      | _ ->
          invalid_arg "OBus_object.Make.handle_call"

  let export connection obj =
    let running = running_of_connection connection in
    let info = Object.cast obj in
    if not (Connection_set.mem connection (React.S.value info.exports)) then begin
      running.running_static_objects <- Object_map.add info.path {
        static_object_handle = handle_call;
        static_object_object = info.pack;
        static_object_connection_closed = (fun connection -> info.set_exports (Connection_set.remove connection (React.S.value info.exports)));
      } running.running_static_objects;
      info.set_exports (Connection_set.add connection (React.S.value info.exports))
    end

  let remove connection obj =
    let info = Object.cast obj in
    if Connection_set.mem connection (React.S.value info.exports) then begin
      info.set_exports (Connection_set.remove connection (React.S.value info.exports));
      match connection#get with
        | Crashed _ ->
            ()
        | Running running ->
            running.running_static_objects <- Object_map.remove info.path running.running_static_objects
    end

  let destroy obj =
    default_destroy (Object.cast obj)

  let dynamic ~connection ~prefix ~handler =
    let running = running_of_connection connection in
    (* Remove any dynamic node declared with the same prefix: *)
    let create path =
      lwt obj = handler path in
      return {
        static_object_handle = handle_call;
        static_object_object = Pack.pack obj;
        static_object_connection_closed = ignore;
      }
    in
    running.running_dynamic_objects <- Object_map.add prefix create running.running_dynamic_objects

  (* +---------------------------------------------------------------+
     | Signals                                                       |
     +---------------------------------------------------------------+ *)

  let emit obj ~interface ~member typ ?peer x =
    let body = OBus_type.make_sequence typ x and obj = Object.cast obj in
    match peer, obj.owner with
      | Some { OBus_peer.connection = connection; OBus_peer.name = destination }, _
      | _, Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
          OBus_signal.dyn_emit ~connection ?destination ~interface ~member ~path:obj.path body
      | None, None ->
          join (Connection_set.fold
                  (fun connection l -> OBus_signal.dyn_emit ~connection ~interface ~member ~path:obj.path body :: l)
                  (React.S.value obj.exports)
                  [])

  (* +---------------------------------------------------------------+
     | Interfaces                                                    |
     +---------------------------------------------------------------+ *)

  let make_interface ?(notify=notify_none) name = Interface.make name Pack.unpack Object.cast notify

  let add_interface obj iface =
    let info = Object.cast obj and iface = Interface.finished iface in
    info.interfaces <- Interface_map.add iface.interface_name iface info.interfaces;
    generate info

  let remove_interface_by_name obj name =
    let info = Object.cast obj in
    info.interfaces <- Interface_map.remove name info.interfaces;
    generate info

  let remove_interface obj iface =
    remove_interface_by_name obj (Interface.name iface)

  (* +---------------------------------------------------------------+
     | Common interfaces                                             |
     +---------------------------------------------------------------+ *)

  let introspectable = make_interface "org.freedesktop.DBus.Introspectable"

  let () =
    Interface.method_call introspectable "Introspect" (<:obus_func< context -> OBus_introspect.document >>)
      (fun obj (connection, message) ->
         let info = Object.cast obj in
         return (introspect obj,
                 match connection#get with
                   | Crashed _ ->
                       []
                   | Running connection ->
                       children connection info.path));
    Interface.close introspectable

  let properties = make_interface "org.freedesktop.DBus.Properties"

  let () =
    Interface.method_call properties "Get" (<:obus_func< string -> string -> variant >>)
      (fun obj interface member ->
         let info = Object.cast obj in
         match binary_search fst interface info.properties with
           | None ->
               fail (Failure (sprintf "no such interface: %S" interface))
           | Some(interface, properties) ->
               match binary_search (fun prop -> prop.property_instance_name) member properties with
                 | Some{ property_instance_signal = Some s } ->
                     return (React.S.value s)
                 | Some{ property_instance_signal = None } ->
                     fail (Failure (sprintf "property %S on interface %S is not readable" member interface))
                 | None ->
                     fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "Set" (<:obus_func< string -> string -> variant -> unit >>)
      (fun obj interface member x ->
         let info = Object.cast obj in
         match binary_search fst interface info.properties with
           | None ->
               fail (Failure (sprintf "no such interface: %S" interface))
           | Some(interface, properties) ->
               match binary_search (fun prop -> prop.property_instance_name) member properties with
                 | Some{ property_instance_setter = Some f } ->
                     f info.pack x
                 | Some{ property_instance_setter = None } ->
                     fail (Failure (sprintf "property %S on interface %S is not writable" member interface))
                 | None ->
                     fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "GetAll" (<:obus_func< string -> (string, variant) dict >>)
      (fun obj interface ->
         let info = Object.cast obj in
         match binary_search fst interface info.properties with
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
    Interface.close properties

  (* +---------------------------------------------------------------+
     | Constructors                                                  |
     +---------------------------------------------------------------+ *)

  let make ?owner ?(common=true) ?(interfaces=[]) path f =
    let interfaces = if common then introspectable :: properties :: interfaces else interfaces in
    let exports, set_exports = React.S.create ~eq:Connection_set.equal Connection_set.empty in
    let info = {
      pack = OBus_pack.dummy;
      path = path;
      exports = exports;
      set_exports = set_exports;
      owner = owner;
      methods = [||];
      properties = [||];
      interfaces =
        List.fold_left
          (fun acc iface ->
             let iface = Interface.finished iface in
             Interface_map.add iface.interface_name iface acc)
          Interface_map.empty
          interfaces;
      changed = Interface_map.empty;
    } in
    let obj = f info in
    info.pack <- Pack.pack obj;
    generate info;
    let () =
      match owner with
        | None ->
            ()
        | Some peer ->
            export (OBus_peer.connection peer) obj;
            ignore (lwt () = OBus_peer.wait_for_exit peer in
                    default_destroy info;
                    return ())
    in
    obj

  let make' ?owner ?common ?interfaces f =
    let id1 , id2 = !object_unique_id in
    let id2 = id2 + 1 in
    if id2 < 0 then
      object_unique_id := (id1 + 1, 0)
    else
      object_unique_id := (id1, id2);
    make ?owner ?common ?interfaces ["ocaml"; sprintf "%d_%d" id1 id2] f
end

(* +-----------------------------------------------------------------+
   | Implementation for native objects                               |
   +-----------------------------------------------------------------+ *)

include Make(struct
               type obj = t
               let cast obj = obj
             end)

let obus_t = obus_obj

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let remove_by_path connection path =
  let running = running_of_connection connection in
  running.running_dynamic_objects <- Object_map.remove path running.running_dynamic_objects;
  match try Some(Object_map.find path running.running_static_objects) with Not_found -> None with
    | Some static_object ->
        running.running_static_objects <- Object_map.remove path running.running_static_objects;
        static_object.static_object_connection_closed connection
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Notification modes                                              |
   +-----------------------------------------------------------------+ *)

let notify_custom ~send ~signature = {
  notify_send = send;
  notify_signature = signature;
}

let notify_global name = {
  notify_send = (fun connection owner path interface members ->
                   OBus_signal.emit
                     ~connection
                     ?destination:owner
                     ~path
                     ~interface
                     ~member:name
                     <:obus_type< unit >>
                     ());
  notify_signature = [Signal(name, [], [])];
}

let notify_update name = {
  notify_send = (fun connection owner path interface members ->
                   OBus_signal.emit
                     ~connection
                     ?destination:owner
                     ~path
                     ~interface
                     ~member:name
                     <:obus_type< (string, variant) dict >>
                     (Member_map.fold (fun name value acc -> (name, value) :: acc) members []));
  notify_signature = [Signal(name, [(None, Tdict(Tstring, Tvariant))], [])];
}
