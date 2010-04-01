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
open OBus_private
open OBus_connection
open OBus_pervasives

(* Numebrs used to generate new unique object paths: *)
let object_unique_id = ref(0, 0)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module MethodMap = OBus_util.MakeMap
  (struct
     type t = OBus_name.interface option * OBus_name.member * signature
     let compare = Pervasives.compare
   end)

module PropertyMap = OBus_util.MakeMap
  (struct
     type t = OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

module ConnectionSet = Set.Make(OBus_connection)

(* Type of a method call handler *)
type untyped_method = packed_object -> OBus_connection.t -> OBus_message.t -> unit Lwt.t

(* Property primitives *)
type untyped_property = {
  up_reader : (packed_object -> OBus_value.single Lwt.t) option;
  up_writer : (packed_object -> OBus_value.single -> unit Lwt.t) option;
}

(* An interface description *)
type untyped_interface = {
  ui_introspect : OBus_introspect.interface;
  (* For introspection *)

  ui_methods : untyped_method MethodMap.t;
  (* For dispatching *)

  ui_properties : untyped_property PropertyMap.t;
  (* List for properties, for reading/writing properties *)
}

type t = {
  path : OBus_path.t;
  exports : ConnectionSet.t React.signal;
  set_exports : ConnectionSet.t -> unit;
  owner : OBus_peer.t option;
  mutable methods : untyped_method MethodMap.t;
  mutable properties : untyped_property PropertyMap.t;
  mutable interfaces : untyped_interface list;
}

(* Generate the [methods] and [properties] fields from the
   [interfaces] field: *)
let generate obj =
  match obj.interfaces with
    | [] ->
        obj.methods <- MethodMap.empty;
        obj.properties <- PropertyMap.empty
    | [ui] ->
        obj.methods <- ui.ui_methods;
        obj.properties <- ui.ui_properties
    | uis ->
        obj.methods <- List.fold_right (fun ui map -> MethodMap.fold MethodMap.add ui.ui_methods map) uis MethodMap.empty;
        obj.properties <- List.fold_right (fun ui map -> PropertyMap.fold PropertyMap.add ui.ui_properties map) uis PropertyMap.empty

let default_destroy obj =
  ConnectionSet.iter
    (fun packed -> match packed#get with
       | Crashed exn ->
           ()
       | Running connection ->
           connection.exported_objects <- ObjectMap.remove obj.path connection.exported_objects)
    (React.S.value obj.exports);
  obj.set_exports ConnectionSet.empty

(* +-----------------------------------------------------------------+
   | Interface                                                       |
   +-----------------------------------------------------------------+ *)

module Interface =
struct
  type 'obj creation = {
    name : OBus_name.interface;
    unpack : packed_object -> 'obj;
    get : 'obj -> t;
    mutable members : OBus_introspect.member list;
    mutable methods : untyped_method MethodMap.t;
    mutable properties : untyped_property PropertyMap.t;
  }

  type 'obj state =
    | Creation of 'obj creation
        (* Members are being added to the interface *)
    | Finished of ('obj -> t) * untyped_interface
        (* Registration is done. *)

  type 'obj t = 'obj state ref

  let make name unpack get =
    ref (Creation{
           name = name;
           unpack = unpack;
           get = get;
           members = [];
           methods = MethodMap.empty;
           properties = PropertyMap.empty;
         })

  let untyped_interface iface = match !iface with
    | Creation cr ->
        let ui = {
          ui_introspect = (cr.name, List.rev cr.members, []);
          ui_methods = cr.methods;
          ui_properties = cr.properties;
        } in
        iface := Finished(cr.get, ui);
        ui
    | Finished(_, ui) ->
        ui

  let close iface = ignore (untyped_interface iface)

  let name iface = match !iface with
    | Creation{ name = name }
    | Finished(_, { ui_introspect = (name, _, _) }) -> name

  let introspect iface = (untyped_interface iface).ui_introspect

  let get_creation caller iface = match !iface with
    | Creation cr ->
        cr
    | Finished(_, { ui_introspect = (name, _, _) }) ->
        Printf.ksprintf failwith "OBus_object.Interface.%s: The interface %S cannot register any new member" caller name

  let with_names typs = List.map (fun t -> (None, t)) typs

  let method_call iface member typ f =
    let cr = get_creation "method_call" iface
    and isig = OBus_type.isignature typ
    and osig = OBus_type.osignature typ in
    cr.members <- Method(member, with_names isig, with_names osig, []) :: cr.members;
    let handler pack connection message =
      let context = (connection, message) in
      try_bind
        (fun () -> OBus_type.cast_func typ ~context message.body (f (cr.unpack pack)))
        (OBus_method.return ~context (OBus_type.func_reply typ))
        (OBus_method.fail ~context)
    in
    cr.methods <- MethodMap.add (Some cr.name, member, isig) handler (MethodMap.add (None, member, isig) handler cr.methods)

  let signal iface member typ =
    let cr = get_creation "signal" iface in
    cr.members <- Signal(member, with_names (OBus_type.type_sequence typ), []) :: cr.members

  let emit iface member typ obj ?peer value =
    let interface, obj = match !iface with
      | Creation{ name = name; get = get }
      | Finished(get, { ui_introspect = (name, _, _) }) -> (name, get obj)
    and body = OBus_type.make_sequence typ value in
    match peer, obj.owner with
      | Some { OBus_peer.connection = connection; OBus_peer.name = destination }, _
      | _, Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
          OBus_signal.dyn_emit ~connection ?destination ~interface ~member ~path:obj.path body
      | None, None ->
          join (ConnectionSet.fold
                  (fun connection l -> OBus_signal.dyn_emit ~connection ~interface ~member ~path:obj.path body :: l)
                  (React.S.value obj.exports)
                  [])

  let property iface member typ reader writer mode =
    let cr = get_creation "property" iface
    and ty = OBus_type.type_single typ in
    cr.members <- Property(member, ty, mode, []) :: cr.members;
    cr.properties <- PropertyMap.add (cr.name, member) {
      up_reader = (match reader with
                     | None ->
                         None
                     | Some f ->
                         Some(fun pack -> f (cr.unpack pack) >|= OBus_type.make_single typ));
      up_writer = (match writer with
                     | None ->
                         None
                     | Some f ->
                         Some(fun pack x -> match OBus_type.opt_cast_single typ x with
                                | Some x -> f (cr.unpack pack) x
                                | None -> fail (Failure (sprintf "invalid type for property %S: '%s', should be '%s'"
                                                           member
                                                           (string_of_signature [type_of_single x])
                                                           (string_of_signature [ty])))));
    } cr.properties

  let property_r iface member typ reader = property iface member typ (Some reader) None Read
  let property_w iface member typ writer = property iface member typ None (Some writer) Write
  let property_rw iface member typ reader writer = property iface member typ (Some reader) (Some writer) Read_write
end

(* +-----------------------------------------------------------------+
   | Objects signature                                               |
   +-----------------------------------------------------------------+ *)

module type S = sig
  type obj with obus(basic)
  val make_interface : OBus_name.interface -> obj Interface.t
  val add_interface : obj -> obj Interface.t -> unit
  val remove_interface : obj -> obj Interface.t -> unit
  val remove_interface_by_name : obj -> OBus_name.interface -> unit
  val introspectable : obj Interface.t
  val properties : obj Interface.t
  val make : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : obj Interface.t list -> OBus_path.t -> t
  val make' : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : obj Interface.t list -> unit -> t
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

module Make(Object : Custom) : S with type obj = Object.obj =
struct
  exception Pack of Object.obj

  let unpack_object = function
    | Pack obj ->
        obj
    | _ ->
        (* This should never happen *)
        failwith "OBus_object.Make.unpack"

  (* +---------------------------------------------------------------+
     | Type and type combinator                                      |
     +---------------------------------------------------------------+ *)

  type obj = Object.obj

  let obus_obj = OBus_type.map_with_context obus_path
    (fun (connection, context) path ->
       match connection#get with
         | Crashed _ ->
             raise (OBus_type.Cast_failure("OBus_object.Make.obus_obj", "connection crashed"))
         | Running connection ->
             match ObjectMap.lookup path connection.exported_objects with
               | Some{ oo_object = Pack obj } ->
                   obj
               | Some{ oo_object = pack } ->
                   raise (OBus_type.Cast_failure("OBus_object.Make.obus_obj",
                                                 Printf.sprintf
                                                   "unexpected type for object with path %S"
                                                   (OBus_path.to_string path)))
               | None ->
                   raise (OBus_type.Cast_failure("OBus_object.Make.obus_obj",
                                                 Printf.sprintf
                                                   "cannot find object with path %S"
                                                   (OBus_path.to_string path))))
    (fun obj -> (Object.cast obj).path)

  (* +---------------------------------------------------------------+
     | Properties                                                    |
     +---------------------------------------------------------------+ *)

  let path obj = (Object.cast obj).path
  let owner obj = (Object.cast obj).owner
  let exports obj = (Object.cast obj).exports
  let introspect obj = List.map (fun ui -> ui.ui_introspect) (Object.cast obj).interfaces

  (* +---------------------------------------------------------------+
     | Export/remove                                                 |
     +---------------------------------------------------------------+ *)

  let handle_call packed_object packed_connection message =
    let info = Object.cast (unpack_object packed_object) in
    match message with
      | { typ = Method_call(path, interface, member) } -> begin
          match MethodMap.lookup (interface, member, type_of_sequence message.body) info.methods with
            | Some f -> f packed_object packed_connection message
            | None -> OBus_method.fail  (packed_connection, message) (unknown_method_exn message)
        end
      | _ ->
          invalid_arg "OBus_object.Make.handle_call"

  let export packed obj =
    let connection = unpack_connection packed in
    let info = Object.cast obj in
    if not (ConnectionSet.mem packed (React.S.value info.exports)) then begin
      connection.exported_objects <- ObjectMap.add info.path {
        oo_handle = handle_call;
        oo_object = Pack obj;
        oo_connection_closed = (fun packed -> info.set_exports (ConnectionSet.remove packed (React.S.value info.exports)));
      } connection.exported_objects;
      info.set_exports (ConnectionSet.add connection.packed (React.S.value info.exports))
    end

  let remove packed obj =
    let info = Object.cast obj in
    if ConnectionSet.mem packed (React.S.value info.exports) then begin
      info.set_exports (ConnectionSet.remove packed (React.S.value info.exports));
      match packed#get with
        | Crashed _ ->
           ()
        | Running connection ->
            connection.exported_objects <- ObjectMap.remove info.path connection.exported_objects
    end

  let destroy obj =
    let info = Object.cast obj in
    ConnectionSet.iter
      (fun packed -> match packed#get with
         | Crashed exn ->
             ()
         | Running connection ->
             connection.exported_objects <- ObjectMap.remove info.path connection.exported_objects)
      (React.S.value info.exports);
    info.set_exports ConnectionSet.empty

  let dynamic ~connection:packed ~prefix ~handler =
    let connection = unpack_connection packed in
    (* Remove any dynamic node declared with the same prefix: *)
    connection.dynamic_objects <- List.filter (fun dynobj -> dynobj.do_prefix <> prefix) connection.dynamic_objects;
    let create path =
      lwt obj = handler path in
      return {
        oo_handle = handle_call;
        oo_object = Pack obj;
        oo_connection_closed = ignore;
      }
    in
    connection.dynamic_objects <- { do_prefix = prefix; do_create = create } :: connection.dynamic_objects

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
          join (ConnectionSet.fold
                  (fun connection l -> OBus_signal.dyn_emit ~connection ~interface ~member ~path:obj.path body :: l)
                  (React.S.value obj.exports)
                  [])

  (* +---------------------------------------------------------------+
     | Interfaces                                                    |
     +---------------------------------------------------------------+ *)

  let make_interface name = Interface.make name unpack_object Object.cast

  let iface_equal { ui_introspect = (name1, _, _) } { ui_introspect = (name2, _, _) } =
    name1 = name2

  let add_interface obj iface =
    let obj = Object.cast obj and ui = Interface.untyped_interface iface in
    if List.exists (iface_equal ui) obj.interfaces then begin
      obj.interfaces <- ui :: List.filter (iface_equal ui) obj.interfaces;
      generate obj
    end else begin
      obj.interfaces <- ui :: obj.interfaces;
      obj.methods <- MethodMap.fold MethodMap.add ui.ui_methods obj.methods;
      obj.properties <- PropertyMap.fold PropertyMap.add ui.ui_properties obj.properties
    end

  let remove_interface_by_name obj name =
    let obj = Object.cast obj in
    if List.exists (fun { ui_introspect = (name', _, _) } -> name = name') obj.interfaces then begin
      obj.interfaces <- List.filter (fun { ui_introspect = (name', _, _) } -> name <> name') obj.interfaces;
      generate obj
    end

  let remove_interface obj iface =
    remove_interface_by_name obj (Interface.name iface)

  (* +---------------------------------------------------------------+
     | Common interfaces                                             |
     +---------------------------------------------------------------+ *)

  let introspectable = make_interface "org.freedesktop.DBus.Introspectable"

  let () =
    Interface.method_call introspectable "Introspect" <:obus_func< context -> OBus_introspect.document >>
      (fun obj (connection, message) ->
         let obj = Object.cast obj in
         return (List.map (fun ui -> ui.ui_introspect) obj.interfaces,
                 match connection#get with
                   | Crashed _ ->
                       []
                   | Running connection ->
                       children connection obj.path));
    Interface.close introspectable

  let properties = make_interface "org.freedesktop.DBus.Properties"

  let () =
    Interface.method_call properties "Get" <:obus_func< string -> string -> variant >>
      (fun obj interface member ->
         match PropertyMap.lookup (interface, member) (Object.cast obj).properties with
           | Some{ up_reader = Some f } ->
               f (Pack obj)
           | Some{ up_reader = None } ->
               fail (Failure (sprintf "property %S on interface %S is not readable" member interface))
           | None ->
               fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "Set" <:obus_func< string -> string -> variant -> unit >>
      (fun obj interface member x ->
         match PropertyMap.lookup (interface, member) (Object.cast obj).properties with
           | Some{ up_writer = Some f } ->
               f (Pack obj) x
           | Some{ up_writer = None } ->
               fail (Failure (sprintf "property %S on interface %S is not writable" member interface))
           | None ->
               fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "GetAll" <:obus_func< string -> (string, variant) dict >>
      (fun obj interface ->
         PropertyMap.fold
           (fun (interface', member) up acc ->
              if interface = interface' then
                match up.up_reader with
                  | Some f ->
                      lwt x = f (Pack obj) and l = acc in
                      return ((member, x) :: l)
                  | None -> acc
              else acc)
           (Object.cast obj).properties
           (return []));
    Interface.close properties

  (* +---------------------------------------------------------------+
     | Constructors                                                  |
     +---------------------------------------------------------------+ *)

  let make ?owner ?(common=true) ?(interfaces=[]) path =
    let interfaces = if common then introspectable :: properties :: interfaces else interfaces in
    let exports, set_exports = React.S.create ~eq:ConnectionSet.equal ConnectionSet.empty in
    let obj = {
      path = path;
      exports = exports;
      set_exports = set_exports;
      owner = owner;
      methods = MethodMap.empty;
      properties = PropertyMap.empty;
      interfaces = List.map Interface.untyped_interface interfaces;
    } in
    generate obj;
    let () =
      match owner with
        | None ->
            ()
        | Some peer ->
            ignore (lwt () = OBus_peer.wait_for_exit peer in
                    default_destroy obj;
                    return ())
    in
    obj

  let make' ?owner ?common ?interfaces () =
    let id1 , id2 = !object_unique_id in
    let id2 = id2 + 1 in
    if id2 < 0 then
      object_unique_id := (id1 + 1, 0)
    else
      object_unique_id := (id1, id2);
    make ?owner ?common ?interfaces ["ocaml"; Printf.sprintf "%d_%d" id1 id2]
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

let remove_by_path packed path =
  let connection = unpack_connection packed in
  connection.dynamic_objects <- List.filter (fun dynobj -> dynobj.do_prefix <> path) connection.dynamic_objects;
  match ObjectMap.lookup path connection.exported_objects with
    | Some obj ->
        connection.exported_objects <- ObjectMap.remove path connection.exported_objects;
        obj.oo_connection_closed connection.packed
    | None ->
        ()
