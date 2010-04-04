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

module MethodMap = Map.Make
  (struct
     type t = OBus_name.interface option * OBus_name.member * signature
     let compare = Pervasives.compare
   end)

module PropertyMap = Map.Make
  (struct
     type t = OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

module ConnectionSet = Set.Make(OBus_connection)
module InterfaceMap = StringMap
module MemberMap = StringMap

(* Type of a method call handler *)
type untyped_method = packed_object -> OBus_connection.t -> OBus_message.t -> unit Lwt.t

(* Type of a property handlers *)
type untyped_property = {
  up_signal : (packed_object -> OBus_value.single React.signal) option;
  up_setter : (packed_object -> OBus_value.single -> unit) option;
}

(* Result of an untyped property applied to an object *)
type applied_property = {
  ap_signal : (OBus_value.single React.signal) option;
  ap_setter : (OBus_value.single -> unit) option;
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
  mutable packed : packed_object;
  path : OBus_path.t;
  exports : ConnectionSet.t React.signal;
  set_exports : ConnectionSet.t -> unit;
  owner : OBus_peer.t option;
  mutable methods : untyped_method MethodMap.t;
  mutable properties : applied_property PropertyMap.t;
  mutable interfaces : untyped_interface list;
  mutable monitored_properties : unit React.signal list;
  mutable changed : OBus_value.single MemberMap.t InterfaceMap.t;
}

let apply_property up packed = {
  ap_signal = (match up.up_signal with
                 | Some f -> Some(f packed)
                 | None -> None);
  ap_setter = (match up.up_setter with
                 | Some f -> Some(f packed)
                 | None -> None);
}

let handle_change info interface member value =
  let empty = InterfaceMap.is_empty info.changed in
  info.changed <- (
    InterfaceMap.add interface
      (MemberMap.add member value
         (try InterfaceMap.find interface info.changed with Not_found -> MemberMap.empty))
      info.changed
  );
  if empty then ignore begin
    (* Sleep a bit, so multiple changes are sent only one time. *)
    lwt () = pause () in
    let changed = info.changed in
    info.changed <- InterfaceMap.empty;
    match info.owner with
      | Some peer ->
          InterfaceMap.iter
            (fun name properties ->
               ignore begin
                 OBus_signal.emit
                   ~connection:(OBus_peer.connection peer)
                   ?destination:(OBus_peer.name peer)
                   ~path:info.path
                   ~interface:"org.freedesktop.Properties"
                   ~member:"OBusPropertiesChanged"
                 <:obus_type< (string, variant) dict >>
                   (MemberMap.fold (fun name value acc -> (name, value) :: acc) properties [])
               end)
            changed;
          return ()
      | None ->
          ConnectionSet.iter
            (fun connection ->
               InterfaceMap.iter
                 (fun name properties ->
                    ignore begin
                      OBus_signal.emit
                        ~connection
                        ~path:info.path
                        ~interface:"org.freedesktop.Properties"
                        ~member:"OBusPropertiesChanged"
                      <:obus_type< (string, variant) dict >>
                        (MemberMap.fold (fun name value acc -> (name, value) :: acc) properties [])
                    end)
                 changed)
            (React.S.value info.exports);
          return ()
  end

(* Generate the [methods] and [properties] fields from the
   [interfaces] field: *)
let generate info =
  (* Stop monitoring of previous values *)
  List.iter React.S.stop info.monitored_properties;
  info.methods <- (
    List.fold_left
      (fun map ui ->
         MethodMap.fold MethodMap.add ui.ui_methods map)
      MethodMap.empty info.interfaces
  );
  info.properties <- (
    List.fold_left
      (fun map ui ->
         PropertyMap.fold
           (fun name up map ->
              PropertyMap.add name (apply_property up info.packed) map)
           ui.ui_properties map)
      PropertyMap.empty info.interfaces
  );
  info.monitored_properties <- (
    PropertyMap.fold
      (fun (interface, member) ap acc ->
         match ap.ap_signal with
           | Some signal ->
               React.S.map (handle_change info interface member) signal :: acc
           | None ->
               acc)
      info.properties
      []
  )

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
    mutable members : OBus_introspect.member MemberMap.t;
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
           members = MemberMap.empty;
           methods = MethodMap.empty;
           properties = PropertyMap.empty;
         })

  let untyped_interface iface = match !iface with
    | Creation cr ->
        let ui = {
          ui_introspect = (cr.name, List.rev (MemberMap.fold (fun name definition acc -> definition :: acc) cr.members []), []);
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
    cr.members <- MemberMap.add member (Method(member, with_names isig, with_names osig, [])) cr.members;
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
    cr.members <- MemberMap.add member (Signal(member, with_names (OBus_type.type_sequence typ), [])) cr.members

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

  let property iface member typ signal setter mode =
    let cr = get_creation "property" iface
    and ty = OBus_type.type_single typ in
    cr.members <- MemberMap.add member (Property(member, ty, mode, [])) cr.members;
    cr.properties <- PropertyMap.add (cr.name, member) {
      up_signal = (match signal with
                     | None ->
                         None
                     | Some f ->
                         Some(fun pack -> React.S.map (OBus_type.make_single typ) (f (cr.unpack pack))));
      up_setter = (match setter with
                     | None ->
                         None
                     | Some f ->
                         Some(fun pack x -> match OBus_type.opt_cast_single typ x with
                                | Some x -> f (cr.unpack pack) x
                                | None -> failwith (sprintf "invalid type for property %S: '%s', should be '%s'"
                                                      member
                                                      (string_of_signature [type_of_single x])
                                                      (string_of_signature [ty]))));
    } cr.properties

  let property_r iface member typ signal = property iface member typ (Some signal) None Read
  let property_w iface member typ setter = property iface member typ None (Some setter) Write
  let property_rw iface member typ signal setter = property iface member typ (Some signal) (Some setter) Read_write
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
             match try Some(ObjectMap.find path connection.exported_objects) with Not_found -> None with
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
          match try Some(MethodMap.find (interface, member, type_of_sequence message.body) info.methods) with Not_found -> None with
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
        oo_object = info.packed;
        oo_connection_closed = (fun packed -> info.set_exports (ConnectionSet.remove packed (React.S.value info.exports)));
      } connection.exported_objects;
      info.set_exports (ConnectionSet.add packed (React.S.value info.exports))
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
    default_destroy (Object.cast obj)

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

  let interface_name { ui_introspect = (name, _, _) } = name

  let add_interface obj iface =
    let info = Object.cast obj and ui = Interface.untyped_interface iface in
    let name = interface_name ui in
    info.interfaces <- ui :: List.filter (fun ui' -> name <> interface_name ui') info.interfaces;
    generate info

  let remove_interface_by_name obj name =
    let info = Object.cast obj in
    info.interfaces <- List.filter (fun ui' -> name <> interface_name ui') info.interfaces;
    generate info

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
         match try Some(PropertyMap.find (interface, member) (Object.cast obj).properties) with Not_found -> None with
           | Some{ ap_signal = Some s } ->
               return (React.S.value s)
           | Some{ ap_signal = None } ->
               fail (Failure (sprintf "property %S on interface %S is not readable" member interface))
           | None ->
               fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "Set" <:obus_func< string -> string -> variant -> unit >>
      (fun obj interface member x ->
         match try Some(PropertyMap.find (interface, member) (Object.cast obj).properties) with Not_found -> None with
           | Some{ ap_setter = Some f } ->
               f x;
               return ()
           | Some{ ap_setter = None } ->
               fail (Failure (sprintf "property %S on interface %S is not writable" member interface))
           | None ->
               fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "GetAll" <:obus_func< string -> (string, variant) dict >>
      (fun obj interface ->
         return (PropertyMap.fold
                   (fun (interface', member) ap acc ->
                      if interface = interface' then
                        match ap.ap_signal with
                          | Some s -> (member, React.S.value s) :: acc
                          | None -> acc
                      else
                        acc)
                   (Object.cast obj).properties
                   []));
    Interface.signal properties "OBusPropertiesChanged" <:obus_type< string * (string, variant) dict >>;
    Interface.close properties

  (* +---------------------------------------------------------------+
     | Constructors                                                  |
     +---------------------------------------------------------------+ *)

  let make ?owner ?(common=true) ?(interfaces=[]) path f =
    let interfaces = if common then introspectable :: properties :: interfaces else interfaces in
    let exports, set_exports = React.S.create ~eq:ConnectionSet.equal ConnectionSet.empty in
    let info = {
      packed = Exit;
      path = path;
      exports = exports;
      set_exports = set_exports;
      owner = owner;
      methods = MethodMap.empty;
      properties = PropertyMap.empty;
      interfaces = List.map Interface.untyped_interface interfaces;
      monitored_properties = [];
      changed = InterfaceMap.empty;
    } in
    let obj = f info in
    info.packed <- Pack obj;
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
    make ?owner ?common ?interfaces ["ocaml"; Printf.sprintf "%d_%d" id1 id2] f
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
  match try Some(ObjectMap.find path connection.exported_objects) with Not_found -> None with
    | Some obj ->
        connection.exported_objects <- ObjectMap.remove path connection.exported_objects;
        obj.oo_connection_closed packed
    | None ->
        ()
