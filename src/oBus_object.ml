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

module Method_map = Map.Make
  (struct
     type t = OBus_name.interface option * OBus_name.member * signature
     let compare = Pervasives.compare
   end)

module Property_map = Map.Make
  (struct
     type t = OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

module Connection_set = Set.Make(OBus_connection)
module Interface_map = String_map
module Member_map = String_map

(* Type of a method call handler *)
type untyped_method = OBus_pack.t -> OBus_connection.t -> OBus_message.t -> unit Lwt.t

(* Type of a property handlers *)
type untyped_property = {
  up_signal : (OBus_pack.t -> OBus_value.single React.signal) option;
  up_setter : (OBus_pack.t -> OBus_value.single -> unit) option;
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

  ui_methods : untyped_method Method_map.t;
  (* For dispatching *)

  ui_properties : untyped_property Property_map.t;
  (* List for properties, for reading/writing properties *)
}

(* D-Bus object informations *)
type info = {
  mutable packed : OBus_pack.t;
  path : OBus_path.t;
  exports : Connection_set.t React.signal;
  set_exports : Connection_set.t -> unit;
  owner : OBus_peer.t option;
  mutable methods : untyped_method Method_map.t;
  mutable properties : applied_property Property_map.t;
  mutable interfaces : untyped_interface list;
  mutable monitored_properties : unit React.signal list;
  mutable changed : OBus_value.single Member_map.t Interface_map.t;
}

type t = info
    (* The doefault type for object is directly D-Bus informations *)

let apply_property up packed = {
  ap_signal = (match up.up_signal with
                 | Some f -> Some(f packed)
                 | None -> None);
  ap_setter = (match up.up_setter with
                 | Some f -> Some(f packed)
                 | None -> None);
}

let handle_change info interface member value =
  let empty = Interface_map.is_empty info.changed in
  info.changed <- (
    Interface_map.add interface
      (Member_map.add member value
         (try Interface_map.find interface info.changed with Not_found -> Member_map.empty))
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
               ignore begin
                 OBus_signal.emit
                   ~connection:(OBus_peer.connection peer)
                   ?destination:(OBus_peer.name peer)
                   ~path:info.path
                   ~interface:"org.freedesktop.Properties"
                   ~member:"OBusPropertiesChanged"
                 <:obus_type< (string, variant) dict >>
                   (Member_map.fold (fun name value acc -> (name, value) :: acc) properties [])
               end)
            changed;
          return ()
      | None ->
          Connection_set.iter
            (fun connection ->
               Interface_map.iter
                 (fun name properties ->
                    ignore begin
                      OBus_signal.emit
                        ~connection
                        ~path:info.path
                        ~interface:"org.freedesktop.Properties"
                        ~member:"OBusPropertiesChanged"
                      <:obus_type< (string, variant) dict >>
                        (Member_map.fold (fun name value acc -> (name, value) :: acc) properties [])
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
         Method_map.fold Method_map.add ui.ui_methods map)
      Method_map.empty info.interfaces
  );
  info.properties <- (
    List.fold_left
      (fun map ui ->
         Property_map.fold
           (fun name up map ->
              Property_map.add name (apply_property up info.packed) map)
           ui.ui_properties map)
      Property_map.empty info.interfaces
  );
  info.monitored_properties <- (
    Property_map.fold
      (fun (interface, member) ap acc ->
         match ap.ap_signal with
           | Some signal ->
               React.S.map (handle_change info interface member) signal :: acc
           | None ->
               acc)
      info.properties
      []
  )

let default_destroy info =
  Connection_set.iter
    (fun connection -> match connection#get with
       | Crashed exn ->
           ()
       | Running running ->
           running.running_static_objects <- Object_map.remove info.path running.running_static_objects)
    (React.S.value info.exports);
  info.set_exports Connection_set.empty

(* +-----------------------------------------------------------------+
   | Interface                                                       |
   +-----------------------------------------------------------------+ *)

module Interface =
struct
  type 'obj creation = {
    name : OBus_name.interface;
    unpack : OBus_pack.t -> 'obj;
    get : 'obj -> t;
    mutable members : OBus_introspect.member Member_map.t;
    mutable methods : untyped_method Method_map.t;
    mutable properties : untyped_property Property_map.t;
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
           members = Member_map.empty;
           methods = Method_map.empty;
           properties = Property_map.empty;
         })

  let untyped_interface iface = match !iface with
    | Creation cr ->
        let ui = {
          ui_introspect = (cr.name, List.rev (Member_map.fold (fun name definition acc -> definition :: acc) cr.members []), []);
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
    cr.members <- Member_map.add member (Method(member, with_names isig, with_names osig, [])) cr.members;
    let handler pack connection message =
      let context = (connection, message) in
      try_bind
        (fun () -> OBus_type.cast_func typ ~context message.body (f (cr.unpack pack)))
        (OBus_method.return ~context (OBus_type.func_reply typ))
        (OBus_method.fail ~context)
    in
    cr.methods <- Method_map.add (Some cr.name, member, isig) handler (Method_map.add (None, member, isig) handler cr.methods)

  let signal iface member typ =
    let cr = get_creation "signal" iface in
    cr.members <- Member_map.add member (Signal(member, with_names (OBus_type.type_sequence typ), [])) cr.members

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
          join (Connection_set.fold
                  (fun connection l -> OBus_signal.dyn_emit ~connection ~interface ~member ~path:obj.path body :: l)
                  (React.S.value obj.exports)
                  [])

  let property iface member typ signal setter mode =
    let cr = get_creation "property" iface
    and ty = OBus_type.type_single typ in
    cr.members <- Member_map.add member (Property(member, ty, mode, [])) cr.members;
    cr.properties <- Property_map.add (cr.name, member) {
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
                                                   Printf.sprintf
                                                     "unexpected type for object with path %S"
                                                     (OBus_path.to_string path)))
                 end
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

  let handle_call pack connection message =
    let info = Object.cast (Pack.unpack pack) in
    match message with
      | { typ = Method_call(path, interface, member) } -> begin
          match try Some(Method_map.find (interface, member, type_of_sequence message.body) info.methods) with Not_found -> None with
            | Some f -> f pack connection message
            | None -> OBus_method.fail  (connection, message) (unknown_method_exn message)
        end
      | _ ->
          invalid_arg "OBus_object.Make.handle_call"

  let export connection obj =
    let running = running_of_connection connection in
    let info = Object.cast obj in
    if not (Connection_set.mem connection (React.S.value info.exports)) then begin
      running.running_static_objects <- Object_map.add info.path {
        static_object_handle = handle_call;
        static_object_object = info.packed;
        static_object_connection_closed = (fun packed -> info.set_exports (Connection_set.remove packed (React.S.value info.exports)));
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

  let make_interface name = Interface.make name Pack.unpack Object.cast

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
         match try Some(Property_map.find (interface, member) (Object.cast obj).properties) with Not_found -> None with
           | Some{ ap_signal = Some s } ->
               return (React.S.value s)
           | Some{ ap_signal = None } ->
               fail (Failure (sprintf "property %S on interface %S is not readable" member interface))
           | None ->
               fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "Set" <:obus_func< string -> string -> variant -> unit >>
      (fun obj interface member x ->
         match try Some(Property_map.find (interface, member) (Object.cast obj).properties) with Not_found -> None with
           | Some{ ap_setter = Some f } ->
               f x;
               return ()
           | Some{ ap_setter = None } ->
               fail (Failure (sprintf "property %S on interface %S is not writable" member interface))
           | None ->
               fail (Failure (sprintf "no such property: %S on interface %S" member interface)));
    Interface.method_call properties "GetAll" <:obus_func< string -> (string, variant) dict >>
      (fun obj interface ->
         return (Property_map.fold
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
    let exports, set_exports = React.S.create ~eq:Connection_set.equal Connection_set.empty in
    let info = {
      packed = OBus_pack.dummy;
      path = path;
      exports = exports;
      set_exports = set_exports;
      owner = owner;
      methods = Method_map.empty;
      properties = Property_map.empty;
      interfaces = List.map Interface.untyped_interface interfaces;
      monitored_properties = [];
      changed = Interface_map.empty;
    } in
    let obj = f info in
    info.packed <- Pack.pack obj;
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

let remove_by_path connection path =
  let running = running_of_connection connection in
  running.running_dynamic_objects <- Object_map.remove path running.running_dynamic_objects;
  match try Some(Object_map.find path running.running_static_objects) with Not_found -> None with
    | Some static_object ->
        running.running_static_objects <- Object_map.remove path running.running_static_objects;
        static_object.static_object_connection_closed connection
    | None ->
        ()
