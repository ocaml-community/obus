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

module MethodMap = OBus_util.MakeMap(struct
                                       type t = OBus_name.interface option * OBus_name.member * tsequence
                                       let compare = Pervasives.compare
                                     end)
module PropertyMap = OBus_util.MakeMap(struct
                                         type t = OBus_name.interface * OBus_name.member
                                         let compare = Pervasives.compare
                                       end)

module ConnectionSet = Set.Make(OBus_connection)

type t = {
  path : OBus_path.t;
  exports : ConnectionSet.t React.signal;
  set_exports : ConnectionSet.t -> unit;
  owner : OBus_peer.t option;
}

let path obj = obj.path
let owner obj = obj.owner
let exports obj = obj.exports

let destroy obj =
  ConnectionSet.iter
    (fun connection -> match connection#get with
       | Crashed exn ->
           ()
       | Running connection ->
           connection.exported_objects <- ObjectMap.remove obj.path connection.exported_objects)
    (React.S.value obj.exports);
  obj.set_exports ConnectionSet.empty

let make ?owner path =
  let exports, set_exports = React.S.create ~eq:ConnectionSet.equal ConnectionSet.empty in
  let obj = {
    path = path;
    exports = exports;
    set_exports = set_exports;
    owner = owner;
  } in
  begin
    match owner with
      | None ->
          ()
      | Some peer ->
          ignore_result (lwt () = OBus_peer.wait_for_exit peer in
                         destroy obj;
                         return ())
  end;
  obj

let id = ref(-1)
let make' ?owner () = incr id; make ?owner ["ocaml"; string_of_int !id]

let remove_by_path connection path =
  match connection#get with
    | Crashed exn ->
        raise exn
    | Running connection ->
        connection.dynamic_objects <- List.filter (fun dynobj -> dynobj.do_prefix <> path) connection.dynamic_objects;
        match ObjectMap.lookup path connection.exported_objects with
          | Some obj ->
              connection.exported_objects <- ObjectMap.remove path connection.exported_objects;
              obj.oo_connection_closed connection.packed
          | None ->
              ()

module type Object = sig
  type obj
  val get : obj -> t
end

module Make(Object : Object) =
struct
  exception Pack of Object.obj

  type t = Object.obj

  let obus_t = OBus_type.map_with_context obus_path
    (fun context path ->
       let connection, message = OBus_connection.cast_context context in
       match connection#get with
         | Crashed _ ->
             raise (OBus_type.Cast_failure("OBus_object.Make.obus_t", "connection crashed"))
         | Running connection ->
             match ObjectMap.lookup path connection.exported_objects with
               | Some{ oo_object = Pack obj } ->
                   obj
               | _ ->
                   raise (OBus_type.Cast_failure("OBus_object.Make.obus_t",
                                                 Printf.sprintf
                                                   "cannot find object with path %S"
                                                   (OBus_path.to_string path))))
    (fun obj -> (Object.get obj).path)

  let methods = ref MethodMap.empty
  let properties = ref PropertyMap.empty
  let interfaces = ref []

  let handle_call pack connection message = match message, pack with
    | { typ = Method_call(path, interface, member) }, Pack obj ->
        begin match MethodMap.lookup (interface, member, type_of_sequence message.body) !methods with
          | Some f -> f obj connection message
          | None -> ignore_result (send_exn connection message (unknown_method_exn message))
        end

    | _ ->
        invalid_arg "OBus_object.Make.handle_call"

  let export packed obj =
    match packed#get with
      | Crashed exn ->
          raise exn
      | Running connection ->
          let o = Object.get obj in
          if not (ConnectionSet.mem packed (React.S.value o.exports)) then begin
            connection.exported_objects <- ObjectMap.add o.path {
              oo_handle = handle_call;
              oo_object = Pack obj;
              oo_connection_closed = (fun packed -> o.set_exports (ConnectionSet.remove packed (React.S.value o.exports)));
            } connection.exported_objects;
            o.set_exports (ConnectionSet.add connection.packed (React.S.value o.exports))
          end

  let remove packed obj =
    match packed#get with
      | Crashed exn ->
          raise exn
      | Running connection ->
          let o = Object.get obj in
          if ConnectionSet.mem packed (React.S.value o.exports) then begin
            connection.exported_objects <- ObjectMap.remove o.path connection.exported_objects;
            o.set_exports (ConnectionSet.remove connection.packed (React.S.value o.exports))
          end

  let destroy obj =
    destroy (Object.get obj)

  let dynamic ~connection ~prefix ~handler =
    match connection#get with
      | Crashed exn ->
          raise exn
      | Running connection ->
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

  let emit obj ~interface ~member typ ?peer x =
    let body = OBus_type.make_sequence typ x and obj = Object.get obj in
    match peer, obj.owner with
      | Some { OBus_peer.connection = connection; OBus_peer.name = destination }, _
      | _, Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
          dyn_emit_signal connection ?destination ~interface ~member ~path:obj.path body
      | None, None ->
          ConnectionSet.fold
            (fun connection w ->
               w <&> dyn_emit_signal connection ~interface ~member ~path:obj.path body)
            (React.S.value obj.exports)
            (return ())

  module Make_interface(Name : OBus_proxy.Interface_name) =
  struct
    let ol_interface = Name.name

    let members = ref []
    let () = interfaces := (Name.name, members) :: !interfaces

    let with_names typs = List.map (fun typ -> (None, typ)) typs

    let ol_method_call member typ f =
      let handler obj connection message =
        ignore_result
          (try_bind
             (fun _ -> OBus_type.cast_func typ ~context:(OBus_connection.make_context (connection, message)) message.body (f obj))
             (send_reply connection message (OBus_type.func_reply typ))
             (send_exn connection message))
      in
      let isig = OBus_type.isignature typ and osig = OBus_type.osignature typ in
      methods := MethodMap.add (Some Name.name, member, isig) handler
        (MethodMap.add (None, member, isig) handler !methods);
      members := Method(member, with_names isig, with_names osig, []) :: !members

    let ol_signal member typ =
      members := Signal(member, with_names (OBus_type.type_sequence typ), []) :: !members;
      (fun obj -> emit obj ~interface:Name.name ~member typ)

    let ol_property member typ reader writer mode =
      let ty = OBus_type.type_single typ in
      properties := PropertyMap.add (Name.name, member)
        ((match reader with
            | None ->
                None
            | Some f ->
                Some(fun obj -> f obj >|= OBus_type.make_single typ)),
         (match writer with
            | None ->
                None
            | Some f ->
                Some(fun obj x -> match OBus_type.opt_cast_single typ x with
                       | Some x -> f obj x
                       | None -> fail (Failure (sprintf "invalid type for property %S: %s, should be %s"
                                                  member
                                                  (string_of_signature [type_of_single x])
                                                  (string_of_signature [ty]))))))
        !properties;
      members := Property(member, ty, mode, []) :: !members

    let ol_property_r member typ reader = ol_property member typ (Some reader) None Read
    let ol_property_w member typ writer = ol_property member typ None (Some writer) Write
    let ol_property_rw member typ reader writer = ol_property member typ (Some reader) (Some writer) Read_write
  end

  include Make_interface(struct let name = "org.freedesktop.DBus.Introspectable" end)

  let introspect obj connection =
    return (List.map (fun (name, members) -> (name, !members, [])) !interfaces,
            match connection#get with
              | Crashed _ ->
                  []
              | Running connection ->
                  children connection (Object.get obj).path)

  OL_method Introspect : OBus_connection.t -> OBus_introspect.document

  include Make_interface(struct let name = "org.freedesktop.DBus.Properties" end)

  let get obj iface name =
    match PropertyMap.lookup (iface, name) !properties with
      | Some(Some reader, _) ->
          reader obj
      | Some(None, _) ->
          fail (Failure (sprintf "property %S on interface %S is not readable" name iface))
      | None ->
          fail (Failure (sprintf "no such property: %S on interface %S" name iface))

  let set obj iface name x =
    match PropertyMap.lookup (iface, name) !properties with
      | Some(_, Some writer) ->
          writer obj x
      | Some(_, None) ->
          fail (Failure (sprintf "property %S on interface %S is not writable" name iface))
      | None ->
          fail (Failure (sprintf "no such property: %S on interface %S" name iface))

  let get_all obj iface =
    PropertyMap.fold (fun (iface', member) (reader, writer) acc ->
                        if iface = iface' then
                          match reader with
                            | Some f ->
                                lwt x = f obj and l = acc in
                                return ((member, x) :: l)
                            | None -> acc
                        else acc) !properties (return [])

  OL_method Get : string -> string -> variant
  OL_method Set : string -> string -> variant -> unit
  OL_method GetAll : string -> (string, variant) dict
end
