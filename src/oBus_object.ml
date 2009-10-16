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
open OBus_type.Perv

module MethodMap = OBus_util.MakeMap(struct
                                       type t = OBus_name.interface option * OBus_name.member * tsequence
                                       let compare = Pervasives.compare
                                     end)
module PropertyMap = OBus_util.MakeMap(struct
                                         type t = OBus_name.interface * OBus_name.member
                                         let compare = Pervasives.compare
                                       end)

type t = {
  path : OBus_path.t;
  mutable exports : OBus_connection.t list;
}

let path obj = obj.path
let make path = { path = path; exports = [] }

let id = ref(-1)
let make' () = incr id; make ["ocaml"; string_of_int !id]

module type Object = sig
  type obj
  val get : obj -> t
end

module Make(Object : Object) =
struct
  exception Pack of Object.obj

  let obus_t = OBus_type.map_with_context OBus_type.Perv.obus_path
    (fun context path -> match context with
       | Context(connection, message) -> begin
           match connection#get with
             | Crashed _ ->
                 raise OBus_type.Cast_failure
             | Running connection ->
                 match ObjectMap.lookup path connection.exported_objects with
                   | Some{ oo_object = Pack obj } ->
                       obj
                   | _ ->
                       raise OBus_type.Cast_failure
         end
       | _ ->
           raise OBus_type.Cast_failure)
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

  let export connection obj =
    match connection#get with
      | Crashed exn ->
          raise exn
      | Running connection ->
          let o = Object.get obj in
          connection.exported_objects <- ObjectMap.add o.path { oo_handle = handle_call; oo_object = Pack obj }
            connection.exported_objects;
          o.exports <- connection.packed :: o.exports

  let emit obj ~interface ~member typ ?peer x =
    let body = OBus_type.make_sequence typ x and obj = Object.get obj in
    match peer with
      | Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
          dyn_emit_signal connection ?destination ~interface ~member ~path:obj.path body
      | None ->
          Lwt_util.iter
            (fun connection ->
               dyn_emit_signal connection ~interface ~member ~path:obj.path body)
            obj.exports

  module MakeInterface(Name : OBus_interface.Name) =
  struct
    let ol_interface = Name.name

    let members = ref []
    let () = interfaces := (Name.name, members) :: !interfaces

    let with_names typs = List.map (fun typ -> (None, typ)) typs

    let ol_method_call member typ f =
      let handler obj connection message =
        ignore_result
          (try_bind
             (fun _ -> OBus_type.cast_func typ ~context:(Context(connection, message)) message.body (f obj))
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

  include MakeInterface(struct let name = "org.freedesktop.DBus.Introspectable" end)

  let introspect obj connection =
    return (List.map (fun (name, members) -> (name, !members, [])) !interfaces,
            match connection#get with
              | Crashed _ ->
                  []
              | Running connection ->
                  children connection (Object.get obj).path)

  OL_method Introspect : OBus_connection.t -> OBus_introspect.document

  include MakeInterface(struct let name = "org.freedesktop.DBus.Properties" end)

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
