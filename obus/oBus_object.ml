(*
 * oBus_object.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open Lwt
open OBus_message
open OBus_interface
open OBus_value
open OBus_type
open OBus_internals
open OBus_connection

type member_info =
  | MI_method of OBus_name.member * tsequence * (OBus_connection.t -> method_call -> unit)
  | MI_signal
  | MI_property of OBus_name.member * (unit -> single Lwt.t) option * (single -> unit Lwt.t) option

type member_desc = OBus_interface.declaration * member_info

(* We use a module here to make [pa_obus] happy *)
module OBus_object =
struct
  class virtual interface = object
    method virtual obus_emit_signal : 'a 'b. OBus_name.Interface.t -> OBus_name.Member.t ->
      ([< 'a OBus_type.cl_sequence ] as 'b) -> 'a -> unit Lwt.t
    method virtual obus_add_interface : OBus_name.interface -> member_desc list -> unit
  end

  let args = List.map (fun x -> (None, x))

  let md_method name typ f =
    let isig = isignature typ in
    (Method(name, args isig, args (osignature typ), []),
     MI_method(name, isig,
               fun connection message ->
                 ignore_result
                   (try_bind
                      (fun _ -> cast_func (tunit --> typ) ~context:(Context(connection, (message :> OBus_message.any))) message.body f)
                      (send_reply connection message (func_reply typ))
                      (send_exn connection message))))

  let md_signal name typ = (Signal(name, args (type_sequence typ), []), MI_signal)

  let md_property_r name typ reader =
    let ty = type_single typ in
    (Property(name, ty, Read, []),
     MI_property(name,
                 Some(fun _ -> perform
                        x <-- reader ();
                        return (make_single typ x)),
                 None))

  let md_property_w name typ writer =
    let ty = type_single typ in
    (Property(name, ty, Read, []),
     MI_property(name,
                 None,
                 Some(fun x -> match opt_cast_single typ x with
                        | Some x -> writer x
                        | None -> fail (OBus_error.Failed (sprintf "invalid type for property %S: %s, should be %s"
                                                             name
                                                             (string_of_signature [type_of_single x])
                                                             (string_of_signature [ty]))))))

  let md_property_rw name typ reader writer =
    let ty = type_single typ in
    (Property(name, ty, Read, []),
     MI_property(name,
                 Some(fun _ -> perform
                        x <-- reader ();
                        return (make_single typ x)),
                 Some(fun x -> match opt_cast_single typ x with
                        | Some x -> writer x
                        | None -> fail (OBus_error.Failed (sprintf "invalid type for property %S: %s, should be %s"
                                                             name
                                                             (string_of_signature [type_of_single x])
                                                             (string_of_signature [ty]))))))
end
include OBus_object

OBUS_class introspectable "org.freedesktop.DBus.Introspectable" = object
  OBUS_method Introspect : OBus_introspect.document
end

OBUS_class properties "org.freedesktop.DBus.Properties" = object
  OBUS_method Get : string -> string -> variant;
  OBUS_method Set : string -> string -> variant -> unit;
  OBUS_method GetAll : string -> {string, variant} list
end

module Method_map = My_map(struct type t = OBus_name.interface option * OBus_name.member * tsequence end)
module Property_map = My_map(struct type t = OBus_name.interface * OBus_name.member end)

class t = object(self)
  val mutable exports = []
  val mutable interfaces = []
  val mutable methods = Method_map.empty
  val mutable properties = Property_map.empty

  inherit introspectable
  inherit properties

  method obus_path = ["ocaml_object"; string_of_int (Oo.id self)]

  method introspect = return (interfaces, [])

  method get iface name =
    match Property_map.lookup (iface, name) properties with
      | Some(Some reader, _) ->
          reader ()
      | Some(None, _) ->
          fail (OBus_error.Failed (sprintf "property %S on interface %S is not readable" name iface))
      | None ->
          fail (OBus_error.Failed (sprintf "no such property: %S on interface %S" name iface))

  method set iface name x =
    match Property_map.lookup (iface, name) properties with
      | Some(_, Some writer) ->
          writer x
      | Some(_, None) ->
          fail (OBus_error.Failed (sprintf "property %S on interface %S is not writable" name iface))
      | None ->
          fail (OBus_error.Failed (sprintf "no such property: %S on interface %S" name iface))

  method getAll iface =
    Property_map.fold (fun (iface', member) (reader, writer) acc ->
                         if iface = iface' then
                           match reader with
                             | Some f ->
                                 (perform
                                    x <-- f ();
                                    l <-- acc;
                                    return ((member, x) :: l))
                             | None -> acc
                         else acc) properties (return [])

  method obus_add_interface iface descs =
    interfaces <- (iface, List.map fst descs, []) :: interfaces;
    methods <- List.fold_left (fun acc (_, info) -> match info with
                                 | MI_method(member, ityp, handler) ->
                                     (* Add the version with and
                                        without interface field *)
                                     Method_map.add (None, member, ityp) handler
                                       (Method_map.add (Some iface, member, ityp) handler acc)
                                 | _ -> acc) methods descs;
    properties <- List.fold_left (fun acc (_, info) -> match info with
                                    | MI_property(member, reader, writer) ->
                                        Property_map.add (iface, member) (reader, writer) acc
                                    | _ -> acc) properties descs

  method obus_handle_call connection message =
    let `Method_call(path, interface, member) = message.typ in
    match Method_map.lookup (interface, member, type_of_sequence message.body) methods with
      | Some f -> f connection message
      | None -> ignore_result (send_exn connection message (unknown_method_exn message))

  method obus_emit_signal interface member typ x =
    let body = make_sequence typ x
    and path = self#obus_path in
    Lwt_util.iter
      (fun connection ->
         demit_signal connection ~interface ~member ~path body)
      exports

  method obus_export connection =
    with_running connection
      (fun running ->
         running.exported_objects <- Object_map.add (self#obus_path) (self :> dbus_object) running.exported_objects;
         exports <- connection :: exports)

  method obus_remove connection = match List.memq connection exports with
    | true -> begin
        exports <- List.filter ((!=) connection) exports;
        with_running connection
          (fun running ->
             let path = self#obus_path in
             match Object_map.lookup path running.exported_objects with
               | Some obj' when (self :> dbus_object) = obj' ->
                   running.exported_objects <- Object_map.remove path running.exported_objects
               | _ -> ())
      end
    | false -> ()
end
