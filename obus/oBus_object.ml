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
open OBus_introspect
open OBus_value
open OBus_internals
open OBus_connection

type member_desc = OBus_internals.member_desc

class virtual interface = object
  method virtual obus_emit_signal : 'a 'b.
    OBus_name.interface -> OBus_name.member ->
    ([< 'a OBus_type.cl_sequence ] as 'b) -> ?peer:OBus_peer.t -> 'a -> unit Lwt.t
  method virtual obus_add_interface : OBus_name.interface -> member_desc list -> unit
end

let args = List.map (fun x -> (None, x))

let md_method name typ f =
  let isig = OBus_type.isignature typ in
  (Method(name, args isig, args (OBus_type.osignature typ), []),
   MI_method(name, isig,
             fun connection message ->
               ignore_result
                 (try_bind
                    (fun _ -> OBus_type.cast_func (OBus_type.abstract tunit typ) ~context:(Context(connection, message)) message.body f)
                    (send_reply connection message (OBus_type.func_reply typ))
                    (send_exn connection message))))

let md_signal name typ = (Signal(name, args (OBus_type.type_sequence typ), []), MI_signal)

let md_property_r name typ reader =
  let ty = OBus_type.type_single typ in
  (Property(name, ty, Read, []),
   MI_property(name,
               Some(fun _ -> perform
                      x <-- reader ();
                      return (OBus_type.make_single typ x)),
               None))

let md_property_w name typ writer =
  let ty = OBus_type.type_single typ in
  (Property(name, ty, Write, []),
   MI_property(name,
               None,
               Some(fun x -> match OBus_type.opt_cast_single typ x with
                      | Some x -> writer x
                      | None -> fail (OBus_error.Failed (sprintf "invalid type for property %S: %s, should be %s"
                                                           name
                                                           (string_of_signature [type_of_single x])
                                                           (string_of_signature [ty]))))))

let md_property_rw name typ reader writer =
  let ty = OBus_type.type_single typ in
  (Property(name, ty, Read_write, []),
   MI_property(name,
               Some(fun _ -> perform
                      x <-- reader ();
                      return (OBus_type.make_single typ x)),
               Some(fun x -> match OBus_type.opt_cast_single typ x with
                      | Some x -> writer x
                      | None -> fail (OBus_error.Failed (sprintf "invalid type for property %S: %s, should be %s"
                                                           name
                                                           (string_of_signature [type_of_single x])
                                                           (string_of_signature [ty]))))))

module Method_map = Util.Make_map(struct type t = OBus_name.interface option * OBus_name.member * tsequence end)
module Property_map = Util.Make_map(struct type t = OBus_name.interface * OBus_name.member end)

class t = object(self)
  val mutable exports = []
  val mutable interfaces = []
  val mutable methods = Method_map.empty
  val mutable properties = Property_map.empty

  inherit interface

  method obus_path = ["ocaml_object"; string_of_int (Oo.id self)]

  method obus_introspect connection =
    return (interfaces,
            match connection#get with
              | Crashed _ ->
                  []
              | Running connection ->
                  children connection (self#obus_path))

  method obus_get iface name =
    match Property_map.lookup (iface, name) properties with
      | Some(Some reader, _) ->
          reader ()
      | Some(None, _) ->
          fail (OBus_error.Failed (sprintf "property %S on interface %S is not readable" name iface))
      | None ->
          fail (OBus_error.Failed (sprintf "no such property: %S on interface %S" name iface))

  method obus_set iface name x =
    match Property_map.lookup (iface, name) properties with
      | Some(_, Some writer) ->
          writer x
      | Some(_, None) ->
          fail (OBus_error.Failed (sprintf "property %S on interface %S is not writable" name iface))
      | None ->
          fail (OBus_error.Failed (sprintf "no such property: %S on interface %S" name iface))

  method obus_get_all iface =
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

  method obus_handle_call connection message = match message with
    | { typ = Method_call(path, interface, member) } ->
        begin match Method_map.lookup (interface, member, type_of_sequence message.body) methods with
          | Some f -> f connection message
          | None -> ignore_result (send_exn connection message (unknown_method_exn message))
        end

    | _ ->
        invalid_arg "#obus_handle_call"

  method obus_emit_signal interface member typ ?peer x =
    let body = OBus_type.make_sequence typ x
    and path = self#obus_path in
    match peer with
      | Some { OBus_peer.connection = connection; OBus_peer.name = destination } ->
          demit_signal connection ?destination ~interface ~member ~path body
      | None ->
          Lwt_util.iter
            (fun connection ->
               demit_signal connection ~interface ~member ~path body)
            exports

  method obus_export (connection : OBus_connection.t) = match connection#get with
    | Crashed exn ->
        raise exn
    | Running connection ->
        connection.exported_objects <- Object_map.add (self#obus_path) (self :> dbus_object) connection.exported_objects;
        exports <- connection.packed :: exports

  method obus_remove connection = match List.mem connection exports with
    | true ->
        exports <- List.filter ((!=) connection) exports;
        begin match connection#get with
          | Crashed _ ->
              ()

          | Running connection ->
              let path = self#obus_path in
              begin match Object_map.lookup path connection.exported_objects with
                | Some obj' when (self :> dbus_object) = obj' ->
                    connection.exported_objects <- Object_map.remove path connection.exported_objects
                | _ ->
                    ()
              end
        end

    | false ->
        ()

  method obus_destroy =
    List.iter (fun c -> self#obus_remove c) exports

  method obus_connection_closed connection =
    exports <- List.filter ((!=) connection) exports

  initializer
    self#obus_add_interface "org.freedesktop.DBus.Introspectable"
      [md_method "Introspect" << OBus_connection.t -> OBus_introspect.document >>
         (fun _ connection -> self#obus_introspect connection)];

    self#obus_add_interface "org.freedesktop.DBus.Properties"
      [md_method "Get" << string -> string -> variant >>
         (fun _ iface name -> self#obus_get iface name);
       md_method "Set" << string -> string -> variant -> unit >>
         (fun _ iface name x -> self#obus_set iface name x);
       md_method "GetAll" << string -> {string, variant} list >>
         (fun _ iface -> self#obus_get_all iface)]
end

(*let tt = OBus_type.wrap_basic_ctx tpath
  (fun context path -> match context with
     | Context(connection, _) ->
         (* Note that this will never fail because:

            - a first successful search has been done has dispatching
            time, so the object exists

            - since dispatching, no lwt operation has been performed
            so the connection is still running *)
         get_by_path connection path
     | _ -> raise OBus_type.Cast_failure)
  (fun obj -> obj#obus_path)*)

class owned owner = object(self)
  inherit t as super

  method obus_emit_signal interface member typ ?peer x =
    super#obus_emit_signal interface member typ
      ~peer:(match peer with
               | Some peer -> peer
               | None -> owner) x

  initializer
    if owner.OBus_peer.name <> None then
      ignore (OBus_peer.wait_for_exit owner >>= fun _ -> self#obus_destroy; return ());
    self#obus_export (OBus_peer.connection owner)
end
