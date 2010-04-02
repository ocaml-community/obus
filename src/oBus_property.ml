(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(property)"

open Lwt
open OBus_private
open OBus_pervasives

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type 'a access = Readable | Writable | Readable_writable

let readable = Readable
let writable = Writable
let readable_writable = Readable_writable

type ('a, 'access) t = {
  cast : OBus_type.context -> OBus_value.single -> 'a;
  make : 'a -> OBus_value.single;
  member : OBus_name.member;
  changed : OBus_name.member option;
  core : OBus_private.property;
  (* The ``core'' property. It is used by the connection *)
}

type 'a r = ('a, [ `readable ]) t
type 'a w = ('a, [ `writable ]) t
type 'a rw = ('a, [ `readable | `writable ]) t

(* +-----------------------------------------------------------------+
   | Property reading/writing                                        |
   +-----------------------------------------------------------------+ *)


let interface = "org.freedesktop.DBus.Properties"

let get property =
  let core = property.core in
  match core.prop_state with
    | Prop_simple ->
        lwt variant, context =
          OBus_method.call
            ~connection:core.prop_connection
            ?destination:core.prop_owner
            ~path:core.prop_path
            ~interface
            ~member:"Get"
            <:obus_func< string -> string -> variant * context >>
            core.prop_interface
            property.member
        in
        return (property.cast context variant)
    | Prop_monitor(properties, stop) ->
        lwt map, context = properties >|= React.S.value in
        return (property.cast context (StringMap.find property.member map))

let set property value =
  let core = property.core in
  OBus_method.call
    ~connection:core.prop_connection
    ?destination:core.prop_owner
    ~path:core.prop_path
    ~interface
    ~member:"Set"
    <:obus_func< string -> string -> variant -> unit >>
    core.prop_interface
    property.member
    (property.make value)

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let get_all core =
  lwt variants, context =
    OBus_method.call
      ~connection:core.prop_connection
      ?destination:core.prop_owner
      ~path:core.prop_path
      ~interface
      ~member:"GetAll"
      <:obus_func< string -> (string, variant) dict * context >>
      core.prop_interface
  in
  return (List.fold_left
            (fun acc (name, variant) -> StringMap.add name variant acc)
            StringMap.empty
            variants,
          context)

let rec contents property =
  match property.changed with
    | None ->
        fail (Failure "OBus_property.contents: this property can not be used here")
    | Some changed ->
        let core = property.core in
        match core.prop_state with
          | Prop_monitor(properties, stop) ->
              lwt signal = properties in
              return (React.S.map
                        (fun (properties, context) ->
                           property.cast context (StringMap.find property.member properties))
                        signal)
          | Prop_simple ->
              let signal =
                OBus_signal.raw_connect
                  ~connection:core.prop_connection
                  ?sender:core.prop_owner
                  ~path:core.prop_path
                  ~interface:core.prop_interface
                  ~member:changed
                  ()
              in
              let waiter, wakener = wait () in
              core.prop_state <- Prop_monitor(waiter, (fun () -> OBus_signal.disconnect signal));
              lwt initial = get_all core in
              wakeup wakener
                (Lwt_signal.fold_s ~eq:(==)
                   (fun (properties, context) message ->
                      let context = (core.prop_connection, message) in
                      match OBus_type.opt_cast_sequence ~context <:obus_type< (string, variant) dict >> (OBus_message.body message) with
                        | Some dict ->
                            (* If the signal contains update
                               informations, then use them
                               directly: *)
                            return (List.fold_left
                                      (fun map (key, value) -> StringMap.add key value map)
                                      properties dict,
                                    context)
                        | None ->
                            (* Otherwise retreive all properties: *)
                            get_all core)
                   initial
                   (OBus_signal.event signal));
              contents property

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let cleanup property =
  let core = property.core in
  let connection = unpack_connection core.prop_connection in
  core.prop_ref_count <- core.prop_ref_count - 1;
  if core.prop_ref_count = 0 then begin
    connection.properties <- PropertyMap.remove (core.prop_owner, core.prop_path, core.prop_interface) connection.properties;
    match core.prop_state with
      | Prop_simple ->
          ()
      | Prop_monitor(properties, stop) ->
          stop ()
  end

let make_backend ~connection ?owner ~path ~interface ~member ~access ?changed ~cast ~make () =
  let connection = unpack_connection connection in
  let core =
    match try Some(PropertyMap.find (owner, path, interface) connection.properties) with Not_found -> None with
      | Some core ->
          core.prop_ref_count <- core.prop_ref_count + 1;
          core
      | None ->
          let core = {
            prop_ref_count = 1;
            prop_state = Prop_simple;
            prop_connection = connection.packed;
            prop_owner = owner;
            prop_path = path;
            prop_interface = interface;
          } in
          connection.properties <- PropertyMap.add (owner, path, interface) core connection.properties;
          core
  in
  let property = {
    cast = cast;
    make = make;
    member = member;
    changed = changed;
    core = core;
  } in
  Gc.finalise cleanup property;
  property

let make ~connection ?owner ~path ~interface ~member ~access ?changed typ =
  make_backend ~connection ?owner ~path ~interface ~member ~access ?changed
    ~cast:(fun context value -> OBus_type.cast_single typ ~context value)
    ~make:(fun value -> OBus_type.make_single typ value)
    ()

let dyn_make ~connection ?owner ~path ~interface ~member ~access ?changed () =
  make_backend ~connection ?owner ~path ~interface ~member ~access ?changed
    ~cast:(fun context value -> value)
    ~make:(fun value -> value)
    ()

(* +-----------------------------------------------------------------+
   | Reading all properties                                          |
   +-----------------------------------------------------------------+ *)

let get_all ~connection ?owner ~path ~interface () =
  let connection = unpack_connection connection in
  match try Some(PropertyMap.find (owner, path, interface) connection.properties) with Not_found -> None with
    | None | Some{ prop_state = Prop_simple } ->
        lwt dict =
          OBus_method.call
            ~connection:connection.packed
            ?destination:owner
            ~path
            ~interface:"org.freedesktop.DBus.Properties"
            ~member:"GetAll"
            <:obus_func< string -> (string, variant) dict >>
            interface
        in
        return (List.fold_left
                  (fun map (key, value) -> StringMap.add key value map)
                  StringMap.empty
                  dict)
    | Some{ prop_state = Prop_monitor(properties, stop) } ->
        properties >|= (fun s -> fst (React.S.value s))
