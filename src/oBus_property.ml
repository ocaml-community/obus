(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(property)"

open Lwt.Infix
open Lwt_react
open OBus_interfaces.Org_freedesktop_DBus_Properties

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module String_map = Map.Make(String)

type map = (OBus_context.t * OBus_value.V.single) String_map.t

type monitor = OBus_proxy.t -> OBus_name.interface -> Lwt_switch.t -> map signal Lwt.t

type ('a, 'access) t = {
  p_interface : OBus_name.interface;
  (* The interface of the property. *)

  p_member : OBus_name.member;
  (* The name of the property. *)

  p_proxy : OBus_proxy.t;
  (* The object owning the property. *)

  p_monitor : monitor;
  (* Monitor for this property. *)

  p_cast : OBus_context.t -> OBus_value.V.single -> 'a;
  p_make : 'a -> OBus_value.V.single;
}

type 'a r = ('a, [ `readable ]) t
type 'a w = ('a, [ `writable ]) t
type 'a rw = ('a, [ `readable | `writable ]) t

type group = {
  g_interface : OBus_name.interface;
  (* The interface of the group *)

  g_proxy : OBus_proxy.t;
  (* The object owning the group of properties *)

  g_monitor : monitor;
  (* Monitor for this group. *)
}

module Group_map = Map.Make
  (struct
     type t = OBus_name.bus * OBus_path.t * OBus_name.interface
         (* Groups are indexed by:
            - name of the owner of the property
            - path of the object owning the property
            - interfaec of the property *)
     let compare = Pervasives.compare
   end)

(* Type of a cache for a group *)
type cache = {
  mutable c_count : int;
  (* Numbers of monitored properties using this group. *)

  c_map : map signal;
  (* The signal holding the current state of properties. *)

  c_switch : Lwt_switch.t;
  (* Switch for the signal used to monitor the group. *)
}

type info = {
  mutable cache : cache Lwt.t Group_map.t;
  (* Cache of all monitored properties. *)
}

(* +-----------------------------------------------------------------+
   | Default monitor                                                 |
   +-----------------------------------------------------------------+ *)

let update_map context dict map =
  List.fold_left (fun map (name, value) -> String_map.add name (context, value) map) map dict

let map_of_list context dict =
  update_map context dict String_map.empty

let get_all_no_cache proxy interface =
  OBus_method.call_with_context m_GetAll proxy interface

let default_monitor proxy interface switch =
  let%lwt event =
    OBus_signal.connect ~switch
      (OBus_signal.with_filters
         (OBus_match.make_arguments [(0, OBus_match.AF_string interface)])
         (OBus_signal.with_context
            (OBus_signal.make s_PropertiesChanged proxy)))
  and context, dict = get_all_no_cache proxy interface in
  Lwt.return (S.map snd
                (S.fold_s ~eq:(fun (_, a) (_, b) -> String_map.equal (=) a b)
                   (fun (_, map) (sig_context, (interface, updates, invalidates)) ->
                      if invalidates = [] then
                        Lwt.return (sig_context, update_map sig_context updates map)
                      else
                        let%lwt context, dict = get_all_no_cache proxy interface in
                        Lwt.return (sig_context, map_of_list context dict))
                   (context, map_of_list context dict)
                   event))

(* +-----------------------------------------------------------------+
   | Property creation                                               |
   +-----------------------------------------------------------------+ *)

let make ?(monitor=default_monitor) desc proxy = {
  p_interface = OBus_member.Property.interface desc;
  p_member = OBus_member.Property.member desc;
  p_proxy = proxy;
  p_monitor = monitor;
  p_cast = (fun context value -> OBus_value.C.cast_single (OBus_member.Property.typ desc) value);
  p_make = (OBus_value.C.make_single (OBus_member.Property.typ desc));
}

let group ?(monitor=default_monitor) proxy interface = {
  g_proxy = proxy;
  g_interface = interface;
  g_monitor = monitor;
}

(* +-----------------------------------------------------------------+
   | Transformations                                                 |
   +-----------------------------------------------------------------+ *)

let map_rw f g property = {
  property with
    p_cast = (fun context x -> f (property.p_cast context x));
    p_make = (fun x -> property.p_make (g x));
}

let map_rw_with_context f g property = {
  property with
    p_cast = (fun context x -> f context (property.p_cast context x));
    p_make = (fun x -> property.p_make (g x));
}

let map_r f property = {
  property with
    p_cast = (fun context x -> f (property.p_cast context x));
    p_make = (fun x -> assert false);
}

let map_r_with_context f property = {
  property with
    p_cast = (fun context x -> f context (property.p_cast context x));
    p_make = (fun x -> assert false);
}

let map_w g property = {
  property with
    p_cast = (fun context x -> assert false);
    p_make = (fun x -> property.p_make (g x));
}

(* +-----------------------------------------------------------------+
   | Operations on maps                                              |
   +-----------------------------------------------------------------+ *)

let find property map =
  let context, value = String_map.find property.p_member map in
  property.p_cast context value

let find_with_context property map =
  let context, value = String_map.find property.p_member map in
  (context, property.p_cast context value)

let find_value name map =
  let context, value = String_map.find name map in
  value

let find_value_with_context name map =
  String_map.find name map

let print_map pp map =
  let open Format in
  pp_open_box pp 2;
  pp_print_string pp "{";
  pp_print_cut pp ();
  pp_open_hvbox pp 0;
  String_map.iter
    (fun name (context, value) ->
       pp_open_box pp 0;
       pp_print_string pp name;
       pp_print_space pp ();
       pp_print_string pp "=";
       pp_print_space pp ();
       OBus_value.V.print_single pp value;
       pp_print_string pp ";";
       pp_close_box pp ();
       pp_print_cut pp ())
    map;
  pp_close_box pp ();
  pp_print_cut pp ();
  pp_print_string pp "}";
  pp_close_box pp ()

let string_of_map map =
  let open Format in
  let buf = Buffer.create 42 in
  let pp = formatter_of_buffer buf in
  pp_set_margin pp max_int;
  print_map pp map;
  pp_print_flush pp ();
  Buffer.contents buf

(* +-----------------------------------------------------------------+
   | Properties reading/writing                                      |
   +-----------------------------------------------------------------+ *)

let key = OBus_connection.new_key ()

let get_with_context prop =
  match OBus_connection.get (OBus_proxy.connection prop.p_proxy) key with
    | Some info -> begin
        match
          try
            Some(Group_map.find (OBus_proxy.name prop.p_proxy,
                                 OBus_proxy.path prop.p_proxy,
                                 prop.p_interface) info.cache)
          with Not_found ->
            None
        with
          | Some cache_thread ->
              let%lwt cache = cache_thread in
              Lwt.return (find_with_context prop (S.value cache.c_map))
          | None ->
              let%lwt context, value = OBus_method.call_with_context m_Get prop.p_proxy (prop.p_interface, prop.p_member) in
              Lwt.return (context, prop.p_cast context value)
      end
    | None ->
        let%lwt context, value = OBus_method.call_with_context m_Get prop.p_proxy (prop.p_interface, prop.p_member) in
        Lwt.return (context, prop.p_cast context value)

let get prop =
  get_with_context prop >|= snd

let set prop value =
  OBus_method.call m_Set prop.p_proxy (prop.p_interface, prop.p_member, prop.p_make value)

let get_group group =
  match OBus_connection.get (OBus_proxy.connection group.g_proxy) key with
    | Some info -> begin
        match
          try
            Some(Group_map.find (OBus_proxy.name group.g_proxy,
                                 OBus_proxy.path group.g_proxy,
                                 group.g_interface) info.cache)
          with Not_found ->
            None
        with
          | Some cache_thread ->
              let%lwt cache = cache_thread in
              Lwt.return (S.value cache.c_map)
          | None ->
              let%lwt context, dict = get_all_no_cache group.g_proxy group.g_interface in
              Lwt.return (map_of_list context dict)
      end
    | None ->
        let%lwt context, dict = get_all_no_cache group.g_proxy group.g_interface in
        Lwt.return (map_of_list context dict)

(* +-----------------------------------------------------------------+
   | Monitoring                                                      |
   +-----------------------------------------------------------------+ *)

let finalise disable _ =
  ignore (Lazy.force disable)

let monitor_group ?switch group =
  Lwt_switch.check switch;
  let cache_key = (OBus_proxy.name group.g_proxy, OBus_proxy.path group.g_proxy, group.g_interface) in
  let info =
    match OBus_connection.get (OBus_proxy.connection group.g_proxy) key with
      | Some info ->
          info
      | None ->
          let info = { cache = Group_map.empty } in
          OBus_connection.set (OBus_proxy.connection group.g_proxy) key (Some info);
          info
  in
  let%lwt cache =
    match
      try
        Some(Group_map.find cache_key info.cache)
      with Not_found ->
        None
    with
      | Some cache_thread ->
          cache_thread
      | None ->
          let waiter, wakener = Lwt.wait () in
          info.cache <- Group_map.add cache_key waiter info.cache;
          let switch = Lwt_switch.create () in
          try%lwt
            let%lwt signal = group.g_monitor group.g_proxy group.g_interface switch in
            let cache = {
              c_count = 0;
              c_map = signal;
              c_switch = switch;
            } in
            Lwt.wakeup wakener cache;
            Lwt.return cache
          with exn ->
            info.cache <- Group_map.remove cache_key info.cache;
            Lwt.wakeup_exn wakener exn;
            let%lwt () = Lwt_switch.turn_off switch in
            Lwt.fail exn
  in

  cache.c_count <- cache.c_count + 1;

  let disable = lazy(
    try%lwt
      cache.c_count <- cache.c_count - 1;
      if cache.c_count = 0 then begin
        info.cache <- Group_map.remove cache_key info.cache;
        Lwt_switch.turn_off cache.c_switch
      end else
        Lwt.return ()
    with exn ->
      let%lwt () =
        Lwt_log.warning_f
          ~section
          ~exn
          "failed to disable monitoring of properties for interface %S on object %S from %S"
          group.g_interface
          (OBus_path.to_string (OBus_proxy.path group.g_proxy))
          (OBus_proxy.name group.g_proxy)
      in
      Lwt.fail exn
  ) in

  let signal = S.with_finaliser (finalise disable) cache.c_map in

  let%lwt () =
    Lwt_switch.add_hook_or_exec
      switch
      (fun () ->
         S.stop signal;
         Lazy.force disable)
  in

  Lwt.return signal

let monitor ?switch prop =
  let%lwt signal = monitor_group ?switch { g_interface = prop.p_interface;
                                       g_proxy = prop.p_proxy;
                                       g_monitor = prop.p_monitor } in
  Lwt.return (S.map (find prop) signal)
