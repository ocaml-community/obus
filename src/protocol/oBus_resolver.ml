(*
 * oBus_resolver.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(resolver)"

open Lwt_react

module String_map = Map.Make(String)

(* We keep track on each connection of the last [cache_size] peers
   that have already exited: *)
let cache_size  = 100

type resolver = {
  mutable count : int;
  (* Number of instances of this resolver. The resolver is
     automatically disabled when this number reach 0. *)

  owner : OBus_name.bus signal;
  (* The owner of the name that is being monitored. *)

  set_owner : OBus_name.bus -> unit;
  (* Sets the owner. *)
}

(* Informations stored in connections *)
and info = {
  mutable resolvers : (resolver * Lwt_switch.t) Lwt.t String_map.t;
  (* Mapping from names to active resolvers. The maps hold thread
     instead of resolver directly to avoid the following problem:

     1 - a resolver for a certain name is being created,
     2 - the creation yields,
     3 - another resolver for the same name is requested before the
         creation of the previous one terminates,
     4 - the second to register in this map wwill erase the first one.
  *)

  mutable exited : OBus_name.bus array;
  (* Array holding the last [cache_size] peers that have already
     exited *)

  mutable exited_index : int;
  (* Position where to store the next exited peers in [exited]. *)
}

let finalise remove _ =
  ignore (Lazy.force remove)

let has_exited peer_name info =
  let rec loop index =
    if index = cache_size then
      false
    else if info.exited.(index) = peer_name then
      true
    else
      loop (index + 1)
  in
  loop 0

let key = OBus_connection.new_key ()

let get_name_owner connection name =
  try%lwt
    OBus_connection.method_call
      ~connection
      ~destination:OBus_protocol.bus_name
      ~path:OBus_protocol.bus_path
      ~interface:OBus_protocol.bus_interface
      ~member:"GetNameOwner"
      ~i_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      ~o_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      name
  with exn when OBus_error.name exn = "org.freedesktop.DBus.Error.NameHasNoOwner" ->
    Lwt.return ""

(* Handle NameOwnerChanged events *)
let update_mapping info message =
  let open OBus_message in
  let open OBus_value in
  match message with
    | { sender = "org.freedesktop.DBus";
        typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameOwnerChanged");
        body = [V.Basic(V.String name); V.Basic(V.String old_owner); V.Basic(V.String new_owner)] } ->

        if OBus_name.is_unique name && new_owner = "" && not (has_exited name info) then begin
          (* Remember that the peer has exited: *)
          info.exited.(info.exited_index) <- name;
          info.exited_index <- (info.exited_index + 1) mod cache_size
        end;

        begin
          match try Lwt.state (String_map.find name info.resolvers) with Not_found -> Sleep with
            | Return(resolver, switch) ->
                resolver.set_owner new_owner
            | Fail _ | Sleep ->
                (* Discards events arriving before GetNameOwner has returned *)
                ()
        end;

        Some message
    | _ ->
        Some message

let make ?switch connection name =
  Lwt_switch.check switch;
  OBus_string.assert_validate OBus_name.validate_bus name;
  let info =
    match OBus_connection.get connection key with
      | Some info ->
          info
      | None ->
          let info = {
            resolvers = String_map.empty;
            exited = Array.make cache_size "";
            exited_index = 0;
          } in
          OBus_connection.set connection key (Some info);
          let _ = Lwt_sequence.add_l (update_mapping info) (OBus_connection.incoming_filters connection) in
          info
  in

  (* If [name] is a unique name and the peer has already exited, then
     there is nothing to do: *)
  if OBus_name.is_unique name && has_exited name info then
    Lwt.return (S.const "")
  else begin
    let%lwt resolver, export_switch =
      match try Some(String_map.find name info.resolvers) with Not_found -> None with
        | Some thread ->
            thread
        | None ->
            let waiter, wakener = Lwt.wait () in
            info.resolvers <- String_map.add name waiter info.resolvers;
            let export_switch = Lwt_switch.create () in
            try%lwt
              let%lwt () =
                OBus_match.export
                  ~switch:export_switch
                  connection
                  (OBus_match.rule
                     ~typ:`Signal
                     ~sender:OBus_protocol.bus_name
                     ~interface:OBus_protocol.bus_interface
                     ~member:"NameOwnerChanged"
                     ~path:OBus_protocol.bus_path
                     ~arguments:(OBus_match.make_arguments [(0, OBus_match.AF_string name)]) ())
              in
              let%lwt current_owner = get_name_owner connection name in
              let owner, set_owner = S.create current_owner in
              let resolver = { count = 0; owner; set_owner } in
              Lwt.wakeup wakener (resolver, export_switch);
              Lwt.return (resolver, export_switch)
            with exn ->
              info.resolvers <- String_map.remove name info.resolvers;
              Lwt.wakeup_exn wakener exn;
              let%lwt () = Lwt_switch.turn_off export_switch in
              Lwt.fail exn
    in

    resolver.count <- resolver.count + 1;

    let remove = lazy(
      try%lwt
        resolver.count <- resolver.count - 1;
        if resolver.count = 0 then begin
          (* The resolver is no more used, so we disable it: *)
          info.resolvers <- String_map.remove name info.resolvers;
          Lwt_switch.turn_off export_switch
        end else
          Lwt.return ()
      with exn ->
        let%lwt () = Lwt_log.warning_f ~section ~exn "failed to disable resolver for name %S" name in
        Lwt.fail exn
    ) in

    let owner = S.with_finaliser (finalise remove) resolver.owner in

    let%lwt () =
      Lwt_switch.add_hook_or_exec
        switch
        (fun () ->
           S.stop owner;
           Lazy.force remove)
    in

    Lwt.return owner
  end
