(*
 * oBus_resolver.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_private_connection

type t = {
  name : OBus_name.bus;
  owner : OBus_name.bus option React.signal;
  disable : unit Lwt.t Lazy.t;
}

let name resolver = resolver.name
let owner resolver = resolver.owner
let disable resolver = Lazy.force resolver.disable

let finalise stop () =
  ignore_result (Lazy.force stop)

let make connection name =
  let running = running_of_connection connection in
  (* If the connection is a peer-to-peer connection, act as if there
     is no owner *)
  if (running.rc_name = None ||
      (* If it is a unique name and the peer has already exited, then
         there is nothing to do *)
      (OBus_name.is_unique name && OBus_cache.mem running.rc_exited_peers name)) then
    return {
      name = name;
      owner = React.S.const None;
      disable = lazy(return ());
    }
  else
    lwt name_resolver =
      match try Some(Name_map.find name running.rc_resolvers) with Not_found -> None with
        | Some name_resolver ->
            (* add a reference to the resolver *)
            name_resolver.nr_ref_count <- name_resolver.nr_ref_count + 1;
            begin
              match name_resolver.nr_state with
                | Resolver_init(waiter, wakener) ->
                    waiter
                | Resolver_running ->
                    return name_resolver
            end;

        | None ->
            let match_rule =
              OBus_match.string_of_rule
                (OBus_match.rule
                   ~typ:`Signal
                   ~sender:"org.freedesktop.DBus"
                   ~interface:"org.freedesktop.DBus"
                   ~member:"NameOwnerChanged"
                   ~path:["org"; "freedesktop"; "DBus"]
                   ~arguments:(OBus_match.make_arguments [(0, OBus_match.AF_string name)]) ()) in

            let init_waiter, init_wakener = Lwt.wait () and owner, set = React.S.create None in
            let name_resolver = {
              nr_owner = owner;
              nr_set = set;
              nr_ref_count = 1;
              nr_match_rule = match_rule;
              nr_state = Resolver_init(init_waiter, init_wakener);
            } in

            (* Immediatly add the resolver to be sure no other thread
               will do it: *)
            running.rc_resolvers <- Name_map.add name name_resolver running.rc_resolvers;

            (* Initialization *)
            try_lwt
              (* Add the rule for monitoring the name + ask for the
                 current name owner. The calling order is important to
                 avoid race conditions. *)
              lwt () = OBus_private_bus.add_match connection match_rule in
              lwt owner = OBus_private_bus.get_name_owner connection name in
              name_resolver.nr_set owner;
              match name_resolver.nr_state with
                | Resolver_init(waiter, wakener) ->
                    name_resolver.nr_state <- Resolver_running;
                    wakeup wakener name_resolver;
                    return name_resolver
                | Resolver_running ->
                    return name_resolver
            with exn ->
              match name_resolver.nr_state with
                | Resolver_init(waiter, wakener) ->
                    running.rc_resolvers <- Name_map.remove name running.rc_resolvers;
                    wakeup_exn wakener exn;
                    fail exn
                | Resolver_running ->
                    (* If we go here, this means that a
                       NameOwnerChanged signals have been received by
                       the connection.

                       We consider that the resolver is OK. *)
                    return name_resolver
    in

    let disable = lazy(
      name_resolver.nr_ref_count <- name_resolver.nr_ref_count - 1;
      if name_resolver.nr_ref_count = 0 then
        match connection#get with
          | Running running ->
              running.rc_resolvers <- Name_map.remove name running.rc_resolvers;
              React.S.stop name_resolver.nr_owner;
              OBus_private_bus.remove_match connection name_resolver.nr_match_rule
          | Crashed _ ->
              return ()
      else
        return ()
    ) in

    return {
      name = name;
      owner = Lwt_signal.with_finaliser (finalise disable) name_resolver.nr_owner;
      disable = disable;
    }
