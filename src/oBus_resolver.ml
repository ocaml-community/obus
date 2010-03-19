(*
 * oBus_resolver.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_private

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
  match connection#get with
    | Crashed exn ->
        fail exn

    | Running connection ->
        (* If the connection is a peer-to-peer connection, act as if
           there is no owner *)
        if (connection.OBus_private.name = None ||
            (* If it is a unique name and the peer has already exited,
               then there is nothing to do *)
            (OBus_name.is_unique name && OBus_cache.mem connection.exited_peers name)) then
          return {
            name = name;
            owner = React.S.const None;
            disable = lazy(return ());
          }
        else
          lwt name_resolver =
            match NameMap.lookup name connection.name_resolvers with
              | Some name_resolver ->
                  (* add a reference to the resolver *)
                  name_resolver.nr_ref_count <- name_resolver.nr_ref_count + 1;
                  begin
                    match name_resolver.nr_state with
                      | Nrs_init(waiter, wakener) ->
                          waiter
                      | Nrs_running ->
                          return name_resolver
                  end;

              | None ->
                  let match_rule = OBus_match.rule
                    ~typ:`Signal
                    ~sender:"org.freedesktop.DBus"
                    ~interface:"org.freedesktop.DBus"
                    ~member:"NameOwnerChanged"
                    ~path:["org"; "freedesktop"; "DBus"]
                    ~arguments:[(0, OBus_match.AF_string name)] () in

                  let init_waiter, init_wakener = Lwt.wait () and owner, set = React.S.create None in
                  let name_resolver = {
                    nr_owner = owner;
                    nr_set = set;
                    nr_ref_count = 1;
                    nr_match_rule = match_rule;
                    nr_state = Nrs_init(init_waiter, init_wakener);
                  } in

                  (* Immediatly add the resolver to be sure no other
                     thread will do it: *)
                  connection.name_resolvers <- NameMap.add name name_resolver connection.name_resolvers;

                  (* Initialization *)
                  try_lwt
                    (* Add the rule for monitoring the name + ask for
                       the current name owner. The calling order is
                       important to avoid race conditions. *)
                    lwt () = OBus_private_bus.add_match connection.packed match_rule in
                    lwt owner = OBus_private_bus.get_name_owner connection.packed name in
                    name_resolver.nr_set owner;
                    match name_resolver.nr_state with
                      | Nrs_init(waiter, wakener) ->
                          name_resolver.nr_state <- Nrs_running;
                          wakeup wakener name_resolver;
                          return name_resolver
                      | Nrs_running ->
                          return name_resolver
                  with exn ->
                    match name_resolver.nr_state with
                      | Nrs_init(waiter, wakener) ->
                          connection.name_resolvers <- NameMap.remove name connection.name_resolvers;
                          wakeup_exn wakener exn;
                          fail exn
                      | Nrs_running ->
                          (* If we go here, this means that a
                             NameOwnerChanged signals have been
                             received by the connection.

                             We consider that the resolver is OK. *)
                          return name_resolver
          in

          let disable = lazy(
            name_resolver.nr_ref_count <- name_resolver.nr_ref_count - 1;
            if name_resolver.nr_ref_count = 0 then
              match connection.packed#get with
                | Running connection ->
                    connection.name_resolvers <- NameMap.remove name connection.name_resolvers;
                    React.S.stop name_resolver.nr_owner;
                    OBus_private_bus.remove_match connection.packed name_resolver.nr_match_rule
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
