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

class type t = object
  method name : OBus_name.bus option React.signal
  method disable : unit Lwt.t
end

let make connection name =
  match connection#get with
    | Crashed exn ->
        fail exn

    | Running connection ->
        (* If the connection is a peer-to-peer connection, act as if
           they is no owner *)
        if (connection.name = None ||

            (* If it is a unique name and the peer has already exited,
               then there is nothing to do *)
            (OBus_name.is_unique name && OBus_cache.mem connection.exited_peers name)) then
          let name = React.S.const None in
          return (object
                    method name = name
                    method disable = return ()
                  end)
        else
          let name_resolver =
            match NameMap.lookup name connection.name_resolvers with
              | Some name_resolver ->
                  (* add a reference to the resolver *)
                  name_resolver.nr_ref_count <- name_resolver.nr_ref_count + 1;
                  name_resolver

              | None ->
                  let match_rule = OBus_match.rule
                    ~typ:`signal
                    ~sender:"org.freedesktop.DBus"
                    ~interface:"org.freedesktop.DBus"
                    ~member:"NameOwnerChanged"
                    ~path:["org"; "freedesktop"; "DBus"]
                    ~arguments:[(0, name)] () in

                  let w, wakener = Lwt.wait ()
                  and owner, set = React.S.create None in
                  let name_resolver = {
                    nr_owner = owner;
                    nr_set = set;
                    nr_ref_count = 1;
                    nr_match_rule = match_rule;
                    nr_init_done = false;
                    nr_init_waiter = w;
                    nr_init_wakener = wakener;
                  } in

                  (* Immediatly add the resolver to be sure no other
                     thread will do it: *)
                  connection.name_resolvers <- NameMap.add name name_resolver connection.name_resolvers;

                  (* Initialization *)
                  ignore_result
                    (try_lwt
                       (* Add the rule for monitoring the name + ask
                          for the current name owner. The calling
                          order is important to avoid race
                          conditions. *)
                       lwt () = OBus_private_bus.add_match connection.packed match_rule in
                       lwt owner = OBus_private_bus.get_name_owner connection.packed name in
                       name_resolver.nr_set owner;
                       if not name_resolver.nr_init_done then Lwt.wakeup name_resolver.nr_init_wakener ();
                       return ()
                     with exn ->
                       if not name_resolver.nr_init_done then Lwt.wakeup_exn name_resolver.nr_init_wakener exn;
                       return ());

                  name_resolver
          in

          (* Wait for initialization *)
          lwt () = name_resolver.nr_init_waiter in

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

          return (object
                    method name = name_resolver.nr_owner
                    method disable = Lazy.force disable
                  end)
