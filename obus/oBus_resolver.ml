(*
 * oBus_resolver.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_internals
open OBus_type

type desc = {
  resolver_name : OBus_name.bus;
  resolver_connection : OBus_connection.t;
  resolver_name_resolver : name_resolver;
  resolver_on_change : name_change_callback MSet.node option;
}

type state =
  | Enabled of desc
  | Disabled

  (* In case we known there will be never an owner: the connection is
     a peer-to-peer connection or the peer owning a unique name has
     already exited *)
  | Fake

(* A resolver is [{ contents = None }] when disabled and [{ contents =
   Some desc }] otherwise *)
type t = state ref

let disable r = match !r with
  | Enabled { resolver_name = name;
              resolver_connection = connection;
           resolver_name_resolver = nr;
           resolver_on_change = on_change } ->
      r := Disabled;
      begin match on_change with
        | Some node -> MSet.remove node
        | None -> ()
      end;
      nr.nr_ref_count <- nr.nr_ref_count - 1;
      (* If nobody use the name resolver, just remove it *)
      if nr.nr_ref_count = 0 then begin
        connection.name_mapping <- Name_map.remove name connection.name_mapping;
        ignore (Bus.remove_match connection nr.nr_match_rule)
      end

  | Fake ->
      r := Disabled

  | Disabled ->
      ()

let opt_call_resolver_handler ?init ncc_opt owner =
  match ncc_opt with
    | Some ncc -> call_resolver_handler ?init ncc owner
    | None -> ()

let make ?(serial=false) ?on_change connection name =
  lwt_with_running begin fun connection ->

    Log.log "plop=%S" name;

    let ncc_opt = Util.wrap_option on_change (fun f -> { ncc_callback = make_callback serial f;
                                                         ncc_initialized = false }) in

    (* If the connection is a peer-to-peer connection, act as if they
       is no owner *)
    if (not (is_bus connection) ||

          (* If it is a unique name and the peer has already exited,
             then there is nothing to do *)
          (OBus_name.is_unique name && Cache.mem connection.exited_peers name)) then
      begin
        (* Immedialty call the name change handler *)
        opt_call_resolver_handler ~init:true ncc_opt None;

        (* Return a fake resolver *)
        return (ref Fake)
      end

    else

      let nr =
        match Name_map.lookup name connection.name_mapping with
          | Some nr ->
              nr

          | None ->
              let match_rule = Match_rule.make
                ~typ:`signal
                ~sender:"org.freedesktop.DBus"
                ~interface:"org.freedesktop.DBus"
                ~member:"NameOwnerChanged"
                ~path:["org"; "freedesktop"; "DBus"]
                ~args:[(0, name)] () in

              (* Immediatly add the resolver to be sure no other
                 thread will do it *)
              let nr = { nr_owner = ref None;
                         nr_ref_count = 0;
                         nr_match_rule = match_rule;
                         nr_on_change = MSet.make ();
                         nr_init = Lwt.wait ();
                         nr_initialized = false } in
              connection.name_mapping <- Name_map.add name nr connection.name_mapping;

              ignore (catch
                        (fun _ -> perform
                           (* Add the rule for monitoring the name +
                              ask for the current name owner. The calling
                              order is important to avoid race conditions. *)
                           Bus.add_match connection match_rule;
                           owner <-- Bus.get_name_owner connection name;
                           let _ =
                             if not nr.nr_initialized then begin
                               nr.nr_initialized <- true;
                               nr.nr_owner := owner
                             end;
                             Lwt.wakeup nr.nr_init ()
                           in
                           return ())
                        (fun exn ->
                           Lwt.wakeup_exn nr.nr_init exn;
                           return ()));
              nr
      in

      let node_opt = Util.wrap_option ncc_opt (MSet.add nr.nr_on_change) in

      (* Wait for initialization *)
      nr.nr_init >>= begin fun _ ->

        (* Initialze the handler with the initial name resolving *)
        opt_call_resolver_handler ~init:true ncc_opt !(nr.nr_owner);

        let resolver = ref (Enabled { resolver_name = name;
                                      resolver_connection = connection;
                                      resolver_name_resolver = nr;
                                      resolver_on_change = node_opt }) in

        (* There is now one new reference to the name resolver *)
        nr.nr_ref_count <- nr.nr_ref_count + 1;

        (* Disable resolver on garbage collection *)
        Gc.finalise disable resolver;

        return resolver
      end
  end connection

let owner r = match !r with
  | Disabled ->
      invalid_arg "OBus_resolver.owner: disabled resolver"
  | Enabled { resolver_name_resolver = nr } ->
      !(nr.nr_owner)
  | Fake ->
      None

let owned r = match !r with
  | Disabled ->
      invalid_arg "OBus_resolver.owned: disabled resolver"
  | Enabled { resolver_name_resolver = nr } ->
      !(nr.nr_owner) <> None
  | Fake ->
      false

let internal_resolver r = match !r with
  | Enabled { resolver_name_resolver = nr } ->
      nr.nr_owner
  | _ ->
      ref None
