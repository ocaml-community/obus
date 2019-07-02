(*
 * network_manager.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This example illustrate the use of OBus to detect network-manager
   connections. *)

open Lwt_react
open Lwt
open Lwt_io
open OBus_value

let () = Lwt_main.run begin
  (* Get the manager. *)
  let%lwt manager = Nm_manager.daemon () in

  (* Create a signal descriptor for listenning on signals comming from
     any DHCP4 object. *)
  let sig_desc =
    OBus_signal.make_any
      Nm_interfaces.Org_freedesktop_NetworkManager_DHCP4Config.s_PropertiesChanged
      (Nm_manager.to_peer manager)
  in

  (* Connects to this signal. *)
  let%lwt event = OBus_signal.connect sig_desc in

  (* Prints all DHCP4 options when one configuration changes. *)
  E.keep
    (E.map_s
       (fun (proxy, properties) ->
          match try Some(List.assoc "Options" properties) with Not_found -> None with
            | Some options ->
                let%lwt () = printlf "DHCP options for %S:" (OBus_path.to_string (OBus_proxy.path proxy)) in
                Lwt_list.iter_s
                  (fun (key, value) ->
                     printlf "  %s = %s" key (V.string_of_single value))
                  (C.cast_single (C.dict C.string C.variant) options)
            | None ->
                return ())
       event);

  fst (wait ())
end
