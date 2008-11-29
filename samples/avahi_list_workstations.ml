(*
 * avahi_list_workstations.ml
 * --------------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Sample using the avahi DBus interfaces. If avahi is running on your
   computer then it will list the computers of your local network
   using zero-conf. *)

open Lwt
open Printf
open Avahi

let main : unit Lwt.t =
  (perform
     avahi <-- Lazy.force server;

     service_browser <-- Server.service_browser_new avahi (-1) (-1) "_workstation._tcp" "" 0;

     OBus_signal.connect service_browser Service_browser.item_new
       (fun (interface, protocol, name, service, domain, flags) ->
          printf "new workstation found:\n  name = %S\n  domain = %S\n\n%!" name domain;

          ignore_result
            (perform
               resolver <-- Server.service_resolver_new avahi interface protocol name service domain (-1) 0;

               OBus_signal.connect resolver Service_resolver.found
                 (fun (interface, protocol, name, typ, domain, host, aprotocol, address, port, txt, flags) ->
                    printf "the resolver found:\n  name = %S\n  host = %S\n  address = %S\n\n%!" name host address);

               OBus_signal.connect resolver Service_resolver.failure
                 (printf "failure of the service resolver: %S\n\n%!")));

     OBus_signal.connect service_browser Service_browser.item_remove
       (fun (interface, protocol, name, service, domain, flags) ->
          printf "workstation removed:  name = %S\n  domain = %S\n\n%!" name domain);

     OBus_signal.connect service_browser Service_browser.failure
       (printf "failure of the service browser: %S\n\n%!");

     let _ = printf "type Ctrl+C to stop\n%!" in
     wait ())

let _ = Lwt_unix.run main
