(*
 * avahi-list-workstations.ml
 * --------------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Sample using the avahi DBus interfaces. If avahi is running on your
   computer then it will list the computers of your local network
   using zero-conf *)

open Printf
open OBus
open Rules
open Avahi

let browser_handler bus server proxy = function
  | ServiceBrowser.Item_new(interface, protocol, name, service, domain, flags) ->
      printf "new workstation found:\n  name = %S\n  domain = %S\n\n%!" name domain;

      Server.service_resolver_new_async server interface protocol name service domain (-1) 0
        (fun resolver ->
           DBus.add_match bus
             [ Type Signal;
               Sender (Interface.name ServiceResolver.interface);
               Path resolver ])

  | ServiceBrowser.Item_remove(interface, protocol, name, service, domain, flags) ->
      printf "workstation removed:  name = %S\n  domain = %S\n\n%!" name domain

  | ServiceBrowser.Failure msg ->
      Printf.printf "failure of the service browser: %S\n\n%!" msg

  | _ -> ()

let resolver_handler proxy = function
  | ServiceResolver.Found(interface, protocol, name, typ, domain, host, aprotocol, address, port, txt, flags) ->
      printf "the resolver found:\n  name = %S\n  host = %S\n  address = %S\n\n%!" name host address

  | ServiceResolver.Failure msg ->
      Printf.printf "failure of the service resolver: %S\n\n%!" msg

let _ =
  let bus = Bus.system () in
  let server = Bus.make_proxy bus Server.interface "org.freedesktop.Avahi" "/" in

  Signal.register bus ServiceBrowser.signals (browser_handler bus server);
  Signal.register bus ServiceResolver.signals resolver_handler;

  Server.service_browser_new_async server (-1) (-1) "_workstation._tcp" "" 0
    (fun browser ->
       DBus.add_match bus
         [ Type Signal;
           Sender (Interface.name ServiceBrowser.interface);
           Path browser ]);

  while true do
    Connection.dispatch bus
  done
