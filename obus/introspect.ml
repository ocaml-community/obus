(*
 * introspect.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

          | Method_call, { interface = Some("org.freedesktop.DBus.Introspectable");
                           member = Some("Introspect");
                           path = Some(path) } ->
              (* Introspection of objects *)
              let buf = Buffer.create 1024 in
                Buffer.add_string buf
                  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"\n\
                   \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">\n\
                  <node>\n";
                let sons = List.fold_left begin fun acc introspecter ->
                  begin try
                    let signature, sons = introspecter path in
                      Interface.print_xml (Printf.bprintf buf) signature;
                      List.fold_left (fun set obj -> ObjectSet.add obj set) acc sons
                  with
                      _ -> acc
                  end ObjectSet.empty (Protected.get bus.introspecters);
                  ObjectSet.iter (Buffer.add_string "  <node name=\"%s\"/>\n") sons;
                  Buffer.add_string "</node>\n"
                  let str = Buffer.contents in
                    bus.transport.Transport.send str 0 (String.length str)
